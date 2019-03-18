package polyite.util

import java.io.BufferedReader
import java.io.File
import java.math.BigInteger
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.logging.Logger

import scala.BigDecimal
import scala.BigInt
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.forkjoin.ForkJoinPool
import scala.math.BigInt.int2bigInt
import scala.util.Random

import org.exastencils.schedopt.chernikova.Chernikova
import org.exastencils.schedopt.chernikova.Generators

import isl.Conversions.convertBigIntegerToBigInt
import isl.Conversions.convertLambdaToVoidCallback1
import isl.Conversions.convertLambdaToVoidCallback2
import isl.Conversions.convertValToBigInt
import isl.Isl.TypeAliases.T_IN
import isl.Isl.TypeAliases.T_OUT
import isl.Isl.TypeAliases.T_PAR
import isl.Isl.TypeAliases.T_SET

/**
  * Collection of general utility functions.
  */
object Util {
  private val myLogger : Logger = Logger.getLogger("")

  /**
    * Calculate the gcd of x and y.
    */
  def gcd(x : Int, y : Int) : Int = {
    var a : Int = x
    var b : Int = y
    while (a != 0) {
      val h = b % a
      b = a
      a = h
    }
    return math.abs(b)
  }

  /**
    * Calculate the gcd of x and y.
    */
  def gcd(x : BigInt, y : BigInt) : BigInt = {
    var a : BigInt = x
    var b : BigInt = y
    while (a != 0) {
      val h = b % a
      b = a
      a = h
    }
    return b.abs
  }

  /**
    * Calculate the gcd of x and y.
    */
  def gcd(x : Long, y : Long) : Long = {
    var a : Long = x
    var b : Long = y
    while (a != 0) {
      val h = b % a
      b = a
      a = h
    }
    return math.abs(b)
  }

  /**
    * Compute the GCD of the given values.
    */
  def gcd(v : Iterable[Int]) : Int = v.foldLeft(0)(Util.gcd)

  /**
    * Compute the GCD of the given values.
    */
  def gcd(v : Iterable[BigInt]) : BigInt = v.foldLeft(BigInt(0))(Util.gcd)

  /**
    * Verify that the GCD of the given values is not 1.
    */
  def checkGcdNotOne(v : Iterable[Int]) : Boolean = gcd(v) != 1

  /**
    * Maps a function A => B on an Iterable a of elements of type A. The first
    * parameter of f is the index of the thread that executes f for a specific
    * element of the array.
    * @param a the original collection
    * @param f a function of type Int => A => B
    * @param parallelismLevel the number of threads
    * @return an Iterable of elements of type B
    */
  def mapInParallel[A, B](a : Iterable[A], f : Int => A => B,
    parallelismLevel : Int) : Iterable[B] = {
    if (parallelismLevel < 1)
      throw new IllegalArgumentException("the parallelism level must be positive.")
    val inputWithIndex : ConcurrentLinkedQueue[(A, Int)] = new ConcurrentLinkedQueue
    a.zipWithIndex.map((t : (A, Int)) => inputWithIndex.offer(t))
    val outputWithIndex : ConcurrentLinkedQueue[(B, Int)] = new ConcurrentLinkedQueue

    def worker(wid : Int)(u : Unit) : Unit = {
      while (!inputWithIndex.isEmpty()) {
        val tMaybe : Option[(A, Int)] = inputWithIndex.synchronized(
          if (inputWithIndex.isEmpty)
            None
          else
            Some(inputWithIndex.poll))
        if (tMaybe.isDefined) {
          val (a, i : Int) = tMaybe.get
          outputWithIndex.offer((f(wid)(a), i))
        }
      }
    }
    val workers : ParArray[Unit => Unit] = new ParArray(parallelismLevel)
    for (i <- 0 until parallelismLevel)
      workers(i) = worker(i)
    workers.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(parallelismLevel))
    workers.map(f => f(()))
    var resultList : List[(B, Int)] = List.empty
    while (!outputWithIndex.isEmpty)
      resultList = outputWithIndex.poll :: resultList
    resultList.sortBy((t : (B, Int)) => t._2).map((t : (B, Int)) => t._1)
  }

  // time limit is in milliseconds
  /**
    * Synchronously runs a given function {@code f} with a time limit. A call to
    * {@code runWithTimeout} blocks until the time limit has been reached or {@code f}
    * has returned. If {@code f} doesn't return on time the thread that it runs
    * on is interrupted. Any exception thrown by {@code f} (except for
    * {@code InterruptedException}) are thrown along.
    *
    * @param a argument for f
    * @param f function to execute
    * @param timeLimit time limit in milliseconds. 0 means to wait forever or until
    *        {@code f} returns by itself.
    * @return Returns {@code Some(v)} if {@code f} returned v. Otherwise returns
    * 				 {@code None}.
    *
    */
  def runWithTimeout[A, B](a : A, f : A => B, timeLimit : Long) : Option[B] = {
    var result : Option[B] = None
    var ex : Throwable = null
    val resultLock = new Object
    val t : Thread = new Thread() {
      override def run() {
        try {
          val res : B = f(a)
          resultLock.synchronized {
            result = Some(res)
          }
        } catch {
          case e : InterruptedException => {
            // ignore
          }
          case t : Throwable => {
            resultLock.synchronized {
              ex = t
            }
          }
        }
      }
    }
    t.start()
    try {
      t.join(timeLimit)
    } catch {
      case e : InterruptedException => {
        val er : RuntimeException = new RuntimeException(
          "Interrupted while waiting for task to finish.")
        t.interrupt()
        er.initCause(e)
        throw er
      }
    }
    if (t.isAlive) {
      t.interrupt()
      t.join()
    }

    resultLock.synchronized {
      // If an exception has been thrown on Thread t, throw it along.
      if (ex != null)
        throw ex
      return result
    }
  }

  /**
    * Uniformly distributed BigInt from the interval [0, n[.
    */
  def getNextRandomBigInt(n : BigInt) : BigInt = {
    val coeff : Double = Random.nextDouble()
    val randVal = (BigDecimal(n) * BigDecimal(coeff)).toBigInt()
    if (randVal > n)
      return n - 1
    else
      return randVal
  }

  /**
    * Check whether a given file {@code f} exists, fulfills the given constraints
    * and has at least the given permissions. Logs an error message if {@code f}
    * does not fulfill the requirements.
    */
  def checkFileExistsAndHasRequiredPermissions(r : Boolean, w : Boolean,
    x : Boolean, isDir : Boolean, f : File) : Boolean = {
    if (!f.exists()) {
      myLogger.warning(f.getPath + " does not exist.")
      return false
    }
    if (isDir && !f.isDirectory()) {
      myLogger.warning(f.getPath() + " is not a directory.")
      return false
    } else if (!isDir && f.isDirectory()) {
      myLogger.warning(f.getPath + " must be a normal file.")
      return false
    }
    if (!((!x || f.canExecute()) && (!r || f.canRead()) && (!w || f.canWrite()))) {
      myLogger.warning(f.getPath + " does not have the right permissions("
        + (if (r) "r" else "-")
        + (if (w) "w" else "-")
        + (if (x) "x" else "-")
        + "): "
        + (if (f.canRead) "r" else "-")
        + (if (f.canWrite) "w" else "-")
        + (if (f.canExecute) "x" else "-"))
      return false
    }
    return true
  }

  /**
    * Checks whether v >= min is true. Logs a warning if this is not true.
    *
    * @param prop parameter name for v - required for logging.
    */
  def checkMin(min : Int, v : Int, prop : String) : Boolean = {
    val isGeq : Boolean = v >= min
    if (!isGeq)
      myLogger.warning(prop + " must be >= " + min + ": " + v)
    return isGeq
  }

  /**
    * Checks whether v >= min is true. Logs a warning if this is not true.
    *
    * @param prop parameter name for v - required for logging.
    */
  def checkMin(min : Long, v : Long, prop : String) : Boolean = {
    val isGeq : Boolean = v >= min
    if (!isGeq)
      myLogger.warning(prop + " must be >= " + min + ": " + v)
    return isGeq
  }

  /**
    * Checks whether v >= min is true. Logs a warning if this is not true.
    *
    * @param prop parameter name for v - required for logging.
    */
  def checkMin(min : Double, v : Double, prop : String) : Boolean = {
    val isGeq : Boolean = v >= min
    if (!isGeq)
      myLogger.warning(prop + " must be >= " + min + ": " + v)
    return isGeq
  }

  /**
    * Checks whether min <= v <= max is true. Logs a warning if this is not
    * true.
    *
    * @param prop parameter name for v - required for logging.
    */
  def checkMinMax(min : Double, max : Double, v : Double,
    prop : String) : Boolean = {
    val withinRange : Boolean = v >= min && v <= max
    if (!withinRange)
      myLogger.warning(prop + " must be from the range [" + min + ", " + max + "]: " + v)
    return withinRange
  }

  /**
    * Checks whether min <= v <= max is true. Logs a warning if this is not
    * true.
    *
    * @param prop parameter name for v - required for logging.
    */
  def checkMinMax(min : Long, max : Long, v : Long,
    prop : String) : Boolean = {
    val withinRange : Boolean = v >= min && v <= max
    if (!withinRange)
      myLogger.warning(prop + " must be from the range [" + min + ", " + max + "]: " + v)
    return withinRange
  }

  /**
    * Checks whether min <= v <= max is true. Logs a warning if this is not
    * true.
    *
    * @param prop parameter name for v - required for logging.
    */
  def checkMinMax(min : Int, max : Int, v : Int,
    prop : String) : Boolean = {
    val withinRange : Boolean = v >= min && v <= max
    if (!withinRange)
      myLogger.warning(prop + " must be from the range [" + min + ", " + max + "]: " + v)
    return withinRange
  }

  /**
    * Calculate the median of the given values.
    */
  def median(values : Iterable[Double]) : Double = {
    if (values.isEmpty)
      throw new IllegalArgumentException("values must not be empty")

    val n = values.size
    val valuesOrdered = values.toList.sorted

    if (n % 2 == 0)
      return (valuesOrdered(n / 2 - 1) + valuesOrdered(n / 2)).toDouble / 2
    else
      return valuesOrdered(n / 2)
  }

  /**
    * Calculates the arithmetic mean of the given values.
    */
  def mean(values : Iterable[Double]) : Double = {
    if (values.isEmpty)
      throw new IllegalArgumentException("values must not be empty")

    val n = values.size
    return values.sum / n
  }

  /**
    * Calculate quartile boundaries (remember box plots) for the given values.
    */
  def getQuartileBoundaries(values : List[Double]) : (Double, Double, Double) = {
    val sorted : Array[Double] = values.sorted.toArray
    return (sorted(sorted.length / 4), sorted(sorted.length / 2), sorted(sorted.length / 4 * 3))
  }

  /**
    * Reads everything from a given buffered reader line by line. Concatenates
    * the lines to a {@code String} separated by line breaks.
    */
  def readAll(r : BufferedReader) : String = {
    val sb : StringBuilder = new StringBuilder()
    var line = r.readLine()
    while (line != null) {
      sb.append(line)
      sb.append('\n')
      line = r.readLine()
    }
    return sb.toString()
  }

  def vectorToArray(vector : Vector[BigInt]) : Array[Int] = {
    val point : Array[Int] = new Array[Int](vector.length)
    for ((bI, i) <- vector.view.zipWithIndex)
      point(i) = bI.intValue()
    return point
  }

  private def maybeCreateArray(a : Array[Int], len : Int) : (Array[Int], Int) = {
    if (a == null)
      return (new Array[Int](len), len)
    else
      return (a, math.min(len, a.length))
  }

  def addArrayPW(a1 : Array[Int], a2 : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, math.min(a1.length, a2.length))

    var i : Int = 0
    while (i < length) {
      res(i) = a1(i) + a2(i)
      i += 1
    }

    return res
  }

  def subArrayPW(a1 : Array[Int], a2 : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, math.min(a1.length, a2.length))

    var i : Int = 0
    while (i < length) {
      res(i) = a1(i) - a2(i)
      i += 1
    }

    return res
  }

  def negateArrayPW(in : Array[Int], out : Array[Int]) : Array[Int] = {
    return mulArrayPW(-1, in, out)
  }

  def mulArrayPW(c : Int, in : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, in.length)

    var i : Int = 0
    while (i < length) {
      res(i) = c * in(i)
      i += 1
    }

    return res
  }

  def divEArrayPW(in : Array[Int], c : Int, out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, in.length)

    var i : Int = 0
    while (i < length) {
      if (in(i) % c != 0)
        return null // not exactly dividable... no result
      res(i) = in(i) / c
      i += 1
    }

    return res
  }

  def fstNZero(a : Array[Int], n : Int) : Boolean = {
    var i : Int = 0
    while (i < n) {
      if (a(i) != 0)
        return false
      i += 1
    }
    return true
  }

  def reduceByGCD(in : Array[Int], out : Array[Int]) : Array[Int] = {
    val (res : Array[Int], length : Int) = maybeCreateArray(out, in.length)

    // find gcd first
    var gcd : Int = in(0)
    var i : Int = 1
    while (i < length) {
      gcd = this.gcd(gcd, in(i))
      i += 1
    }

    // divide everything if gcd is not 1 (or 0)
    if (gcd != 1 && gcd != 0) {
      i = 0
      while (i < length) {
        res(i) = in(i) / gcd
        i += 1
      }
    }

    return res
  }

  /**
    * Adds a value to a given matrix that is given as an {@code ArrayBuffer} of
    * {@code Array[BigInt]} instances in row major order. The columns have a
    * rows width {@code nCols}. If a row index doesn't exist, rows up to the
    * requested index are added to the matrix.
    *
    * @throws IndexOutOfBoundsException thrown if {@code col >= nCols} or
    * {@code col < 0}.
    * @throws IllegalArgumentException thrown if {@code row < 0}.
    */
  def addValToMatrix(row : Int, col : Int, nCols : Int,
    matrix : ArrayBuffer[Array[BigInt]], v : BigInt) {
    if (row < 0)
      throw new IllegalArgumentException("row must not be < 0: " + row)
    if (col < 0)
      throw new IndexOutOfBoundsException("col must not be < 0: " + col)
    if (col >= nCols)
      throw new IndexOutOfBoundsException("col must not be >= nCols: " + nCols)

    while (row >= matrix.size) {
      val newRow : Array[BigInt] = Array.fill(nCols) { BigInt(0) }
      matrix.append(newRow)
    }
    matrix(row)(col) = v
  }

  implicit def vec2List[T](v : Vector[T]) : List[T] = v.toList

  /**
    * Converts an {@code isl.Point} into a list of rational numbers.
    */
  def islPoint2RatList(p : isl.Point) : List[Rat] = {
    var result : List[Rat] = List.empty
    for (i <- 0 until p.dim(T_SET))
      result ::= Rat(p.getCoordinateVal(T_SET, i).getNum, p.getCoordinateVal(T_SET, i).getDen)
    return result.reverse
  }

  /**
    * Takes a map {@code parentMap} from keys of type {@code K1} to maps from keys of type {@code K2} to values of type
    * {@code V} and a key {@code k} of type {@code K1}. If {@code parentMap} contains a map for {@code k}, this map is
    * returned. Otherwise a new empty map {@code K2 -> V} is inserted into {@code parentMap} and is returned.
    */
  def getOrCreateSubMap[K1, K2, V](
    parentMap : HashMap[K1, HashMap[K2, V]],
    k : K1) : HashMap[K2, V] = {
    if (parentMap.contains(k))
      return parentMap(k)
    else {
      val tmp : HashMap[K2, V] = HashMap.empty
      parentMap.put(k, tmp)
      return tmp
    }
  }

  /**
    * Divide the coefficients of {@code m} by their GCD. The range of {@code m} must be one-dimensional. All coefficients
    * {@code m} must be integers.
    */
  def divideCoeffsByGcd(m : isl.UnionMap) : isl.UnionMap = {
    val gcd : BigInt = islUnionMapCoeffsGCD(m)
    return divideCoeffsByVal(m, gcd)
  }

  /**
    * Divide the coefficients of {@code m} by their GCD. The range of {@code m} must be one-dimensional. All coefficients
    * {@code m} must be integers.
    */
  def divideCoeffsByGcd(m : isl.Map) : isl.Map = {
    val gcd : BigInt = islMapCoeffsGCD(m)
    return divideCoeffsByVal(m, gcd)
  }

  /**
    * Divide the coefficients of {@code m} by {@code v}. The range of {@code m} must be one-dimensional. All coefficients
    * {@code m} must be integers. The divisions are integer divisions.
    */
  def divideCoeffsByVal(m : isl.UnionMap, v : BigInt) : isl.UnionMap = {
    var res : isl.UnionMap = isl.UnionMap.empty(m.getSpace)
    m.foreachMap((m : isl.Map) => res = res.addMap(divideCoeffsByVal(m, v)))
    return res
  }

  /**
    * Divide the coefficients of {@code m} by {@code v}. The range of {@code m} must be one-dimensional. All coefficients
    * {@code m} must be integers. The divisions are integer divisions.
    */
  def divideCoeffsByVal(m : isl.Map, v : BigInt) : isl.Map = {
    val pwMAff : isl.PwMultiAff = isl.PwMultiAff.fromMap(m)
    var resAff : isl.Aff = null
    var nPiece : Int = 0
    pwMAff.foreachPiece((_ : isl.Set, mAff : isl.MultiAff) => {
      nPiece += 1
      if (nPiece > 1)
        throw new IllegalArgumentException("Cannot process union maps that result in a piecewise affine expression.")
      if (mAff.dim(T_OUT) != 1)
        throw new IllegalArgumentException("The range of m is not one-dimensional: " + m)
      val aff : isl.Aff = mAff.getAff(0)
      resAff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(aff.getDomainSpace))

      def divideCoeffs(t : isl.DimType) {
        for (i <- 0 until aff.dim(t)) {
          val c : isl.Val = aff.getCoefficientVal(t, i)
          checkIsInteger(c)
          resAff = resAff.setCoefficientVal(t, i, isl.Val.fromBigInteger(m.getCtx, c.getNum.divide(v.bigInteger)))
        }
      }
      divideCoeffs(T_PAR)
      divideCoeffs(T_IN)
      val c : isl.Val = aff.getConstantVal
      checkIsInteger(c)
      resAff = resAff.setConstantVal(isl.Val.fromBigInteger(m.getCtx, c.getNum.divide(v.bigInteger)))
    })
    return isl.Map.fromAff(resAff)
  }

  /**
    * Calculates the GCD of the coefficients of {@code m}. All coefficients must be integers. The range must be
    * one-dimensional.
    * @throws IllegalArgumentException thrown if {@code m} does not meat the specified criteria.
    */
  def islUnionMapCoeffsGCD(m : isl.UnionMap) : BigInt = {
    var perMapGCDs : List[BigInt] = List.empty
    m.foreachMap((mm : isl.Map) => {
      perMapGCDs ::= islMapCoeffsGCD(mm)
    })
    return gcd(perMapGCDs)
  }

  private def checkIsInteger(c : isl.Val) {
    if (c.getDen.intValue() != 1)
      throw new IllegalArgumentException("m has a non-integer coefficient: " + c)
  }

  /**
    * Calculates the GCD of the coefficients of {@code m}. All coefficients must be integers. The range must be
    * one-dimensional.
    * @throws IllegalArgumentException thrown if {@code m} does not meat the specified criteria.
    */
  def islMapCoeffsGCD(m : isl.Map) : BigInt = {
    var coeffs : List[BigInt] = List.empty
    val pwMAff : isl.PwMultiAff = isl.PwMultiAff.fromMap(m)
    pwMAff.foreachPiece((_ : isl.Set, mAff : isl.MultiAff) => {
      val nOutDim : Int = mAff.dim(T_OUT)
      if (nOutDim != 1)
        throw new IllegalArgumentException("The range of m ist not one-dimensional: " + nOutDim)
      for (i <- 0 until nOutDim) {
        val aff : isl.Aff = mAff.getAff(i)

        def extractCoeffs(t : isl.DimType) {
          for (j <- 0 until aff.dim(t)) {
            val c : isl.Val = aff.getCoefficientVal(t, j)
            checkIsInteger(c)
            coeffs ::= c
          }
        }
        extractCoeffs(T_PAR)
        extractCoeffs(T_IN)
        val c : isl.Val = aff.getConstantVal
        checkIsInteger(c)
        coeffs ::= c
      }
    })
    return gcd(coeffs)
  }

  /**
    * Calculates the unbiased sample variance of the given values.
    */
  def variance(values : Iterable[Double]) : Double = {
    val theMean : Double = mean(values)
    return values.toList.map(x => math.pow(x - theMean, 2)).sum * ((1 : Double) / (values.size - 1))
  }

  /**
    * Calculates the unbiased sample standard deviation of the given values.
    */
  def standardDeviation(values : Iterable[Double]) : Double = math.sqrt(variance(values))

  /**
    * Calculates the standard error of the given values (assuming that the values come from a normally distributed population)
    */
  def standardError(values : Iterable[Double]) : Double = standardDeviation(values) / math.sqrt(values.size)

  /**
    * Calculates the relative standard error of the given values (assuming that the values come from a normally distributed population)
    */
  def relativeStandardError(values : Iterable[Double]) : Double = (standardDeviation(values) / math.sqrt(values.size)) / mean(values)

  /**
    * Calculates the variance coefficient of the given values.
    */
  def varianceCoeff(values : Iterable[Double]) : Double = standardDeviation(values) / mean(values)

  /**
    * This function chooses uniformly from the given options.
    *
    * The number of options is the length of the List. The elements
    * in the List containing propabilities as {@code Rat}.
    *
    * @return uniform random List index
    */
  def selectRandom[A](options : List[(Rat, A)]) : A = {
    assert(options.foldLeft(Rat(0))((a : Rat, b : (Rat, A)) => a + b._1) == Rat(1), options.foldLeft(Rat(0))((a : Rat, b : (Rat, A)) => a + b._1))
    var gcV : BigInt = options.map { x => x._1.denominator }.fold(BigInt(1))((x, y) => x * y)
    val random = Util.getNextRandomBigInt(gcV)
    val it = options.iterator
    var sum = BigInt(0)
    while (it.hasNext) {
      val next = it.next()
      val v : BigInt = next._1.numerator * (gcV / next._1.denominator)
      sum = sum + v
      if (random < sum) {
        return next._2
      }
    }
    throw new Exception("Something terribly went wrong. Reboot your mind! Tried to select random number between 0 and "
      + gcV + ", just selected " + random + ". Options were " + options.toString())
  }

  /**
    * Calculates the row-echelon form of a given matrix
    * @param m matrix in row major order
    * @param nRows m's number of rows
    * @param nCols m's number of columns
    */
  def calcRowEchelonForm(m : Array[Array[Rat]], nRows : Int, nCols : Int) : Array[Array[Rat]] = {
    if (m.size != nRows)
      throw new IllegalArgumentException(f"m is not a matrix with ${nRows} rows.")
    if ((0 until nRows).exists((i : Int) => m(i).size != nCols))
      throw new IllegalArgumentException(f"m is not a rectangular array with ${nCols} elements per rows.")
    val res : Array[Array[Rat]] = new Array(nRows)
    for (i <- 0 until nRows) {
      res(i) = new Array(nCols)
      for (j <- 0 until nCols)
        res(i)(j) = m(i)(j)
    }

    var h = 0
    var k = 0

    def argmax(f : Int => Rat, domain : List[Int]) : Int = {
      return (domain.foldLeft((domain.head, Rat(Int.MinValue)))((t : (Int, Rat), i : Int) => {
        if (f(i) > t._2)
          (i, f(i))
        else
          t
      }))._1
    }

    while (h < nRows && k < nCols) {
      val iMax = argmax(res(_)(k).abs, (h until nRows).toList)
      if (res(iMax)(k) == Rat(0))
        k += 1
      else {
        val tmp : Array[Rat] = res(h)
        res(h) = res(iMax)
        res(iMax) = tmp
        for (i <- h + 1 until nRows) {
          val f = res(i)(k) / res(h)(k)
          res(i)(k) = Rat(0)
          for (j <- k + 1 until nCols)
            res(i)(j) = res(i)(j) - res(h)(j) * f
        }
        h = h + 1
        k = k + 1
      }
    }
    return res
  }

  /**
    * Calculate the row rank of the given matrix in row-echelon form.
    */
  def calcRowRank(m : Array[Array[Rat]]) : Int = {
    m.count(!_.forall(_ == Rat(0)))
  }

  def main(args : Array[String]) : Unit = {
    //    val ctx : isl.Ctx = Isl.ctx
    //    val m : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S[i, j] -> [2*n + 4*j]; T[i] -> [8*i + 6*m + 12] }")
    //    println(m)
    //    println(divideCoeffsByGcd(m))
    println(median(List(5, 1)))
  }
}
