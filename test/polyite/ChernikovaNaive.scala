package polyite

import scala.collection.mutable.HashSet

object ChernikovaNaive {

  def main(args : Array[String]) {
    val Y : List[Array[Int]] = List(
      Array(-1, 1, 0, 1, 0, 0),
      Array(1, 0, 1, 0, 1, 0),
      Array(0, 0, -2, 0, 0, 1))
    val lStart : Int = 3
    var res : List[Array[Int]] = chernikova(Y, lStart - 1)
    println("extremal rays:")
    for (col : Array[Int] <- res) {
      println(col.drop(lStart).mkString(", "))
    }
  }

  def printMatrix(name : String, Y : List[Array[Int]], uLast : Int) {
    println(f"${name}:")
    for (i <- 0 to uLast) {
      println(Y.map(_(i)).mkString("\t"))
    }
    println(Y.map(_ => "--").mkString(""))
    for (i <- uLast + 1 until Y(0).length) {
      println(Y.map(_(i)).mkString("\t"))
    }
  }

  def chernikova(Y : List[Array[Int]], uLast : Int) : List[Array[Int]] = {
    var YCurr : List[Array[Int]] = Y
    printMatrix("Y", Y, uLast)

    while (!checkAllNonNeg(YCurr, uLast) && !checkAnyRowAllNegativ(YCurr, uLast)) {

      // 1
      val r : Int = getFirstNegOfU(YCurr, uLast)
      println("r = " + (r + 1))

      // 2
      val R : List[Int] = (for ((col : Array[Int], j : Int) <- YCurr.zipWithIndex if (col(r) >= 0)) yield j).toList
      println("R = " + R.map(_ + 1).mkString("{ ", ", ", " }"))

      var YNew : List[Array[Int]] = R.map(YCurr(_))
      printMatrix("Y_new", YNew, uLast)

      // 2'
      var skip3 : Boolean = false
      if (YCurr.size == 2) {
        val yr1 : Int = YCurr(r)(0)
        val yr2 : Int = YCurr(r)(1)
        if (yr1 * yr2 < 0) {
          val fstSummand : Array[Int] = YCurr(0).map(_ * math.abs(yr2))
          val sndSummand : Array[Int] = YCurr(1).map(_ * math.abs(yr1))
          val newCol : Array[Int] = vectAdd(fstSummand, sndSummand)
          println("Adjoining Y[][1] * |Y[r][2]| + Y[][2] * |Y[r][1]| = " + newCol.mkString("<", ", ", ">"))
          YNew :+= newCol
        }
        skip3 = true
      }

      // 3
      if (!skip3) {
        val S : Set[(Int, Int)] = (for (i <- 0 until YCurr.length; j <- i + 1 until YCurr.length if (YCurr(i)(r) * YCurr(j)(r) < 0)) yield (i, j)).toSet
        println("S = " + S.map(t => f"(${t._1 + 1}, ${t._2 + 1})").mkString("{ ", ", ", " }"))
        val I0 : Set[Int] = getNonNegRows(YCurr)
        for ((s : Int, t : Int) <- S) {
          println(f"Inspecting (s, t) = (${s + 1}, ${t + 1})")
          val I1 : Set[Int] = I0.filter((i : Int) => YCurr(s)(i) == 0 && YCurr(t)(i) == 0)
          println("I1 = " + I1.map(_ + 1).mkString("{ ", ", ", " }"))
          // 3b
          if (!I1.isEmpty) {
            if (!(0 until YCurr.length).exists((u : Int) => u != s && u != t && I1.forall(YCurr(u)(_) == 0))) {
              val alpha1 = math.abs(YCurr(t)(r))
              println(f"alpha1 = ${alpha1}")
              val alpha2 = math.abs(YCurr(s)(r))
              println(f"alpha2 = ${alpha2}")
              val fstSummand : Array[Int] = YCurr(s).map(_ * alpha1)
              val sndSummand : Array[Int] = YCurr(t).map(_ * alpha2)
              val newCol : Array[Int] = vectAdd(fstSummand, sndSummand)
              println("Adjoining Y[][s] * alpha1 + Y[][t] * alpha2 = " + newCol.mkString("<", ", ", ">"))
              YNew :+= newCol
            } else {
              println(f"No new column from (${s + 1}, ${t + 1})")
            }
          }
        }
      }

      // 4
      YCurr = YNew
      printMatrix("Y", YCurr, uLast)
    }
    return YCurr
  }

  def vectAdd(v1 : Array[Int], v2 : Array[Int]) : Array[Int] = v1.zip(v2).map((t : (Int, Int)) => t._1 + t._2)

  def getNonNegRows(Y : List[Array[Int]]) : Set[Int] = {
    var result : HashSet[Int] = HashSet.empty
    if (!Y.isEmpty) {
      for (i <- 0 until Y(0).length) {
        if ((0 until Y.length).forall(Y(_)(i) >= 0)) {
          result.add(i)
        }
      }
    }
    return result.toSet
  }

  def getFirstNegOfU(Y : List[Array[Int]], uLast : Int) : Int = {
    for (i <- 0 to uLast) {
      for (col : Array[Int] <- Y) {
        if (col(i) < 0)
          return i
      }
    }
    return -1
  }

  def checkAllNonNeg(Y : List[Array[Int]], uLast : Int) : Boolean = {
    for (col : Array[Int] <- Y) {
      for (i <- 0 to uLast) {
        if (col(i) < 0)
          return false
      }
    }
    return true
  }

  def checkAnyRowAllNegativ(Y : List[Array[Int]], uLast : Int) : Boolean = {
    for (i <- 0 to uLast) {
      if ((0 until Y.length).forall(Y(_)(i) < 0))
        return true
    }
    return false
  }
}