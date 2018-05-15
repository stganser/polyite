package polyite.schedule

import scala.BigInt
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.IndexedSeqView
import scala.math.BigInt.int2bigInt
import isl.Isl
import isl.Isl.TypeAliases._
import isl.VoidCallback1
import isl.Conversions._
import polyite.util.Rat
import polyite.util.Util
import polyite.config.ConfigGA
import polyite.export.JSCOPInterface
import polyite.export.ScheduleExport
import polyite.sched_eval.EvalResult
import java.io.File

/**
  * Mutable representation of a polyhedral schedule. Different representations
  * are stored alongside, while the main representation is through a matrix of
  * rational schedule coefficients. Dimensions of this matrix may be added and
  * removed dynamically. A schedule may be transformed into an Isl union map with
  * integer coefficients.
  *
  * For the purpose of future minimalist mutation the linear combination of
  * Chernikova generators that formed the coefficient vector of each schedule
  * dimension is stored, too.
  *
  * The dimensions of a schedule are numbered. The outermost dimension akka the
  * first row of the schedule coefficient matrix is dimension 0.
  */

object Schedule {

  /**
    * Filters {@code deps} for dependencies that are satisfied strongly by the
    * schedule given through the coefficient matrix {@code coeffs}.
    */
  private def getDepsStronglySatisfiedBySchedule(coeffs : List[BigInt],
    deps : Set[Dependence], domInfo : DomainCoeffInfo) : Set[Dependence] = {
    deps.filter { d : Dependence =>
      //Util.islSetContains(d.strongConstr, coeffs.toArray)
      val schedMap : isl.UnionMap = ScheduleUtils.coeffMatrix2IslUnionMap(domInfo, coeffs)
      ScheduleUtils.getDirectionOfDep(schedMap, d).isPositiveOnly
    }
  }

  /**
    * Filters {@code deps} for dependencies that are satisfied weakly by the
    * schedule given through the coefficient matrix {@code coeffs}.
    */
  private def getDepsWeaklySatisfiedBySchedule(coeffs : List[BigInt],
    deps : Set[Dependence], domInfo : DomainCoeffInfo) : Set[Dependence] = {
    deps.filter { d : Dependence =>
      //Util.islSetContains(d.weakConstr, coeffs.toArray)
      val schedMap : isl.UnionMap = ScheduleUtils.coeffMatrix2IslUnionMap(domInfo, coeffs)
      !ScheduleUtils.getDirectionOfDep(schedMap, d).isNegative
    }
  }

  def computeLinDepSpaceInt(domInfo : DomainCoeffInfo, vectors : Iterable[Array[Int]], start : Int, nr : Int) : isl.BasicSet = {
    def int2Val = (i : Int) => isl.Val.intFromSi(domInfo.ctx, i)
    return computeLinDepSpace(domInfo, vectors, start, nr, int2Val)
  }

  def computeLinDepSpaceBigInt(domInfo : DomainCoeffInfo, vectors : Iterable[Array[BigInt]], start : Int, nr : Int) : isl.BasicSet = {
    def bigint2Val = (i : BigInt) => isl.Val.fromBigInteger(domInfo.ctx, i.bigInteger)
    return computeLinDepSpace(domInfo, vectors, start, nr, bigint2Val)
  }

  private def computeLinDepSpace[T](domInfo : DomainCoeffInfo, vectors : Iterable[Array[T]], start : Int, nr : Int, t2Val : T => isl.Val) : isl.BasicSet = {
    if (vectors.isEmpty)
      return null

    val ctx = domInfo.ctx
    val nrPoints : Int = vectors.size
    val coeffsWithIndex : Iterable[(Array[T], Int)] = vectors.view.zipWithIndex
    //implicit val int2Val = (i : Int) => isl.Val.intFromSi(ctx, i)

    var mAff = isl.MultiAff.zero(isl.Space.alloc(ctx, 0, nrPoints, nr))
    for (pos <- 0 until nr) {
      var aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(isl.Space.setAlloc(ctx, 0, nrPoints)))
      for ((cs, i) <- coeffsWithIndex)
        aff = aff.setCoefficientVal(T_IN, i, t2Val(cs(start + pos)))
      mAff = mAff.setAff(pos, aff)
    }
    var linDepSpace : isl.BasicSet = isl.BasicMap.fromMultiAff(mAff).range()
    // remove existentially qualified variables in linDepSace (with removeDivs)
    //   because there could be holes in the space... [1 0 | 0 2] does not span the full 2D space
    linDepSpace = linDepSpace.removeDivs()

    // expand to full dimensionality
    return linDepSpace.insertDims(T_SET, 0, start).addDims(T_SET, domInfo.dim - (start + nr))
  }

  def computeLinIndepSpaceInt(domInfo : DomainCoeffInfo, vectors : Iterable[Array[Int]], start : Int, nr : Int) : isl.Set = {
    if (vectors.isEmpty)
      return null
    return computeLinDepSpaceInt(domInfo, vectors, start, nr).complement()
  }

  def computeLinIndepSpaceBigInt(domInfo : DomainCoeffInfo, vectors : Iterable[Array[BigInt]], start : Int, nr : Int) : isl.Set = {
    if (vectors.isEmpty)
      return null
    return computeLinDepSpaceBigInt(domInfo, vectors, start, nr).complement()
  }

  def main(args : Array[String]) : Unit = {
    println(ScheduleVectorUtils.multiplyWithCommonDenominator(List(
      Rat(0), Rat(0), Rat(2, 3), Rat(0), Rat(1), Rat(0), Rat(0), Rat(-2, 3),
      Rat(0), Rat(0), Rat(0), Rat(-1), Rat(-2, 3), Rat(0), Rat(0), Rat(-2, 3),
      Rat(0))))
  }
}

/**
  * @param domInfo Iteration domain and mapping of schedule coefficients to
  * parameters, constants and iteration variables of the iteration domain.
  *
  * @param deps Set of data dependences that must be carried by this schedule.
  */
class Schedule(val domInfo : DomainCoeffInfo, val deps : Set[Dependence]) {
  private var scheduleSummands : ArrayBuffer[Set[ScheduleSummand]] = new ArrayBuffer()
  private var scheduleVectors : ArrayBuffer[List[Rat]] = new ArrayBuffer()
  private var dim2StronglySatisfiedDeps : ArrayBuffer[Set[Dependence]] = ArrayBuffer.empty
  private var strRepr : String = null
  private var schedMap : isl.UnionMap = null

  /**
    * Produces a deep copy of this schedule.
    */
  override def clone() : Schedule = {
    val copy = new Schedule(domInfo, deps)
    copy.scheduleVectors = scheduleVectors.clone
    copy.dim2StronglySatisfiedDeps = dim2StronglySatisfiedDeps.clone()
    copy.scheduleSummands = scheduleSummands.clone
    copy.strRepr = strRepr
    copy.schedMap = schedMap
    return copy
  }

  /**
    * Produces a deep copy of this schedule and transfers all internal data
    * structures of the copy to the given Isl context.
    */
  def transferToCtx(newCtx : isl.Ctx) : Schedule = {
    val newDeps : Set[Dependence] = deps.map { d =>
      d.transferToCtx(newCtx)
    }
    val copy = new Schedule(domInfo.transferToCtx(newCtx), newDeps)
    scheduleVectors.zip(scheduleSummands).map((t : (List[Rat], Set[ScheduleSummand])) => {
      copy.addScheduleVector(t._1, t._2)
    })
    copy.strRepr = strRepr
    return copy
  }

  /**
    * Does the same as {@code transferToCtx(newCtx : isl.Ctx)}, but uses the
    * given set of dependences and the given {@code DomainCoeffInfo} for the new
    * schedule. Be sure that these objects are associated to Isl context
    * {@code newCtx}.
    */
  def transferToCtx(newCtx : isl.Ctx, ds : Set[Dependence],
    dInf : DomainCoeffInfo) : Schedule = {
    val copy = new Schedule(dInf, ds)
    scheduleVectors.zip(scheduleSummands).map((t : (List[Rat], Set[ScheduleSummand])) => {
      copy.addScheduleVector(t._1, t._2)
    })
    copy.strRepr = strRepr
    return copy
  }

  /**
    * Appends a new (inner) dimension to this schedule.
    *
    * @param coeffs rational schedule coefficients of the new schedule dimension.
    * @param schedSummands the linear combination of Chernikova generators that
    * formed {@code coeffs}.
    */
  def addScheduleVector(coeffs : List[Rat], schedSummands : Set[ScheduleSummand]) {
    scheduleSummands.append(schedSummands)
    scheduleVectors.append(coeffs)
    updateStronglySatisfiedDependences(scheduleVectors.length - 1)
    strRepr = null
    schedMap = null
  }

  /**
    * Appends a new (inner) dimension to this schedule that is equal to
    * dimension {@code otherDim} of schedule {@code o}.
    */
  def addForeignDim(o : Schedule, otherDim : Int) {
    if (otherDim >= o.numDims)
      throw new IllegalArgumentException("ownDim must not be >= o.numDims ("
        + o.numDims + "): " + otherDim)
    scheduleSummands.append(o.scheduleSummands(otherDim))
    scheduleVectors.append(o.scheduleVectors(otherDim))
    updateStronglySatisfiedDependences(scheduleVectors.length - 1)
    strRepr = null
    schedMap = null
  }

  /* 
   * Bookkeeping of the dependences that are satisfied strongly by each schedule
   * dimension.
   */
  private def updateStronglySatisfiedDependences(dim : Int) {
    val schedCoeffs : List[BigInt] = ScheduleVectorUtils.multiplyWithCommonDenominator(scheduleVectors(dim))
    val carriedDeps : Set[Dependence] = Schedule.getDepsStronglySatisfiedBySchedule(schedCoeffs, deps, domInfo)
    if (dim2StronglySatisfiedDeps.size == dim)
      dim2StronglySatisfiedDeps.append(carriedDeps)
    else
      dim2StronglySatisfiedDeps(dim) = carriedDeps
  }

  /**
    * Removes the last/innermost schedule dimension.
    */
  def removeLastScheduleVector {
    if (scheduleVectors.size > 0) {
      val lastIdx : Int = numDims - 1
      scheduleSummands.remove(lastIdx)
      scheduleVectors.remove(lastIdx)
      dim2StronglySatisfiedDeps.remove(lastIdx)
      strRepr = null
      schedMap = null
    }
  }

  /**
    * Get the schedule coefficients of dimension {@code dim}.
    */
  def getScheduleVector(dim : Int) : List[Rat] = {
    scheduleVectors(dim)
  }

  /**
    * Get the schedule coefficients of dimension {@code dim} multiplied by their
    * common denominator.
    */
  def getIntScheduleVector(dim : Int) : List[BigInt] = ScheduleVectorUtils.multiplyWithCommonDenominator(getScheduleVector(dim))

  /**
    * Replaces the schedule coefficients of dimension {@code dim}.
    * @see polyite.schedule.Schedule#addScheduleVector(List[Rat], Set[ScheduleSummand])
    */
  def replaceScheduleVector(dim : Int, coeffs : List[Rat], schedSummands : Set[ScheduleSummand]) {
    scheduleSummands(dim) = schedSummands
    scheduleVectors(dim) = coeffs
    val intCoeffs = ScheduleVectorUtils.multiplyWithCommonDenominator(coeffs)
    updateStronglySatisfiedDependences(dim)
    strRepr = null
    schedMap = null
  }

  /**
    * Replaces dimension {myDim} of this schedule with dimension {@otherDim} of
    * schedule {@code o}.
    */
  def shareScheduleVector(o : Schedule, myDim : Int, otherDim : Int) {
    if (myDim >= numDims)
      throw new IllegalArgumentException("myDim must not be >= numDims ("
        + numDims + "): " + myDim)
    if (otherDim >= o.numDims)
      throw new IllegalArgumentException("otherDim must not be >= o.numDims ("
        + o.numDims + "): " + otherDim)
    scheduleSummands(myDim) = o.scheduleSummands(otherDim)
    scheduleVectors(myDim) = o.scheduleVectors(otherDim)
    updateStronglySatisfiedDependences(myDim)
    strRepr = null
    schedMap = null
  }

  /**
    * Returns the linear combination of schedule generators that formed dimension
    * {@code dim}.
    */
  def getSchedSummands(dim : Int) : Set[ScheduleSummand] = scheduleSummands(dim)

  /**
    * The number of dimensions of this schedule.
    */
  def numDims : Int = scheduleVectors.size

  /**
    * Builds a representation of this schedule as an Isl union map with integer
    * schedule coefficients.
    */
  def getSchedule : isl.UnionMap = {
    if (schedMap == null)
      schedMap = ScheduleUtils.coeffMatrix2IslUnionMap(domInfo, scheduleVectors.map(ScheduleVectorUtils
        .multiplyWithCommonDenominator) : _*)
    return schedMap
  }

  /**
    * Builds a representation of the 1-d partial schedule of dimension {@code dim}
    * as an Isl union map with integer schedule coefficients.
    */
  def getSchedule(dim : Int) : isl.UnionMap = {
    if (dim >= numDims)
      throw new IllegalArgumentException("value of dim is too big: " + dim)
    return ScheduleUtils.coeffMatrix2IslUnionMap(domInfo, ScheduleVectorUtils.multiplyWithCommonDenominator(scheduleVectors(dim)))
  }

  /**
    * Builds a representation of the partial schedule of dimensions
    * {@code startDim} (inclusive) to {@code endDim} (exclusive) as an Isl union
    * map with integer schedule coefficients.
    */
  def getPartialSchedule(startDim : Int, endDim : Int) : isl.UnionMap = {
    if (startDim >= endDim)
      throw new IllegalArgumentException("startDim >= endDim: " + startDim + " >= " + endDim)

    if (endDim > numDims)
      throw new IllegalArgumentException("endDim > numScheds: " + endDim + " > " + numDims)

    if (startDim < 0)
      throw new IllegalArgumentException("startDim < 0: " + startDim)
    val schedVects = scheduleVectors.drop(startDim).take(endDim - startDim)
      .map(ScheduleVectorUtils.multiplyWithCommonDenominator)
    return ScheduleUtils.coeffMatrix2IslUnionMap(domInfo, schedVects : _*)
  }

  /**
    * Returns the rational coefficient matrix of this schedule.
    */
  def getScheduleVectors() : Iterable[List[Rat]] = {
    scheduleVectors.clone()
  }

  /**
    * Returns the set of dependences that are carried strongly up to dimension
    * {@code dim} (Starting from the outermost dimension).
    */
  def getDependencesCarriedUpToDim(dim : Int) : Set[Dependence] = {
    var result : Set[Dependence] = Set.empty
    for (i <- 0 until dim + 1) {
      result ++= dim2StronglySatisfiedDeps(i)
    }
    return result
  }

  /**
    * Returns the set of dependences that are satisfied strongly by dimension {@code dim}.
    */
  def getDependencesSatisfiedStronglyByDim(dim : Int) : Set[Dependence] = dim2StronglySatisfiedDeps(dim)

  /**
    * Returns the set of dependences that are carried weakly by dimension {@code dim}.
    */
  def getDependencesSatisfiedWeaklyByDim(dim : Int) : Set[Dependence] = {
    val coeffs : List[BigInt] = ScheduleVectorUtils.multiplyWithCommonDenominator(scheduleVectors(dim))
    return Schedule.getDepsWeaklySatisfiedBySchedule(coeffs, deps, domInfo)
  }

  /**
    * Information about the set of dependences that are satisfied strongly by each
    * dimension.
    */
  def getDependencesSatisfiedByDims : IndexedSeqView[Set[Dependence], scala.collection.mutable.ArrayBuffer[Set[Dependence]]] = dim2StronglySatisfiedDeps.view

  /**
    * Returns the set of all dependences that are carried by this
    * schedule.
    */
  def getCarriedDeps : Set[Dependence] = {
    if (numDims > 0)
      return getDependencesCarriedUpToDim(numDims - 1)
    return Set.empty[Dependence]
  }

  /**
    * String representation of the rational coefficient matrix of this schedule.
    */
  def scheduleVectorsString() : String = {
    val sb = new StringBuilder()
    sb ++= "List("
    for (v <- getScheduleVectors())
      sb ++= v.toString ++= ", "
    sb.delete(sb.length - 2, sb.length)
    sb += ')'
    return sb.toString()
  }

  /**
    * Check the linear independence between coeffs and the first maxDim
    * dimensions of this Schedule.
    */
  def checkLinearIndependence(coeffs : List[Rat], maxDim : Int) : Boolean = {
    if (maxDim <= 0)
      throw new IllegalArgumentException("maxDim must not be <= 0: " + maxDim)

    if (maxDim > numDims)
      throw new IllegalArgumentException("the value of maxDim is too big: " + maxDim)

    val coeffsInt : Array[BigInt] = ScheduleVectorUtils.multiplyWithCommonDenominator(coeffs).toArray
    val linDepSpace : isl.Set = computeLinDepSpace(maxDim)
    return !Isl.islSetContains(linDepSpace, coeffsInt)
  }

  private def checkLinearIndependenceHelp(coeffs : List[BigInt], maxDim : Int) : Boolean = {
    val linDepSpace : isl.Set = computeLinDepSpace(maxDim)
    return !Isl.islSetContains(linDepSpace, coeffs.toArray)
  }

  /**
    * Check whether dimension {@code dim} is linearly independent of any previous
    * dimensions in the coefficients of the iteration variables of the domain.
    */
  def isLinIndepToPrefix(dim : Int) : Boolean = {
    if (dim >= numDims)
      throw new IllegalArgumentException("dim is to big: " + dim)

    if (dim < 1)
      throw new IllegalArgumentException("cannot compare to nothing: " + dim)
    return checkLinearIndependenceHelp(ScheduleVectorUtils.multiplyWithCommonDenominator(
      scheduleVectors(dim)), dim)
  }

  /**
    * Check whether the 1-d schedule given through the schedule coefficients
    * {@code schedCoeffs} would satisfy strongly any dependences that have not
    * already been carried by this schedule.
    */
  def carriesNewDependency(schedCoeffs : List[Rat]) : Boolean = {
    !Schedule.getDepsStronglySatisfiedBySchedule(ScheduleVectorUtils.multiplyWithCommonDenominator(schedCoeffs), deps, domInfo)
      .subsetOf(getDependencesCarriedUpToDim(numDims - 1))
  }

  /**
    * Check whether dimension {@code dim} carries dependences that are not carried
    * by previous dimensions.
    */
  def carriesNewDependency(dim : Int) : Boolean = !getDepsNewlyCarriedBy(dim).isEmpty

  /**
    * Returns the set of dependences that are satisfied stronly by dimension {@code dim}
    * and have not been carried before by any other dimension.
    */
  def getDepsNewlyCarriedBy(dim : Int) : Set[Dependence] = {
    if (dim > 0)
      getDependencesSatisfiedStronglyByDim(dim) -- getDependencesCarriedUpToDim(dim - 1)
    else
      getDependencesSatisfiedStronglyByDim(dim)
  }

  /**
    * Check whether dimension {@code dim} is a constant schedule dimension.
    */
  def isConstant(dim : Int) : Boolean = {
    val coeffs : List[Rat] = scheduleVectors(dim)
    return ScheduleVectorUtils.checkIsConstant(coeffs, domInfo)
  }

  /**
    * Check whether a statement exists that is only shifted by a constant offset
    * in dimension dim.
    */
  def isConstantForAnyStmt(dim : Int) : Boolean = {
    val coeffs : List[Rat] = scheduleVectors(dim)
    return ScheduleVectorUtils.checkIsConstantForAnyStmt(coeffs, domInfo)
  }

  /**
    * Check whether the coefficients of the iteration variables for any statement
    * are linearly dependent to previous dimensions.
    */
  def isLinDepForAnyStmt(dim : Int) : Boolean = {
    if (dim < 1)
      throw new IllegalArgumentException("dim must be > 0: " + dim)

    val coeffs = ScheduleVectorUtils.multiplyWithCommonDenominator(scheduleVectors(dim))
    var isLinDep : Boolean = false
    for (sttmnt : String <- domInfo.stmtInfo.keySet) {
      val linDepSpace = computeLinDepSpace(dim, sttmnt)
      isLinDep ||= Isl.islSetContains(linDepSpace, coeffs.toArray)
    }
    return isLinDep
  }

  /**
    * Two schedules are equal iff their rational coefficient matrices are equal,
    * the linear combinations of Chernikova generators that formed the
    * dimensions of the matrices are equal and tiling settings equal.
    */
  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[Schedule]) {
      val os : Schedule = o.asInstanceOf[Schedule]
      var equal : Boolean = os.scheduleVectors.equals(scheduleVectors)
      equal &&= os.scheduleSummands.equals(scheduleSummands)
      return equal
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 31
    return scheduleVectors.hashCode() * prime + scheduleSummands.hashCode()
  }

  override def toString : String = {
    if (strRepr == null)
      strRepr = getSchedule.toString
    return strRepr
  }

  /**
    * Compute the set of schedule coefficient vectors that is linearly
    * dependent to the coefficients of dim 0 (inclusive) to maxDim (exclusive).
    * Linear dependence is only required for the coefficients of iteration
    * variables.
    */
  def computeLinDepSpace(maxDim : Int) : isl.Set = {
    if (scheduleVectors.isEmpty)
      return null

    val scheduleVectorsPrefix : Iterable[Array[BigInt]] = scheduleVectors.view
      .take(maxDim).map(ScheduleVectorUtils.multiplyWithCommonDenominator(_).toArray)

    var result : isl.Set = domInfo.universe
    for ((stmnt : String, StmtCoeffInfo(itStart, nrIt, parStart, cstIdx)) <- domInfo.stmtInfo) {
      // linear dependence for iterator coeffs only is required (params and consts are irrelevant)
      val linDepSpace : isl.Set = Schedule.computeLinDepSpaceBigInt(domInfo, scheduleVectorsPrefix, itStart, nrIt)
      result = result.intersect(linDepSpace)
    }
    return Isl.simplify(result)
  }

  /*
    * Compute the set of schedule coefficient vectors that is linearly
    * dependent to the coefficients of dim 0 (inclusive) to maxDim (exclusive).
    * Linear dependence is only required for the coefficients of the iteration
    * variables of statement sttmnt.
    */
  private def computeLinDepSpace(maxDim : Int, sttmnt : String) : isl.Set = {
    if (scheduleVectors.isEmpty)
      return null

    val scheduleVectorsPrefix : Iterable[Array[BigInt]] = scheduleVectors.view
      .take(maxDim).map(ScheduleVectorUtils.multiplyWithCommonDenominator(_).toArray)

    val sttmntInfo = domInfo.stmtInfo(sttmnt)
    val linDepSpace : isl.Set = Schedule.computeLinDepSpaceBigInt(domInfo,
      scheduleVectorsPrefix, sttmntInfo.itStart, sttmntInfo.nrIt)
    return Isl.simplify(linDepSpace)
  }

  /**
    * Compute the set of schedule coefficient vectors that is linearly
    * independent to the schedule coefficient vectors of this schedule.
    * Linear independence is only required for the coefficients of iteration
    * variables.
    *
    * @return Returns {@code None} if the resulting space is unconstrained.
    * Otherwise returns {@code Some(sp)}.
    */
  def computeLinIndepSpace(fixFreeDims : Boolean) : Option[isl.Set] = {
    return computeLinIndepSpace(numDims, fixFreeDims)
  }

  /**
    * Compute the set of schedule coefficient vectors that is linearly
    * independent to the schedule coefficient vectors of this schedule from
    * dimension 0 (inclusive) to dimension {@code maxDim} (exclusive). Linear
    * independence is only required for the coefficients of iteration variables.
    *
    * @return Returns {@code None} if the resulting space is unconstrained.
    * Otherwise returns {@code Some(sp)}.
    */
  def computeLinIndepSpace(maxDim : Int, fixFreeDims : Boolean) : Option[isl.Set] = {

    if (maxDim < 0 || maxDim > numDims)
      throw new IllegalArgumentException("maxDim must be from the interval [0, "
        + numDims + "(: " + maxDim)

    val schedVects : Iterable[Array[BigInt]] = scheduleVectors.view.take(maxDim)
      .map(ScheduleVectorUtils.multiplyWithCommonDenominator(_).toArray)

    if (schedVects.isEmpty)
      return None

    var result : isl.Set = domInfo.universe

    val zeroVal = isl.Val.zero(domInfo.ctx)
    val oneVal = isl.Val.one(domInfo.ctx)
    var allZero : Boolean = true

    for ((_, StmtCoeffInfo(itStart, nrIt, parStart, cstIdx)) <- domInfo.stmtInfo) {
      // linear independence for iterator coeffs only is required (params and consts are irrelevant)
      val linIndepSpace : isl.Set = Schedule.computeLinIndepSpaceBigInt(
        domInfo, schedVects, itStart, nrIt)
      if (linIndepSpace == null || Isl.simplify(linIndepSpace).isEmpty()) {
        if (fixFreeDims) {
          // dimension for this statement irrelevant... no exploration required, set anything
          result = result.fixVal(T_SET, itStart, oneVal) // 0-solution may not be allowed in universe
          for (i <- itStart + 1 until itStart + nrIt)
            result = result.fixVal(T_SET, i, zeroVal)
          for (i <- parStart until parStart + domInfo.nrParPS)
            result = result.fixVal(T_SET, i, zeroVal)
          result = result.fixVal(T_SET, cstIdx, zeroVal)
        }
      } else {
        allZero = false
        result = result.intersect(linIndepSpace)
      }
    }

    if (allZero)
      return None
    else
      return Some(Isl.simplify(result))
  }

  /**
    * Checks whether the value range of Int fits all integer coefficients of
    * this schedule.
    * @return Returns true if the condition holds. Otherwise returns false.
    */
  def fitsInt : Boolean = scheduleVectors.forall { v =>
    ScheduleVectorUtils.multiplyWithCommonDenominator(v).forall { x => x.isValidInt }
  }
}
