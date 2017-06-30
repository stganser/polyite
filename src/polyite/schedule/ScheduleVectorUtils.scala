package polyite.schedule

import scala.math.BigInt.int2bigInt

import polyite.util.Rat
import polyite.util.Util
import scala.collection.mutable.HashSet
import isl.Isl.TypeAliases._

object ScheduleVectorUtils {

  def checkAllZero(coeffs : Iterable[Rat]) : Boolean = coeffs.forall { _ == Rat(0) }

  def checkAllZeroBigInt(coeffs : Iterable[BigInt]) : Boolean = coeffs.forall { _ == BigInt(0) }

  def checkIsConstant(coeffs : Iterable[Rat], domInfo : DomainCoeffInfo) : Boolean = {
    var found = true
    for (sInfo : StmtCoeffInfo <- domInfo.stmtInfo.values) {
      found &&= coeffs.view.drop(sInfo.itStart).take(sInfo.nrIt).forall { _ == Rat(0) }
    }
    found
  }

  def checkIsConstantForAnyStmt(coeffs : Iterable[Rat], domInfo : DomainCoeffInfo) : Boolean = {
    var found = false
    for (sInfo : StmtCoeffInfo <- domInfo.stmtInfo.values) {
      found ||= coeffs.view.drop(sInfo.itStart).take(sInfo.nrIt).forall { _ == Rat(0) }
    }
    found
  }

  // v1 + c * v2
  def add(v1 : List[Rat], v2 : List[Rat], c : Rat) : List[Rat] = v1.zip(v2)
    .map((x : (Rat, Rat)) => x._1 + (x._2 * c))

  def checkForZeroCoeffPerStatement(coeffs : Array[Rat],
    domInfo : DomainCoeffInfo) : Boolean = {
    var found = false
    for (sInfo : StmtCoeffInfo <- domInfo.stmtInfo.values) {
      val stmtCoeffs : Array[Rat] = (coeffs.drop(sInfo.itStart).take(sInfo.nrIt)
        ++ coeffs.drop(sInfo.parStart).take(domInfo.nrParPS)).:+(coeffs(sInfo.cstIdx))
      found ||= (coeffs(sInfo.cstIdx) == Rat(0)) && checkAllZero(coeffs.view
        .drop(sInfo.itStart).take(sInfo.nrIt)) && checkAllZero(coeffs.view
        .drop(sInfo.itStart).take(sInfo.nrIt))
    }
    found
  }

  def getMaxAbsoluteComponent(v : Iterable[Rat]) : Rat = (v.map { x => x.abs })
    .foldLeft(Rat(0))((x, y) => if (x > y) x else y)

  def checkLinearIndep(v1 : Iterable[Rat], v2 : Iterable[Rat]) : Boolean = {
    if (v1.size != v2.size)
      throw new IllegalArgumentException("v1 and v2 don't have the same dimensionality!")

    if (isZeroVector(v1) || isZeroVector(v2))
      return false

    val v1v2 = v1.zip(v2)

    // e.g. (0, 1) or (-3, 0) implies linear independence
    if (v1v2.foldLeft(false)((b : Boolean, t : (Rat, Rat)) => b
      || (t._1 == Rat(0) && t._2 != Rat(0)) || (t._1 != Rat(0) && t._2 == Rat(0))))
      return true
    val v1v2NoZero : Iterable[(Rat, Rat)] = v1v2
      .filterNot((t : (Rat, Rat)) => t._1 == Rat(0) /* && t._2 == Rat(0)*/ )
    val idx1Factor : Rat = v1v2NoZero.head._1 / v1v2NoZero.head._2
    return !(v1v2NoZero forall ((t : (Rat, Rat)) => (t._2 * idx1Factor) == t._1))
  }

  def isZeroVector(v : Iterable[Rat]) = v.forall { _ == Rat(0) }

  /**
    * Multiplies each component of the given vector of rational numbers with
    * their common denominator. The result is a vector of integers.
    */
  def multiplyWithCommonDenominator(v : List[Rat]) : List[BigInt] = {
    val denomVals : HashSet[BigInt] = HashSet.empty
    val commonDenom : BigInt = v.foldLeft(BigInt(1))((d : BigInt, x : Rat) =>
      if (d % x.denominator == 0)
        d
      else {
        d * (x.denominator / x.denominator.gcd(d))
      })
    val result : List[BigInt] = v map { x => x.numerator * (commonDenom / x.denominator) }
    return result
  }
}