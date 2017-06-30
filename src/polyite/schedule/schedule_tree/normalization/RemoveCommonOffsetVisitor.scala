package polyite.schedule.schedule_tree.normalization

import scala.BigInt
import scala.math.BigInt.int2bigInt

import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleUtils
import polyite.schedule.ScheduleVectorUtils

import isl.Isl

class RemoveCommonOffsetVisitor extends PartialSchedMapNormalizationVisitor {

  override def toString() = "remove common offset"

  override def simplifySchedMap(m : isl.UnionMap, domain : isl.UnionSet) : Option[isl.UnionMap] = {
    val domInfo : DomainCoeffInfo = DomainCoeffInfo(domain)
    val coeffVect : Array[BigInt] = ScheduleUtils.islUnionMap2CoeffMatrix(domInfo, m).head
    val sttmts : List[String] = Isl.islUnionMapGetTupleNames(m).toList
    // iterate over the parameters
    for (pIdx : Int <- 0 until domInfo.nrParPS) {
      // determine the extreme coefficients for all statements for the current
      // parameter
      def getParCoeff(s : String) : BigInt = {
        return coeffVect(domInfo.stmtInfo(s).parStart + pIdx)
      }

      val headCoeff : BigInt = coeffVect(domInfo.stmtInfo(sttmts.head).parStart + pIdx)
      var minCoeff : BigInt = headCoeff
      var maxCoeff : BigInt = headCoeff
      for (s : String <- sttmts) {
        val sCoeff : BigInt = getParCoeff(s)
        if (sCoeff < minCoeff)
          minCoeff = sCoeff
        if (sCoeff > maxCoeff)
          maxCoeff = sCoeff
      }
      // determine the delta that can be added to the param coeff for each
      // statement without changing the schedule.
      val delta : BigInt = calcDelta(minCoeff, maxCoeff)
      // add the delta to the coefficients for the current parameter.
      for (s : String <- sttmts)
        coeffVect(domInfo.stmtInfo(s).parStart + pIdx) += delta
    }

    // treat the constant
    def getCst(s : String) : BigInt = {
      return coeffVect(domInfo.stmtInfo(s).cstIdx)
    }
    val headCst : BigInt = getCst(sttmts.head)
    var minCst : BigInt = headCst
    var maxCst : BigInt = headCst
    for (s : String <- sttmts) {
      val sCst : BigInt = getCst(s)
      if (sCst < minCst)
        minCst = sCst
      if (sCst > maxCst)
        maxCst = sCst
    }
    val delta : BigInt = calcDelta(minCst, maxCst)
    for (s : String <- sttmts) {
      coeffVect(domInfo.stmtInfo(s).cstIdx) += delta
    }
    if (ScheduleVectorUtils.checkAllZeroBigInt(coeffVect))
      return None
    return Some(ScheduleUtils.coeffMatrix2IslUnionMap(domInfo, coeffVect.toList))
  }

  private def calcDelta(minCoeff : BigInt, maxCoeff : BigInt) : BigInt = {
    if (minCoeff < 0 && maxCoeff < 0)
      return -maxCoeff
    if (minCoeff > 0 && maxCoeff > 0)
      return -minCoeff
    return BigInt(0)
  }
}