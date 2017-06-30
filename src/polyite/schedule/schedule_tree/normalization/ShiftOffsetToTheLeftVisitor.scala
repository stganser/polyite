package polyite.schedule.schedule_tree.normalization

import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.ScheduleNodeVisitorLeaveNDegUnchanged
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.ScheduleNode
import isl.Isl.TypeAliases._
import isl.VoidCallback2
import isl.Conversions._
import polyite.schedule.ScheduleUtils
import polyite.schedule.DomainCoeffInfo
import polyite.util.Rat
import polyite.schedule.ScheduleVectorUtils

class ShiftOffsetToTheLeftVisitor extends PartialSchedMapNormalizationVisitor {

  def simplifySchedMap(sched : isl.UnionMap, domain : isl.UnionSet) : Option[isl.UnionMap] = {
    val negInfty : isl.Val = isl.Val.neginfty(sched.getCtx)
    val maxCoeff : Array[isl.Val] = Array.fill(sched.params().dim(T_PAR) + 1)(negInfty)

    sched.foreachMap((m : isl.Map) => {
      val pwMaff : isl.PwMultiAff = isl.PwMultiAff.fromMap(m)
      var nPiece : Int = 0
      pwMaff.foreachPiece(((_ : isl.Set), (mAff : isl.MultiAff)) => {
        if (mAff.dim(T_OUT) > 1)
          throw new IllegalArgumentException("sched is multidimensional.")
        val aff : isl.Aff = mAff.getAff(0)
        val cst : isl.Val = aff.getConstantVal
        if (cst.gt(maxCoeff(0)))
          maxCoeff(0) = cst
        for (i <- 0 until aff.dim(T_PAR)) {
          val coeff : isl.Val = aff.getCoefficientVal(T_PAR, i)
          if (coeff.gt(maxCoeff(i + 1)))
            maxCoeff(i + 1) = coeff
        }
        nPiece += 1
        if (nPiece > 1)
          throw new IllegalArgumentException("sched is defined piecewise: " + sched)
      })
    })

    var newSched : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)

    sched.foreachMap((m : isl.Map) => {
      val pwMaff : isl.PwMultiAff = isl.PwMultiAff.fromMap(m)
      pwMaff.foreachPiece(((_ : isl.Set), (mAff : isl.MultiAff)) => {
        var aff : isl.Aff = mAff.getAff(0)
        if (maxCoeff(0).gt(negInfty))
          aff = aff.setConstantVal(aff.getConstantVal.sub(maxCoeff(0)))
        for (i <- 0 until aff.dim(T_PAR))
          if (maxCoeff(i + 1).gt(negInfty))
            aff = aff.setCoefficientVal(T_PAR, i, aff.getCoefficientVal(T_PAR, i).sub(maxCoeff(i + 1)))
        newSched = newSched.addMap(isl.Map.fromAff(aff))
      })
    })

    val reducedDomInfo : DomainCoeffInfo = DomainCoeffInfo(domain)
    val coeffs : Array[BigInt] = ScheduleUtils.islUnionMap2CoeffMatrix(reducedDomInfo, newSched).head
    if (ScheduleVectorUtils.checkAllZeroBigInt(coeffs))
      return None

    return Some(newSched)
  }
}