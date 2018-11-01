package polyite.schedule.schedule_tree.normalization

import polyite.schedule.schedule_tree.ScheduleNodeVisitorLeaveNDegUnchanged
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.LeafNode
import polyite.util.Util
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SimpleBandNode

class DivideCoeffsByGCDVisitor extends ScheduleNodeVisitorLeaveNDegUnchanged {

  override def toString() : String = "divide coefficients by GCD"

  def visit(n : DimNode) : ScheduleNode = {
    val newSched : isl.UnionMap = Util.divideCoeffsByGcd(n.getSched)
    return new DimNode(n.getDomain, newSched, n.getChild.accept(this), n.getCoeffMatrDims)
  }

  def visit(n : SimpleBandNode) : ScheduleNode = {
    return new SimpleBandNode(n.getDomain, transformBand(n.getScheds), n.getChild.accept(this), n.getCoeffMatrDims)
  }

  private def transformBand(origScheds : List[(isl.UnionMap, Boolean)]) : List[(isl.UnionMap, Boolean)] = {
    return origScheds.map((t : (isl.UnionMap, Boolean)) => (Util.divideCoeffsByGcd(t._1), t._2))
  }

  def visit(n : BandNodeLoop) : ScheduleNode = {
    return new BandNodeLoop(n.getDomain, transformBand(n.getScheds), n.getChild.accept(this), n.getLoop, n.getCoeffMatrDims)
  }

  def visit(n : LeafNode) : ScheduleNode = n
}