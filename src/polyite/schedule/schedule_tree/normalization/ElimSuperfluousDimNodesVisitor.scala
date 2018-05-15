package polyite.schedule.schedule_tree.normalization

import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.ScheduleNode
import scala.collection.mutable.HashSet
import polyite.schedule.schedule_tree.ScheduleNodeVisitorLeaveNDegUnchanged
import isl.VoidCallback1
import isl.Conversions._
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SimpleBandNode

class ElimSuperfluousDimNodesVisitor extends ScheduleNodeVisitorLeaveNDegUnchanged {

  override def toString() : String = "eliminate superfluous dim nodes."

  def visit(t : DimNode) : ScheduleNode = {
    var domainStmts : HashSet[String] = HashSet.empty
    t.getDomain.foreachSet((s : isl.Set) => {
      domainStmts.add(s.getTupleName)
    })
    val domainStmtsArr : Array[String] = domainStmts.toArray
    var canEliminate : Boolean = true
    for (i <- 0 until domainStmtsArr.size) {
      val s1 : String = domainStmtsArr(i)
      for (j <- i until domainStmtsArr.size) {
        val s2 : String = domainStmtsArr(j)
        val stmts : Set[String] = Set(s1, s2)
        val schedPrefix : List[isl.UnionMap] = if (t.isRoot)
          List.empty
        else {
          t.getFather.getSchedulePrefix(stmts)
        }
        val order = SchedTreeUtil.calcOrder(t.getSched, schedPrefix, s1, s2, t.getDomain)
        if (order.isDefined && !order.get.isZeroOnly)
          canEliminate = false
      }
    }
    if (canEliminate)
      return t.getChild.accept(this)
    return new DimNode(t.getDomain, t.getSched, t.getChild.accept(this), t.getCoeffMatrDims)
  }

  private def handleBandNodes : ScheduleNode = throw new UnsupportedOperationException("Operation is not implemented for BandNodes.")

  def visit(t : SimpleBandNode) : ScheduleNode = handleBandNodes

  def visit(t : BandNodeLoop) : ScheduleNode = handleBandNodes

  def visit(t : LeafNode) : ScheduleNode = t
}
