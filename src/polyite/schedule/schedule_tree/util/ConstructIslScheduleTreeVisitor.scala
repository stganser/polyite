package polyite.schedule.schedule_tree.util

import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.BandNodeLoop
import isl.Conversions._
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.SetNode
import isl.Isl
import polyite.schedule.schedule_tree.ScheduleNode
import isl.ScheduleNodeType
import polyite.schedule.schedule_tree.SimpleBandNode

object ConstructIslScheduleTreeVisitor {

  /*
   * Assumes that the only child of the current parent node is a newly inserted
   * band node. The number of elements of coincidence flags must match the
   * number of members of that band.
   */
  private def setCoincidence(s : isl.Schedule, coincidenceFlags : List[Boolean]) : isl.Schedule = {
    return s.mapScheduleNodeBottomUp((n : isl.ScheduleNode) => {
      if (n.hasParent() && n.parent().getType == isl.ScheduleNodeType.NodeDomain) {
        assert(n.getType.equals(isl.ScheduleNodeType.NodeBand))
        coincidenceFlags.zipWithIndex.foldLeft(n)((m : isl.ScheduleNode, t : (Boolean, Int)) => {
          m.bandMemberSetCoincident(t._2, if (t._1) 1 else 0)
        })
      } else n
    })
  }
}

class ConstructIslScheduleTreeVisitor extends ScheduleNodeVisitor[isl.Schedule] {

  def visit(t : LeafNode) : isl.Schedule = {
    return isl.Schedule.fromDomain(t.getDomain)
  }

  def visit(t : DimNode) : isl.Schedule = {
    val childSched : isl.Schedule = t.getChild.accept(this)
    return childSched.insertPartialSchedule(isl.MultiUnionPwAff.fromUnionMap(t.getSched))
  }

  def visit(t : SimpleBandNode) : isl.Schedule = handleBandNode(t)

  def visit(t : BandNodeLoop) : isl.Schedule = handleBandNode(t)

  private def handleBandNode(t : BandNode) : isl.Schedule = {
    val bandSched : isl.UnionMap = Isl.buildMultiDimUnionMap(t.getScheds.map(_._1))
    val childSched : isl.Schedule = t.getChild.accept(this)
    var result = childSched
      .insertPartialSchedule(isl.MultiUnionPwAff.fromUnionMap(bandSched))
    val coincidenceFlags : List[Boolean] = t.getScheds.map(_._2)
    result = ConstructIslScheduleTreeVisitor.setCoincidence(result, coincidenceFlags)
    return result
  }

  def visit(t : SeqNode) : isl.Schedule = {
    return seqCascade(t.getChildren)
  }

  def visit(t : SetNode) : isl.Schedule = {
    return setCascade(t.getChildren)
  }

  private def seqCascade(childTrees : List[(Set[String], ScheduleNode)]) : isl.Schedule = {
    childTrees match {
      case List()         => throw new IllegalArgumentException("Cannot handle a SeqNode with no children.")
      case List((_, t))   => return t.accept(this)
      case (_, t) :: rest => return t.accept(this).sequence(seqCascade(rest))
    }
  }

  private def setCascade(childTrees : Set[(Set[String], ScheduleNode)]) : isl.Schedule = {
    childTrees.toList match {
      case List()         => throw new IllegalArgumentException("Cannot handle a SeqNode with no children.")
      case List((_, t))   => return t.accept(this)
      case (_, t) :: rest => return t.accept(this).set(setCascade(rest.toSet))
    }
  }
}