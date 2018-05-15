package polyite.schedule.schedule_tree.normalization

import polyite.schedule.schedule_tree.ScheduleNodeVisitorLeaveNDegUnchanged
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SimpleBandNode

abstract class PartialSchedMapNormalizationVisitor extends ScheduleNodeVisitorLeaveNDegUnchanged {

  def simplifySchedMap(m : isl.UnionMap, domain : isl.UnionSet) : Option[isl.UnionMap];

  def visit(b : LeafNode) : ScheduleNode = {
    return b
  }

  def visit(b : DimNode) : ScheduleNode = {
    val schedSimplified : Option[isl.UnionMap] = simplifySchedMap(b.getSched, b.getDomain)
    val newChild : ScheduleNode = b.getChild.accept(this)
    if (schedSimplified.isDefined)
      return new DimNode(b.getDomain, schedSimplified.get, newChild, b.getCoeffMatrDims)
    return newChild
  }

  def visit(b : SimpleBandNode) : ScheduleNode = {
    val schedsSimplified : List[(Option[isl.UnionMap], Boolean)] = b.getScheds.map(t => (simplifySchedMap(t._1, b.getDomain), t._2))
    assert(schedsSimplified.length == b.getCoeffMatrDims.length)
    val schedsSimplifiedFiltered : List[(isl.UnionMap, Boolean)] = schedsSimplified.filter(_._1.isDefined).map(t => (t._1.get, t._2))
    val coeffMatrDimsFiltered : List[Int] = schedsSimplified.zip(b.getCoeffMatrDims).filter(_._1._1.isDefined).map(_._2)
    val newChild : ScheduleNode = b.getChild.accept(this)
    if (schedsSimplifiedFiltered.isEmpty)
      return newChild
    return new SimpleBandNode(b.getDomain, schedsSimplifiedFiltered, newChild, coeffMatrDimsFiltered)
  }

  def visit(b : BandNodeLoop) : ScheduleNode = {
    val schedsSimplified : List[(Option[isl.UnionMap], Boolean)] = b.getScheds.map(t => (simplifySchedMap(t._1, b.getDomain), t._2))
    assert(schedsSimplified.length == b.getCoeffMatrDims.length)
    val schedsSimplifiedPairs : List[((Option[isl.UnionMap], Boolean), Map[String, Boolean])] = schedsSimplified.zip(b.getLoop)
    val schedsSimplifiedFiltered : List[((Option[isl.UnionMap], Boolean), Map[String, Boolean])] = schedsSimplifiedPairs.filter(p => p._1._1.isDefined)
    val coeffMatrDimsFiltered : List[Int] = schedsSimplified.zip(b.getCoeffMatrDims).filter(_._1._1.isDefined).map(_._2)

    val newChild : ScheduleNode = b.getChild.accept(this)

    if (schedsSimplifiedFiltered.isEmpty)
      return newChild

    val schedsNew : List[(isl.UnionMap, Boolean)] = schedsSimplifiedFiltered.map(t => (t._1._1.get, t._1._2))
    val loopNew : List[Map[String, Boolean]] = schedsSimplifiedFiltered.map(t => t._2)
    return new BandNodeLoop(b.getDomain, schedsNew, newChild, loopNew, coeffMatrDimsFiltered)
  }
}
