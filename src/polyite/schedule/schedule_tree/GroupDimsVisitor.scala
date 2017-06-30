package polyite.schedule.schedule_tree

import polyite.schedule.Dependence
import isl.Isl.TypeAliases._
import scala.collection.mutable.ListBuffer
import polyite.schedule.ScheduleUtils

/**
  *  Groups DimNodes of a schedule tree (internal representation) into
  *  permutable bands of maximal size.
  */
class GroupDimsVisitor extends ParamScheduleNodeVisitor[ScheduleNode, Set[Dependence]] {

  def visit(t : SeqNode, deps : Set[Dependence]) : ScheduleNode = {
    val newChildren : List[(Set[String], ScheduleNode)] = t.getChildren.map(t => {
      // Filter deps for dependences that are associated only to statements from
      // the current subtree. All other dependences have already been carried.
      val remDeps : Set[Dependence] = deps.filter { d =>
        {
          t._1.contains(d.getTupleNameIn()) && t._1.contains(d.getTupleNameOut())
        }
      }
      (t._1, t._2.accept(this, remDeps))
    })
    return new SeqNode(t.getDomain, newChildren)
  }

  def visit(t : SetNode, deps : Set[Dependence]) : ScheduleNode = {
    val newChildren : Set[(Set[String], ScheduleNode)] = t.getChildren.map(t => {
      // Filter deps for dependences that are associated only to statements from
      // the current subtree. All other dependences have already been carried.
      val remDeps : Set[Dependence] = deps.filter { d =>
        {
          t._1.contains(d.map.getTupleName(T_IN)) && t._1.contains(d.map.getTupleName(T_OUT))
        }
      }
      (t._1, t._2.accept(this, remDeps))
    })
    return new SetNode(t.getDomain, newChildren)
  }

  def visit(t : DimNode, deps : Set[Dependence]) : ScheduleNode = {
    var bands : ListBuffer[ListBuffer[(isl.UnionMap, Boolean)]] = ListBuffer.empty
    var currBand : ListBuffer[(isl.UnionMap, Boolean)] = ListBuffer.empty
    var remainingDeps : Set[Dependence] = deps

    var currBandWeaklyCarriedDeps = ScheduleUtils
      .getDepsCarriedWeaklyBySchedule(t.getSched, remainingDeps)
    var currDimStronglyCarriedDeps = ScheduleUtils
      .getDepsCarriedBySchedule(t.getSched, remainingDeps)
    var currBandStronglyCarriedDeps = currDimStronglyCarriedDeps
    var currDimIsCoincident : Boolean = currDimStronglyCarriedDeps.isEmpty // &&
    //        t.getSched.nMap() == 1
    currBand.append((t.getSched, currDimIsCoincident))
    var tCurr : ScheduleNode = t.getChild

    while (tCurr.isInstanceOf[DimNode]) {
      val currDimNode : DimNode = tCurr.asInstanceOf[DimNode]
      // Determine all (still relevant) dependences that are carried by the
      // current schedule dimension.
      val currWeaklyCarriedDeps : Set[Dependence] = ScheduleUtils
        .getDepsCarriedWeaklyBySchedule(currDimNode.getSched, remainingDeps)

      // Determine all dependences that have not been carried yet and are
      // carried by the current schedule dimension
      currDimStronglyCarriedDeps = ScheduleUtils
        .getDepsCarriedBySchedule(currDimNode.getSched, remainingDeps) --
        currBandStronglyCarriedDeps
      currDimIsCoincident = currDimStronglyCarriedDeps.isEmpty //&&
      //currDimNode.sched.nMap() == 1

      // Append the current dimension to the current band
      if (currWeaklyCarriedDeps.equals(currBandWeaklyCarriedDeps)) {
        currBand.append((currDimNode.getSched, currDimIsCoincident))
        currBandStronglyCarriedDeps ++= currDimStronglyCarriedDeps
      } else {
        // Start a new band
        bands.append(currBand)
        currBand = ListBuffer((currDimNode.getSched, currDimIsCoincident))
        remainingDeps --= currBandStronglyCarriedDeps
        currBandStronglyCarriedDeps = currDimStronglyCarriedDeps
        currBandWeaklyCarriedDeps = currWeaklyCarriedDeps
      }
      tCurr = currDimNode.getChild
    }
    bands.append(currBand)
    remainingDeps --= currBandStronglyCarriedDeps
    val subTree : ScheduleNode = tCurr.accept(this, remainingDeps)
    return bands.foldRight(subTree)((b, t1) => new SimpleBandNode(t.getDomain, b.toList, t1))
  }

  def visit(t : LeafNode, deps : Set[Dependence]) : ScheduleNode = t

  def handleBandNodes : ScheduleNode = throw new UnsupportedOperationException("Operation is not implemented for BandNodes.")

  def visit(t : SimpleBandNode, deps : Set[Dependence]) : ScheduleNode = handleBandNodes

  def visit(t : BandNodeLoop, deps : Set[Dependence]) : ScheduleNode = handleBandNodes
}