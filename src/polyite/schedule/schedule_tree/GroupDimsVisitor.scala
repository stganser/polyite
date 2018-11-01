package polyite.schedule.schedule_tree

import polyite.schedule.Dependence
import isl.Isl.TypeAliases._
import scala.collection.mutable.ListBuffer
import polyite.schedule.ScheduleUtils
import isl.Isl
import polyite.schedule.schedule_tree.normalization.RebuildDimSchedsVisitor
import isl.VoidCallback1
import isl.Conversions._

/**
  *  Groups DimNodes of a schedule tree (internal representation) into
  *  permutable bands of maximal size.
  */
class GroupDimsVisitor extends ParamScheduleNodeVisitor[ScheduleNode, Map[Dependence, isl.Map]] {

  def visit(t : SeqNode, deps : Map[Dependence, isl.Map]) : ScheduleNode = {
    val newChildren : List[(Set[String], ScheduleNode)] = t.getChildren.map(t => {
      // Filter deps for dependences that are associated only to statements from
      // the current subtree. All other dependences have already been carried.
      val remDeps : Map[Dependence, isl.Map] = deps.filter { d =>
        {
          t._1.contains(d._1.getTupleNameIn()) && t._1.contains(d._1.getTupleNameOut())
        }
      }
      (t._1, t._2.accept(this, remDeps))
    })
    return new SeqNode(t.getDomain, newChildren, t.getCoeffMatrDims)
  }

  def visit(t : SetNode, deps : Map[Dependence, isl.Map]) : ScheduleNode = {
    val newChildren : Set[(Set[String], ScheduleNode)] = t.getChildren.map(t => {
      // Filter deps for dependences that are associated only to statements from
      // the current subtree. All other dependences have already been carried.
      val remDeps : Map[Dependence, isl.Map] = deps.filter { d =>
        {
          t._1.contains(d._1.map.getTupleName(T_IN)) && t._1.contains(d._1.map.getTupleName(T_OUT))
        }
      }
      (t._1, t._2.accept(this, remDeps))
    })
    return new SetNode(t.getDomain, newChildren, t.getCoeffMatrDims)
  }

  private def getCarriedPartOfDep(sched : isl.UnionMap, dep : isl.Map, domain : isl.UnionSet) : isl.Map = {
    val stmtIn : String = dep.getTupleName(T_IN)
    val stmtOut : String = dep.getTupleName(T_OUT)
    val schedule : isl.UnionMap = sched.intersectDomain(domain)
    val happensBeforeRel : isl.UnionMap = Isl.constructHappensBeforeMap(Isl.islUnionSetFilter(domain, Set(stmtIn, stmtOut)), sched).coalesce().detectEqualities()
    var happensBeforeRelFiltered : isl.UnionMap = isl.UnionMap.empty(happensBeforeRel.getSpace)
    happensBeforeRel.foreachMap((m : isl.Map) => {
      if (m.getTupleName(T_IN).equals(stmtIn) && m.getTupleName(T_OUT).equals(stmtOut))
        happensBeforeRelFiltered = happensBeforeRelFiltered.addMap(m)
    })
    happensBeforeRelFiltered = happensBeforeRelFiltered.coalesce().detectEqualities()
    val res : isl.Map = if (happensBeforeRelFiltered.isEmpty())
      isl.Map.empty(dep.getSpace)
    else
      dep.intersect(isl.Map.fromUnionMap(happensBeforeRelFiltered)).coalesce().detectEqualities()
    return res
  }

  def visit(t : DimNode, deps : Map[Dependence, isl.Map]) : ScheduleNode = {
    var bands : ListBuffer[ListBuffer[(isl.UnionMap, Boolean)]] = ListBuffer.empty
    var coeffMatrDims : ListBuffer[ListBuffer[Int]] = ListBuffer.empty
    var currBand : ListBuffer[(isl.UnionMap, Boolean)] = ListBuffer.empty
    var currCoeffMatrDims : ListBuffer[Int] = ListBuffer.empty
    var remainingDeps : Map[Dependence, isl.Map] = deps

    var currBandWeaklyCarriedDeps = ScheduleUtils
      .getDepsCarriedWeaklyBySchedule(t.getSched, remainingDeps.keySet)
    var currDimStronglyCarriedDeps = ScheduleUtils
      .getDepsCarriedBySchedule(t.getSched, remainingDeps.keySet)
    var currBandStronglyCarriedDeps = currDimStronglyCarriedDeps
    var currDimIsCoincident : Boolean = true
    def updateUncarriedPartsCalcCoincidence(sched : isl.UnionMap) {
      val remDepsNotComplCarried : Set[Dependence] = remainingDeps.filter(!_._2.isEmpty()).keySet
      val remDepsCarriedPart : Map[Dependence, isl.Map] = remainingDeps.filter((p : (Dependence, isl.Map)) => remDepsNotComplCarried.contains(p._1)).map((p : (Dependence, isl.Map)) => (p._1, getCarriedPartOfDep(sched, p._2, t.getDomain)))
      currDimIsCoincident = remDepsCarriedPart.forall(_._2.isEmpty())
      remDepsNotComplCarried.map((d : Dependence) => {
        remainingDeps = remainingDeps + ((d, remainingDeps(d).subtract(remDepsCarriedPart(d))))
      })
    }
    updateUncarriedPartsCalcCoincidence(t.getSched)

    currBand.append((t.getSched, currDimIsCoincident))
    currCoeffMatrDims.append(t.getCoeffMatrDims.head)
    var tCurr : ScheduleNode = t.getChild

    while (tCurr.isInstanceOf[DimNode]) {
      val currDimNode : DimNode = tCurr.asInstanceOf[DimNode]
      // Determine all (still relevant) dependences that are carried by the
      // current schedule dimension.
      val currWeaklyCarriedDeps : Set[Dependence] = ScheduleUtils
        .getDepsCarriedWeaklyBySchedule(currDimNode.getSched, remainingDeps.keySet)

      // Determine all dependences that have not been carried yet and are
      // carried by the current schedule dimension
      currDimStronglyCarriedDeps = ScheduleUtils
        .getDepsCarriedBySchedule(currDimNode.getSched, remainingDeps.keySet) --
        currBandStronglyCarriedDeps
      updateUncarriedPartsCalcCoincidence(currDimNode.getSched)

      // Append the current dimension to the current band
      if (currWeaklyCarriedDeps.equals(currBandWeaklyCarriedDeps)) {
        currBand.append((currDimNode.getSched, currDimIsCoincident))
        currCoeffMatrDims.append(currDimNode.getCoeffMatrDims.head)
        currBandStronglyCarriedDeps ++= currDimStronglyCarriedDeps
      } else {
        // Start a new band
        bands.append(currBand)
        coeffMatrDims.append(currCoeffMatrDims)
        currBand = ListBuffer((currDimNode.getSched, currDimIsCoincident))
        currCoeffMatrDims = ListBuffer(currDimNode.getCoeffMatrDims.head)
        remainingDeps --= currBandStronglyCarriedDeps
        currBandStronglyCarriedDeps = currDimStronglyCarriedDeps
        currBandWeaklyCarriedDeps = currWeaklyCarriedDeps
      }
      tCurr = currDimNode.getChild
    }
    bands.append(currBand)
    coeffMatrDims.append(currCoeffMatrDims)
    remainingDeps --= currBandStronglyCarriedDeps
    val subTree : ScheduleNode = tCurr.accept(this, remainingDeps)
    return bands.zip(coeffMatrDims).foldRight(subTree)((b, t1) => new SimpleBandNode(t.getDomain, b._1.toList, t1,
      b._2.toList))
  }

  def visit(t : LeafNode, deps : Map[Dependence, isl.Map]) : ScheduleNode = t

  def handleBandNodes : ScheduleNode = throw new UnsupportedOperationException("Operation is not implemented for BandNodes.")

  def visit(t : SimpleBandNode, deps : Map[Dependence, isl.Map]) : ScheduleNode = handleBandNodes

  def visit(t : BandNodeLoop, deps : Map[Dependence, isl.Map]) : ScheduleNode = handleBandNodes
}