package polyite.schedule.schedule_tree.normalization

import polyite.schedule.schedule_tree.ScheduleNode
import isl.Isl
import polyite.schedule.schedule_tree.LeafNode
import scala.collection.mutable.HashSet
import isl.VoidCallback1
import isl.Conversions._
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.SeqNode
import isl.UnionMap
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SimpleBandNode

object ElimSuperfluousSubTreesVisitor {

  private def handleNode(n : ScheduleNode, constr : () => ScheduleNode) : ScheduleNode = {
    val schedPrefixList : List[isl.UnionMap] = getSchedulePrefix(n)

    if (schedPrefixList.isEmpty)
      return constr()
    else {
      val schedPrefix : isl.UnionMap = Isl.buildMultiDimUnionMap(schedPrefixList)

      if (schedPrefix.isInjective())
        return new LeafNode(n.getDomain, List(n.getFather.getCoeffMatrDims.last))
      return constr()
    }
  }

  private def getSchedulePrefix(n : ScheduleNode) : List[isl.UnionMap] = {
    var domainStmts : HashSet[String] = HashSet.empty
    n.getDomain.foreachSet((s : isl.Set) => {
      domainStmts.add(s.getTupleName)
    })
    if (n.isRoot)
      return List.empty
    else
      return n.getFather.getSchedulePrefix(domainStmts.toSet)
  }
}

class ElimSuperfluousSubTreesVisitor extends ScheduleNodeVisitor[ScheduleNode] {

  override def toString() : String = "eliminate superfluous sub trees."

  def visit(n : LeafNode) : ScheduleNode = n

  // returns the (reduced) band and a flag carrying the information whether the current path of the schedule tree is
  // an injective schedule.
  def handleBand(nodePrefix : List[isl.UnionMap], band : List[(isl.UnionMap, Boolean)], domain : isl.UnionSet) : (List[(isl.UnionMap, Boolean)], Boolean) = {
    var schedPrefixList : List[isl.UnionMap] = nodePrefix

    if (!schedPrefixList.isEmpty) {
      val schedPrefix : isl.UnionMap = Isl.buildMultiDimUnionMap(schedPrefixList)
      if (schedPrefix.isInjective())
        return (List.empty, true)
    }
    
    var schedPrefix : isl.UnionMap = null
    var remainingBandMembers : List[(isl.UnionMap, Boolean)] = band
    var isInjective = false
    do {
      schedPrefixList = schedPrefixList :+ remainingBandMembers.head._1
      remainingBandMembers = remainingBandMembers.tail
      schedPrefix = Isl.buildMultiDimUnionMap(schedPrefixList)
      schedPrefix = schedPrefix.intersectDomain(domain)
      isInjective = schedPrefix.isInjective()
    } while (!isInjective && !remainingBandMembers.isEmpty)
      
    if (isInjective) {
      val nBandMembers2Keep : Int = band.size - remainingBandMembers.size
      return (band.take(nBandMembers2Keep), true)
    }
    return (band, false)
  }
  
  def visit(n : SimpleBandNode) : ScheduleNode = {
    val schedPrefixList : List[isl.UnionMap] = ElimSuperfluousSubTreesVisitor.getSchedulePrefix(n)
    val (bandReduced : List[(isl.UnionMap, Boolean)], injective) = handleBand(schedPrefixList, n.getScheds, n.getDomain)
    if (injective) {
      val coeffMatrDimsReduced : List[Int] = n.getCoeffMatrDims.take(bandReduced.length)
      return new SimpleBandNode(n.getDomain, bandReduced, new LeafNode(n.getDomain, List(coeffMatrDimsReduced.last)),
          coeffMatrDimsReduced)
    } else
      return new SimpleBandNode(n.getDomain, n.getScheds, n.getChild.accept(this), n.getCoeffMatrDims)
  }
  
  def visit(n : BandNodeLoop) : ScheduleNode = {
    val schedPrefixList : List[isl.UnionMap] = ElimSuperfluousSubTreesVisitor.getSchedulePrefix(n)
    val (bandReduced : List[(isl.UnionMap, Boolean)], injective) = handleBand(schedPrefixList, n.getScheds, n.getDomain)
    if (injective) {
      val loopsReduced : List[Map[String, Boolean]] = n.getLoop.take(bandReduced.size)
      val coeffMatrDimsReduced : List[Int] = n.getCoeffMatrDims.take(bandReduced.length)
      return new BandNodeLoop(n.getDomain, bandReduced, new LeafNode(n.getDomain, List(coeffMatrDimsReduced.last)),
          loopsReduced, coeffMatrDimsReduced)
    } else {
      return new BandNodeLoop(n.getDomain, n.getScheds, n.getChild.accept(this), n.getLoop, n.getCoeffMatrDims)
    }
  }

  def visit(n : DimNode) : ScheduleNode = {
    def constr() : ScheduleNode = {
      return new DimNode(n.getDomain, n.getSched, n.getChild.accept(this), n.getCoeffMatrDims)
    }
    return ElimSuperfluousSubTreesVisitor.handleNode(n, constr)
  }

  def visit(n : SetNode) : ScheduleNode = {
    def constr() : ScheduleNode = {
      return new SetNode(n.getDomain, n.getChildren.map((t : (Set[String], ScheduleNode)) => (t._1, t._2.accept(this))),
          n.getCoeffMatrDims)
    }
    return ElimSuperfluousSubTreesVisitor.handleNode(n, constr)
  }

  def visit(n : SeqNode) : ScheduleNode = {
    def constr() : ScheduleNode = {
      return new SeqNode(n.getDomain, n.getChildren.map((t : (Set[String], ScheduleNode)) => (t._1, t._2.accept(this))), 
          n.getCoeffMatrDims)
    }
    return ElimSuperfluousSubTreesVisitor.handleNode(n, constr)
  }
}
