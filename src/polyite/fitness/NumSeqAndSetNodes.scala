package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.SimpleBandNode
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.ScopInfo
import polyite.util.SCoPMetrics

/**
  * Count the number of sequence nodes and set nodes.
  */
object NumSeqAndSetNodes extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {
    val nSeqNodes : Int = t.accept(CountNumSeqAndSetNodesVisitor)
    if (!conf.normalizeFeatures)
      return nSeqNodes
    val maxNumSeqNodes : Int = scopMetrics.numStmts - 1
    val res : Double = nSeqNodes.toDouble / maxNumSeqNodes
    return res
  }

  def isMultiStmt() : Boolean = true

  override def toString() : String = getClass.getSimpleName
}

private object CountNumSeqAndSetNodesVisitor extends ScheduleNodeVisitor[Int] {

  def visit(b : LeafNode) : Int = 0

  def visit(b : SetNode) : Int = 1 + b.getChildren.map(_._2.accept(this)).sum

  def visit(b : SeqNode) : Int = 1 + b.getChildren.map(_._2.accept(this)).sum

  def visit(b : DimNode) : Int = b.getChild.accept(this)

  def visit(b : BandNodeLoop) : Int = b.getChild.accept(this)

  def visit(b : SimpleBandNode) : Int = b.getChild.accept(this)
}