package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.SimpleBandNode
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.ScopInfo
import polyite.util.SCoPMetrics

/**
  * Calculate the maximal depth of a schedule tree.
  */
object TreeDepth extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {
    val depth : Int = t.accept(TreeDepthVisitor)
    if (!conf.normalizeFeatures)
      return depth
    val maxDepth : Int = scopMetrics.numStmts + scopMetrics.maxLoopDepth + 1
    val res : Double = depth.toDouble / maxDepth
    return res
  }

  def isMultiStmt() : Boolean = true

  override def toString() : String = getClass.getSimpleName
}

private object TreeDepthVisitor extends ScheduleNodeVisitor[Int] {

  def visit(b : LeafNode) : Int = 1

  def visit(b : SetNode) : Int = {
    return 1 + b.getChildren.map(_._2.accept(this)).max
  }

  def visit(b : SeqNode) : Int = {
    return 1 + b.getChildren.map(_._2.accept(this)).max
  }

  def visit(b : DimNode) : Int = {
    return 1 + b.getChild.accept(this)
  }

  def visit(b : BandNodeLoop) : Int = {
    return 1 + b.getChild.accept(this)
  }

  def visit(b : SimpleBandNode) : Int = {
    return 1 + b.getChild.accept(this)
  }
}