package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SimpleBandNode
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.ScopInfo
import polyite.schedule.DomainCoeffInfo
import polyite.util.SCoPMetrics

/**
  * Counts the number of leaf nodes in the schedule tree.
  */
object NumLeafs extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {
    val nLeafs : Int = t.accept(CountLeafsVisitor)
    if (!conf.normalizeFeatures)
      return nLeafs
    val maxNumLeafs : Int = scopMetrics.numStmts
    return nLeafs.toDouble / maxNumLeafs
  }

  def isMultiStmt() : Boolean = true

  override def toString() : String = getClass.getSimpleName
}

private object CountLeafsVisitor extends ScheduleNodeVisitor[Int] {

  def visit(b : LeafNode) : Int = 1

  def visit(b : SetNode) : Int = b.getChildren.map(_._2.accept(this)).sum

  def visit(b : SeqNode) : Int = b.getChildren.map(_._2.accept(this)).sum

  def visit(b : DimNode) : Int = b.getChild.accept(this)

  def visit(b : BandNodeLoop) : Int = b.getChild.accept(this)

  def visit(b : SimpleBandNode) : Int = b.getChild.accept(this)
}