package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import isl.Isl
import isl.Isl.TypeAliases._
import polyite.config.Config
import polyite.schedule.schedule_tree.ParamScheduleNodeVisitor
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.SimpleBandNode
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.ScopInfo
import polyite.util.SCoPMetrics

/**
  * For each statement: determine the size of its maximum enclosing band.  Sum over all statements.
  */
object TilableBands extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {
    val stmtDomains : List[isl.Set] = Isl.splitUnionSet(t.getDomain)
    val res : Double = (stmtDomains.map { (d : isl.Set) =>
      {
        val stmt : String = d.getTupleName
        val tilableBandSize : Int = t.accept(GetSizeOfTilableBand, (stmt, conf))
        val nLoops : Int = d.dim(T_SET)
        if (conf.normalizeFeatures && nLoops > 0) {
          assert(tilableBandSize <= nLoops, "n tilable loop dims: " + tilableBandSize + "; n loops: " + nLoops)
          tilableBandSize.toDouble / nLoops
        } else
          tilableBandSize.toDouble
      }
    } sum) / (if (conf.normalizeFeatures) scopMetrics.numStmts else 1)
    return res
  }

  def isMultiStmt() : Boolean = false

  override def toString() : String = getClass.getSimpleName
}

private object GetSizeOfTilableBand extends ParamScheduleNodeVisitor[Int, (String, Config)] {

  def visit(b : LeafNode, param : (String, polyite.config.Config)) : Int = 0

  def visit(b : SetNode, param : (String, polyite.config.Config)) : Int = {
    val stmt : String = param._1
    val conf : Config = param._2
    return handleMultiChild(b.getChildren, stmt, conf)
  }

  private def handleMultiChild(children : Iterable[(Set[String], ScheduleNode)], stmt : String, conf : Config) : Int = {
    return children.find(_._1.contains(stmt)) match {
      case None => throw new IllegalArgumentException("sequence or set node does not schedule statement \"" + stmt
        + "\", which is part of the nodes domain.")
      case Some(t) => t._2.accept(this, (stmt, conf))
    }
  }

  def visit(b : SeqNode, param : (String, polyite.config.Config)) : Int = {
    val stmt : String = param._1
    val conf : Config = param._2
    return handleMultiChild(b.getChildren, stmt, conf)
  }

  def visit(b : DimNode, param : (String, polyite.config.Config)) : Int = {
    throw new IllegalArgumentException("Did not expect to find a dim node.")
  }

  def visit(b : BandNodeLoop, param : (String, polyite.config.Config)) : Int = {
    val stmt : String = param._1
    val conf : Config = param._2
    if (SchedTreeUtil.isTiledByPolly(b, conf.tilingPermitInnerSeq))
      return b.getLoop.map(_(stmt)).count((x : Boolean) => x)
    return b.getChild.accept(this, param)
  }

  def visit(b : SimpleBandNode, param : (String, polyite.config.Config)) : Int = {
    throw new IllegalArgumentException("Did not expect to find a simple band node.")
  }
}