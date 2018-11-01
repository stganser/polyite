package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import isl.Isl
import isl.Isl.TypeAliases._
import polyite.schedule.schedule_tree.ParamScheduleNodeVisitor
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.SimpleBandNode
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.ScopInfo
import polyite.schedule.DomainCoeffInfo
import polyite.util.SCoPMetrics

/**
  * For each statement subtract the depth of its outermost enclosing parallel loop from the total number of
  * enclosing loops. Sum over all statements. Note that Polly parallelizes only the outermost parallelizeable loop.
  */
object ParallelLoops extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {
    val stmtDomains : List[isl.Set] = Isl.splitUnionSet(t.getDomain)
    val result : Double = (stmtDomains.map { (d : isl.Set) =>
      {
        val stmt : String = d.getTupleName
        val nLoops : Int = d.dim(T_SET)
        (nLoops - t.accept(FirstParLoopIdxVisitor, (stmt, 0))).toDouble / (if (conf.normalizeFeatures && nLoops > 0) nLoops else 1)
      }
    } sum) / (if (conf.normalizeFeatures) stmtDomains.size else 1)
    return result
  }

  def isMultiStmt() : Boolean = false

  override def toString() : String = getClass.getSimpleName
}

private object FirstParLoopIdxVisitor extends ParamScheduleNodeVisitor[Int, (String, Int)] {

  def visit(b : LeafNode, param : (String, Int)) : Int = {
    val nestingDepth : Int = param._2
    return nestingDepth
  }

  def visit(b : SetNode, param : (String, Int)) : Int = {
    val stmt : String = param._1
    val nestingDepth : Int = param._2
    handleMultiChild(b.getChildren, stmt, nestingDepth)
  }

  private def handleMultiChild(children : Iterable[(Set[String], ScheduleNode)], stmt : String, nestingDepth : Int) : Int = {
    return children.find(_._1.contains(stmt)) match {
      case None => throw new IllegalArgumentException("sequence or set node does not schedule statement \"" + stmt
        + "\", which is part of the nodes domain.")
      case Some(t) => t._2.accept(this, (stmt, nestingDepth))
    }
  }

  def visit(b : SeqNode, param : (String, Int)) : Int = {
    val stmt : String = param._1
    val nestingDepth : Int = param._2
    handleMultiChild(b.getChildren, stmt, nestingDepth)
  }

  def visit(b : DimNode, param : (String, Int)) : Int = {
    throw new IllegalArgumentException("I did not expect to find a dim node.")
  }

  def visit(b : BandNodeLoop, param : (String, Int)) : Int = {
    val stmt : String = param._1
    val nestingDepth : Int = param._2
    val nestingDepthAfterNode : Int = b.getLoop.zip(b.getScheds.map(_._2)).foldLeft(nestingDepth)({
      (currDepth : Int, dim : (Map[String, Boolean], Boolean)) =>
        {
          val loopInfo : Map[String, Boolean] = dim._1
          val dimIsPar : Boolean = dim._2
          if (loopInfo(stmt) && dimIsPar)
            return currDepth
          if (loopInfo(stmt))
            currDepth + 1
          else
            currDepth
        }
    })
    return b.getChild.accept(this, (stmt, nestingDepthAfterNode))
  }

  def visit(b : SimpleBandNode, param : (String, Int)) : Int = {
    throw new IllegalArgumentException("I did not expect to find an ordinary band node.")
  }
}