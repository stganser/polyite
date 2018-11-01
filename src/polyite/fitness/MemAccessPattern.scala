package polyite.fitness

import java.util.logging.Logger

import polyite.ScopInfo
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.util.SCoPMetrics

import isl.Conversions.convertLambdaToVoidCallback2
import isl.Isl
import isl.Isl.TypeAliases.T_IN
import isl.Isl.TypeAliases.T_OUT
import isl.Isl.TypeAliases.T_SET
import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import polyite.schedule.schedule_tree.ParamScheduleNodeVisitor
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SimpleBandNode

/**
  * Analyzes the memory access pattern. Good schedules access arrays in a forward direction in the innermost loop
  * surrounding each access.
  */
object MemAccessPattern extends Feature {

  private val myLogger : Logger = Logger.getLogger("")

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {

    val stmts : Set[String] = Isl.islUnionSetGetTupleNames(scop.getDomain)
    val memAccesses : List[isl.BasicMap] = (Isl.splitUnionMapNoCoalesce(scop.getRds).map(Isl.splitMapNoCoalesce(_)).flatten.toList ++
      Isl.splitUnionMapNoCoalesce(scop.getWrs).map(Isl.splitMapNoCoalesce(_)).flatten.toList).filter(_.dim(T_OUT) > 0)
    var nFiltered : Int = 0
    val memAccessesForward : List[Boolean] = (
      for (
        stmt : String <- stmts;
        val stmtDomain : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(scop.getDomain, Set(stmt)));
        val stmtSched : isl.UnionMap = getStmtLoopSched(t, stmt) if (!stmtSched.isEmpty() && stmtDomain.dim(T_SET) > 0)
      ) yield {
        val stmtAccessesStmt : List[isl.BasicMap] = memAccesses.filter(_.getTupleName(T_IN) == stmt)
        val memAccessesForward : List[Boolean] = stmtAccessesStmt.map { (memAcc : isl.BasicMap) =>
          {
            val memAccMapped : isl.BasicMap = Isl.islBasicMapFromMap(isl.Map.fromUnionMap(memAcc.applyDomain(stmtSched)))
            if (memAccMapped.isSingleValued()) {
              val lastNonConstantInputDim : Int = (0 until memAccMapped.dim(T_IN)).filterNot(Isl.isConstInputDim(memAccMapped, _)).max
              val memAccMappedPwMAff : isl.PwMultiAff = isl.PwMultiAff.fromMap(memAccMapped)
              val forward : Boolean = (0 until memAccMappedPwMAff.dim(T_OUT) - 1).forall((dim : Int) => {
                val pwAff : isl.PwAff = memAccMappedPwMAff.getPwAff(dim)
                pwAff2Aff(pwAff).getCoefficientVal(T_IN, lastNonConstantInputDim).isZero()
              })
              val coeffLastDim : isl.Val = pwAff2Aff(memAccMappedPwMAff.getPwAff(memAccMappedPwMAff.dim(T_OUT) - 1)).getCoefficientVal(T_IN, lastNonConstantInputDim)
              forward && (coeffLastDim.isZero() || coeffLastDim.isPos())
            } else {
              nFiltered += 1
              true
            }
          }
        }
        memAccessesForward
      }).toList.flatten
    val result : Double = (memAccessesForward.count(_ == true) - nFiltered).toDouble / (memAccessesForward.size - nFiltered).toDouble
    return result
  }

  private def getStmtLoopSched(t : ScheduleNode, stmt : String) : isl.UnionMap = {
    Isl.buildMultiDimUnionMap(t.accept(StmtSchedVisitor, stmt), t.getDomain.getSpace)
  }

  private object StmtSchedVisitor extends ParamScheduleNodeVisitor[List[isl.Map], String] {

    def visit(b : LeafNode, stmt : String) : List[isl.Map] = List.empty

    def visit(b : SetNode, stmt : String) : List[isl.Map] = {
      return handleMultiChild(b.getChildren, stmt)
    }

    private def handleMultiChild(children : Iterable[(Set[String], ScheduleNode)], stmt : String) : List[isl.Map] = {
      return children.filter(_._1.contains(stmt)).head._2.accept(this, stmt)
    }

    def visit(b : SeqNode, stmt : String) : List[isl.Map] = {
      return handleMultiChild(b.getChildren, stmt)
    }

    def visit(b : DimNode, stmt : String) : List[isl.Map] = throw new IllegalArgumentException("Did not expect to find a DimNode.")

    def visit(b : BandNodeLoop, stmt : String) : List[isl.Map] = {
      val resChild = b.getChild.accept(this, stmt)
      return b.getScheds.map(_._1).zip(b.getLoop.map(_(stmt))).foldRight(resChild)((t : (isl.UnionMap, Boolean), res : List[isl.Map]) => {
        val generatesLoop : Boolean = t._2
        val sched : isl.UnionMap = t._1
        if (generatesLoop)
          isl.Map.fromUnionMap(Isl.islUnionMapFilter(sched, Set(stmt))) :: res
        else
          res
      })
    }

    def visit(b : SimpleBandNode, param : String) : List[isl.Map] = throw new IllegalArgumentException("Did not expect to find a SimpleBandNode.")
  }

  private def pwAff2Aff(pwAff : isl.PwAff) : isl.Aff = {
    var aff : isl.Aff = null
    pwAff.foreachPiece((_ : isl.Set, piece : isl.Aff) => {
      if (aff != null)
        throw new IllegalArgumentException("PWAff has more than one piece: " + pwAff)
      aff = piece
    })
    return aff
  }

  def isMultiStmt() : Boolean = false

  override def toString() : String = getClass.getSimpleName
}