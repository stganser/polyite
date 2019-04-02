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
    val memAccesses : List[isl.BasicMap] = (Isl.splitUnionMapNoCoalesce(scop.getRds).map(Isl.splitMapNoCoalesce(_)).flatten ++
      Isl.splitUnionMapNoCoalesce(scop.getWrs).map(Isl.splitMapNoCoalesce(_)).flatten).filter(_.dim(T_OUT) > 0)
    val memAccessesForward : List[Boolean] = (
      for (
        stmt : String <- stmts.toList;
        val stmtDomain : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(scop.getDomain, Set(stmt)));
        if (stmtDomain.dim(T_SET) > 0 && !stmtDomain.isSingleton())
      ) yield {
        val stmtSched : isl.Map = isl.Map.fromUnionMap(getStmtLoopSched(t, stmt))
        val stmtAccessesStmt : List[isl.BasicMap] = memAccesses.filter(_.getTupleName(T_IN) == stmt)
        val memAccessesForwardStmt : List[Boolean] = stmtAccessesStmt.map { (memAcc : isl.BasicMap) =>
          {
            val memAccMapped : isl.BasicMap = Isl.islBasicMapFromMap(memAcc.applyDomain(stmtSched))
            var comp : isl.Map = isl.Map.fromDomainAndRange(stmtDomain, stmtDomain)
            comp = comp.applyDomain(stmtSched).applyRange(stmtSched)

            val compLsp : isl.LocalSpace = isl.LocalSpace.fromSpace(comp.getSpace)
            for (d <- 0 until (stmtSched.dim(T_OUT) - 1)) {
              var constr : isl.Constraint = isl.Constraint.allocEquality(compLsp)
              constr = constr.setCoefficientSi(T_IN, d, 1)
              constr = constr.setCoefficientSi(T_OUT, d, -1)
              comp = comp.addConstraint(constr)
            }
            val ineqConstr : isl.Constraint = isl.Constraint.allocInequality(isl.LocalSpace.fromSpace(comp.getSpace))
              .setCoefficientSi(T_IN, stmtSched.dim(T_OUT) - 1, -1)
              .setCoefficientSi(T_OUT, stmtSched.dim(T_OUT) - 1, 1).setConstantSi(-1)
            comp = comp.addConstraint(ineqConstr)

            comp = comp.applyDomain(memAccMapped).applyRange(memAccMapped)

            val deltas : isl.Set = comp.deltas()

            var lastForward : isl.Set = isl.Set.universe(deltas.getSpace)
            for (d <- 0 until (lastForward.dim(T_SET) - 1))
              lastForward = lastForward.fixVal(T_SET, d, isl.Val.fromInt(lastForward.getCtx, 0))
            val constr : isl.Constraint = isl.Constraint.allocInequality(isl.LocalSpace.fromSpace(lastForward.getSpace))
              .setCoefficientSi(T_SET, lastForward.dim(T_SET) - 1, 1)
            lastForward = lastForward.addConstraint(constr)

            deltas.isSubset(lastForward)
          }
        }
        memAccessesForwardStmt
      }).toList.flatten
    var n = memAccessesForward.size
    if (n == 0)
      return 1
    val result : Double = (memAccessesForward.count(_ == true)).toDouble / n.toDouble
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

  override def getDisplayName() : String = "memory access pattern"
}
