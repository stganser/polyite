package polyite.sched_eval

import polyite.schedule.Schedule
import polyite.config.Config
import polyite.ScopInfo
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Dependence
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.schedule.schedule_tree.normalization.RebuildDimSchedsVisitor
import polyite.schedule.schedule_tree.normalization.RemoveCommonOffsetVisitor
import polyite.schedule.schedule_tree.normalization.DivideCoeffsByGCDVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousSubTreesVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousDimNodesVisitor
import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import scala.collection.mutable.HashMap
import java.util.logging.Logger

/**
  * Utility to evaluate the duration of schedule tree simplification.
  */
object EvalSchedTreeGenerationDuration {

  val myLogger : Logger = Logger.getLogger("")

  /**
    * Measures the duration of schedule tree simplification {@code nMeasurements} times for each of the given schedules.
    * Produces an EvalResult for each of the schedules that contains the measured values.
    */
  def eval(scheds : Iterable[Schedule], conf : Config, nMeasurements : Int, scop : ScopInfo) : Map[Schedule, Fitness] = {
    return scheds.map { (s : Schedule) =>
      {
        val res : Fitness = EvalResultOnly(measureSched(s, conf, nMeasurements, scop))
        myLogger.info("benchmarking schedule " + s)
        (s, res)
      }
    } toMap
  }

  private def measureSched(sched : Schedule, conf : Config, nMeasurements : Int, scop : ScopInfo) : EvalResult = {
    val schedMap : isl.UnionMap = sched.getSchedule
    val basicSchedTree : ScheduleNode = ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(schedMap, sched.domInfo,
      scop, sched.deps, conf.insertSetNodes, conf.splitLoopBodies)
    // warm up
    for (i <- 0 until 2)
      simplifySchedTree(basicSchedTree)

    val durations : List[Double] = (for (i <- 0 until nMeasurements) yield {
      val start : Long = System.currentTimeMillis()
      simplifySchedTree(basicSchedTree)
      val end : Long = System.currentTimeMillis()
      (end - start).toDouble / 1000
    }).toList
    val res : EvalResult = EvalResult.notEvaluated
      .setSchedTreeSimplDurations(durations)
      .setCompletelyEvaluated(true)
    return res
  }

  val v1 : ScheduleNodeVisitor[ScheduleNode] = new RebuildDimSchedsVisitor
  val v2 : ScheduleNodeVisitor[ScheduleNode] = new RemoveCommonOffsetVisitor
  val v3 : ScheduleNodeVisitor[ScheduleNode] = new DivideCoeffsByGCDVisitor
  val v4 : ScheduleNodeVisitor[ScheduleNode] = new ElimSuperfluousSubTreesVisitor
  val v5 : ScheduleNodeVisitor[ScheduleNode] = new ElimSuperfluousDimNodesVisitor

  private def simplifySchedTree(s : ScheduleNode) {
    s.accept(v1)
      .accept(v2)
      .accept(v3)
      .accept(v4)
      .accept(v5)
  }
}