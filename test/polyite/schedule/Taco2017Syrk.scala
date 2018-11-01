package polyite.schedule

import polyite.MainUtil
import polyite.ScopInfo
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.util.fitness.FeatureCalculator
import polyite.config.Config
import polyite.fitness.Feature
import polyite.util.SCoPMetrics

object Taco2017Syrk {

  def main(args : Array[String]) : Unit = {
    val scop : ScopInfo = MainUtil.loadScop("/home/stg/workspace/schedule-optimization/polybench-c-4.1/syrk/kernel_syrk___%entry.split---%for.end38.jscop").get._2
    val conf : Config = MainUtil.loadConfig("/home/stg/workspace/schedule-optimization/polybench-c-4.1/syrk/config_ga_syrk_kernel_syrk_%entry.split---%for.end38.properties", Config.loadAndValidateConfig).get

    val scopMetrics : SCoPMetrics = SCoPMetrics.calc(scop)

    val ctx : isl.Ctx = scop.getSched.getCtx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m, p_2, p_3] -> { Stmt_for_body6[i0, i1] -> [2*i0, 42*i1 + m, i0, 0]; Stmt_for_body14[i0, i1, i2] -> [2*i0 + 1, 0, 21*i1 - m, i2] }")
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    println(scop.getSched)
    println(ScheduleUtils.genCode(sched, scop.getDomain))
    val schedTree : ScheduleNode = ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(sched, domInfo, scop, deps, false, true)
    val schedTreeIsl : isl.Schedule = SchedTreeUtil.scheduleTree2IslScheduleTree(schedTree)
    println(schedTreeIsl)
    val schedTreeSimpl : ScheduleNode = SchedTreeUtil.simplifySchedTree(schedTree, deps)
    val schedTreeSimplIsl : isl.Schedule = SchedTreeUtil.scheduleTree2IslScheduleTree(schedTreeSimpl)
    println(schedTreeSimplIsl)
    println(ScheduleUtils.genCode(schedTreeSimplIsl, scop.getDomain))

    val schedTreeIslTiled : isl.Schedule = SchedTreeUtil.tileSchedule(schedTreeSimplIsl, 64, true)
    println(ScheduleUtils.genCode(schedTreeIslTiled, scop.getDomain))

    println(FeatureCalculator.calcFeatures(sched, None, None, domInfo, scop, scopMetrics, deps, Feature.features, conf))
  }
}