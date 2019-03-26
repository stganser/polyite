package polyite

import polyite.config.Config
import polyite.util.SCoPMetrics
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.util.fitness.FeatureCalculator
import polyite.fitness.Feature

object ThesisSyrk {

  def main(args : Array[String]) {
    val scop : ScopInfo = MainUtil.loadScop("/home/stg/workspace/schedule-optimization/polybench-c-4.1/syrk/kernel_syrk___%entry.split---%for.end38.jscop").get._2
    val conf : Config = MainUtil.loadConfig("/home/stg/workspace/schedule-optimization/polybench-c-4.1/syrk/config_ga_syrk_kernel_syrk_%entry.split---%for.end38.properties", Config.loadAndValidateConfig).get

    val scopMetrics : SCoPMetrics = SCoPMetrics.calc(scop)

    val ctx : isl.Ctx = scop.getSched.getCtx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m, p_2, p_3] -> { Stmt_for_body6[i0, i1] -> [2*i0, i1, 0]; Stmt_for_body14[i0, i1, i2] -> [2*i0 + 1, i2, i1] }")
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    
    println(FeatureCalculator.calcFeatures(sched, None, None, domInfo, scop, scopMetrics, deps, Feature.features, conf))
  }
}