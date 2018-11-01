package polyite.sched_eval

import polyite.config.Config
import polyite.config.MinimalConfig
import polyite.util.SCoPMetrics
import polyite.fitness.Feature
import polyite.schedule.DomainCoeffInfo
import polyite.ScopInfo
import polyite.schedule.Dependence

object EvaluationStrategyFactory {

  def createEvaluationStrategy(conf : Config, scop : ScopInfo, domInfo : DomainCoeffInfo, deps : Set[Dependence],
    makeTmpWorkingDirDistinct : String => String) : AbstractFitnessEvaluation = {
    return conf.evaluationStrategy match {
      case MinimalConfig.EvaluationStrategy.CLASSIFIER => {

        val scopMetrics : SCoPMetrics = SCoPMetrics.calc(scop)
        ScheduleClassification.init(
          config = conf,
          features = Feature.features,
          domainInfo = domInfo,
          scopInf = scop,
          dependences = deps,
          scopMetrics = scopMetrics)
        ScheduleClassification
      }
      case MinimalConfig.EvaluationStrategy.CLASSIFIER_AND_CPU => {
        val scopMetrics : SCoPMetrics = SCoPMetrics.calc(scop)
        BenchmarkingWithFilter.init(
          config = conf,
          features = Feature.features,
          domainInfo = domInfo,
          dependences = deps,
          scopMetrics = scopMetrics,
          scopInf = scop,
          makeTmpWorkingDirDistinct = makeTmpWorkingDirDistinct)
        BenchmarkingWithFilter
      }
      case MinimalConfig.EvaluationStrategy.CPU => {
        ScheduleEvaluation.init(conf, scop, domInfo, deps, makeTmpWorkingDirDistinct)
        ScheduleEvaluation
      }
    }
  }
}