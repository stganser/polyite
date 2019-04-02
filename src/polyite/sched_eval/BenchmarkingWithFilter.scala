package polyite.sched_eval

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.util.Random

import polyite.ScopInfo
import polyite.config.Config
import polyite.fitness.Feature
import polyite.fitness.Prediction
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.util.SCoPMetrics

/**
  * This schedule evaluation strategy is based on the benchmarking strategy, but pre-filters the schedules using a
  * classifier.
  */
object BenchmarkingWithFilter extends AbstractScheduleEvaluation {

  def init(
    config : Config,
    features : List[Feature],
    domainInfo : DomainCoeffInfo,
    dependences : Set[Dependence],
    scopMetrics : SCoPMetrics,
    scopInf : ScopInfo,
    makeTmpWorkingDirDistinct : String => String) {
    this.synchronized {
      ScheduleClassification.init(config, features, domainInfo, dependences, scopMetrics, scopInf)
      super.initialize(config, scopInf, domainInfo, dependences, makeTmpWorkingDirDistinct)
    }
  }

  /**
    * Classifies all of the schedules to be evaluated and selects the good ones for benchmarking. If less than half of
    * the schedules are classified as good, randomly chosen bad schedules will also be elected for benchmarking in order
    * to retain diversity of the population.
    */
  override def classifyForEvaluation(pairs : HashMap[Schedule, Fitness]) : Option[(HashMap[Schedule, Fitness], HashSet[Schedule])] = {
    val Some((alreadyEvaluated : HashMap[Schedule, Fitness], toEvaluate : HashSet[Schedule])) = super.classifyForEvaluation(pairs)
    val (classificationResult : HashMap[Schedule, Fitness], classSuccess : Boolean) = ScheduleClassification.evaluateSchedules(toEvaluate)
    if (!classSuccess) {
      val nUnclassified : Int = classificationResult.count(!_._2.isCompletelyEvaluated)
      myLogger.warning(f"Failed to classify all schedules that require evaluation. ${nUnclassified} of ${toEvaluate.size} schedules could not be classified.")
      return None
    }
    val scheds2Evaluate : HashSet[Schedule] = HashSet.empty
    classificationResult.filter(_._2.getPrediction.get.pClass == Prediction.PerfClass.GOOD).map(_._1).foreach(scheds2Evaluate.add)
    val schedsNot2Evaluate : HashSet[Schedule] = HashSet.empty

    classificationResult.filter((t : (Schedule, Fitness)) => scheds2Evaluate.contains(t._1)).map(_._1).foreach(schedsNot2Evaluate.add)

    val nBenchmarkedInAlreadyEval : Int = alreadyEvaluated.count(t => {
      val bRes : Option[EvalResult] = t._2.getEvalResult
      bRes.isDefined && bRes.get.completelyEvaluated
    })

    while (scheds2Evaluate.size + nBenchmarkedInAlreadyEval < pairs.size / 2 && !schedsNot2Evaluate.isEmpty) {
      val schedsNot2EvalSeq : Array[Schedule] = schedsNot2Evaluate.toArray
      val randSched : Schedule = schedsNot2EvalSeq(Random.nextInt(schedsNot2EvalSeq.size))
      scheds2Evaluate.add(randSched)
      schedsNot2Evaluate.remove(randSched)
    }
    schedsNot2Evaluate.foreach((s : Schedule) => {
      alreadyEvaluated.put(s, PredictionAndEvalResult(classificationResult(s).getPrediction.get, EvalResult.notEvaluated.setCompletelyEvaluated(true)))
    })
    return Some((alreadyEvaluated, scheds2Evaluate))
  }
}