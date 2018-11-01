package polyite.evolution

import polyite.schedule.Schedule
import polyite.sched_eval.Fitness
import scala.collection.mutable.HashMap
import polyite.config.ConfigGA

/**
  * Trait for strategies that select schedules that form the basis for the next
  * generation of schedules in the genetic algorithm.
  */
trait SelectionStrategy {

  /**
    * Selects {@code k} schedules according to some criterion.
    *
    * @param schedsEval schedules together with evaluation results.
    * @param generation Number of the subsequent generation. Make sure
    * to hand in only schedules that have been evaluated successfully.
    * @param k number of schedules to select
    * @return an array containing the selected schedules together with their
    * evaluation results.
    */
  def select(
    generation : Int,
    schedsEval : HashMap[Schedule, Fitness], k : Int) : Array[(Schedule, Fitness)]
}

abstract class SortingStrategy extends SelectionStrategy {
  protected def sortSchedules(
    schedsEval : List[(Schedule, Fitness)],
    conf : ConfigGA) : List[(Schedule, Fitness)] = {
    schedsEval.toList.sortBy(t => {
      if (!t._2.getEvalResult.isDefined)
        throw new IllegalArgumentException("Found a schedule without an evaluation result.")
      if (conf.optimizeParallelExecTime)
        t._2.getEvalResult.get.getNormalizedExecTimePar
      else
        t._2.getEvalResult.get.getNormalizedExecTimeSeq
    })
  }
}

/**
  * Selects the {@code k} schedules with the best execution time from the given
  * list of schedules. Depending on the configuration {@code config} either
  * parallel or sequential execution time is used as the selection criterion.
  */
class TakeKBestSchedules(conf : ConfigGA) extends SortingStrategy {
  override def select(
    generation : Int,
    schedsEval : HashMap[Schedule, Fitness], k : Int) : Array[(Schedule, Fitness)] = {
    val schedsOrdered : List[(Schedule, Fitness)] = sortSchedules(
      schedsEval.toList, conf)
    return schedsOrdered.take(math.min(schedsOrdered.size, k)).toArray
  }
}

/**
  * Selects the {@code k} schedules with the best execution time from the given
  * list of schedules. Depending on the configuration {@code config} either
  * parallel or sequential execution time is used as the selection criterion.
  *
  * If generation equals 1 the 25 percent best schedules are dropped before
  * selecting the {@code k} best ones from the remaining schedules.
  */
class TakeKBestSchedulesDropFstQuartile(conf : ConfigGA) extends SortingStrategy {
  override def select(
    generation : Int,
    schedsEval : HashMap[Schedule, Fitness], k : Int) : Array[(Schedule, Fitness)] = {
    var schedsOrdered : List[(Schedule, Fitness)] = sortSchedules(
      schedsEval.toList, conf)
    if (generation == 1)
      schedsOrdered = schedsOrdered.drop(schedsEval.size / 4)
    return schedsOrdered.take(math.min(schedsOrdered.size, k)).toArray
  }
}

/**
  * Takes selects {@code k} schedules. Prefers schedules that have been classified as good.
  */
class TakeKBestUseFitness(conf : ConfigGA) extends SelectionStrategy {
  override def select(
    generation : Int,
    schedsEval : HashMap[Schedule, Fitness], k : Int) : Array[(Schedule, Fitness)] = {
    if (!schedsEval.forall(_._2.getPrediction.isDefined))
      throw new IllegalArgumentException("At least some of the given schedules do not have a prediction.")
    return schedsEval.toList.sortBy(_._2.getPrediction.get).take(k).toArray
  }
}