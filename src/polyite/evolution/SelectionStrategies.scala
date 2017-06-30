package polyite.evolution

import scala.collection.mutable.HashMap

import polyite.config.ConfigGA
import polyite.sched_eval.EvalResult
import polyite.schedule.Schedule

/**
 * Strategies for selecting the schedules that form the basis for the next
 * generation of schedules in the genetic algorithm.
 */
object SelectionStrategies {

  private def sortSchedules(schedsEval : List[(Schedule, EvalResult)],
    conf : ConfigGA) : List[(Schedule, EvalResult)] = schedsEval.toList
    .sortBy(t => if (conf.optimizeParallelExecTime)
      t._2.getNormalizedExecTimePar
    else
      t._2.getNormalizedExecTimeSeq)

  /**
   * Selects the {@code k} schedules with the best execution time from the given
   * list of schedules. Depending on the configuration {@code config} either
   * parallel or sequential execution time is used as the selection criterion.
   * 
   * @param conf user configuration
   * @param schedsEval schedules together with evaluation results. Make sure
   * to hand in only schedules that have been evaluated successfully.
   * @param generation Number of the subsequent generation.
   * @param k number of schedules to select
   * @return an array containing the selected schedules together with their
   * evaluation results.
   */
  def takeKBestSchedules(conf : ConfigGA)(generation: Int,
      schedsEval : HashMap[Schedule, EvalResult], k : Int) : Array[(Schedule, EvalResult)] = {
    val schedsOrdered : List[(Schedule, EvalResult)] = sortSchedules(
      schedsEval.toList, conf)
    return schedsOrdered.take(math.min(schedsOrdered.size, k)).toArray
  }

  /**
   * Selects the {@code k} schedules with the best execution time from the given
   * list of schedules. Depending on the configuration {@code config} either
   * parallel or sequential execution time is used as the selection criterion.
   * 
   * If generation equals 1 the 25 percent best schedules are dropped before
   * selecting the {@code k} best ones from the remaining schedules.
   * 
   * @param conf user configuration
   * @param schedsEval schedules together with evaluation results. Make sure
   * to hand in only schedules that have been evaluated successfully.
   * @param generation Number of the subsequent generation.
   * @param k number of schedules to select
   * @return an array containing the selected schedules together with their
   * evaluation results.
   */
  def takeKBestSchedulesDropFstQuartile(conf : ConfigGA)(generation: Int,
      schedsEval : HashMap[Schedule, EvalResult], k : Int) : Array[(Schedule, EvalResult)] = {
    var schedsOrdered : List[(Schedule, EvalResult)] = sortSchedules(
      schedsEval.toList, conf)
    if (generation == 1)
      schedsOrdered = schedsOrdered.drop(schedsEval.size / 4)
    return schedsOrdered.take(math.min(schedsOrdered.size, k)).toArray
  }
}