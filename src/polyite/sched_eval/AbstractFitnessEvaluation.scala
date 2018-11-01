package polyite.sched_eval

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.logging.Level
import java.util.logging.Logger

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import polyite.ScopInfo
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.util.Util
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleTreeConstruction

/**
  * Trait for strategies to evaluate the fitness of schedules.
  */
trait AbstractFitnessEvaluation {
  protected val myLogger : Logger = Logger.getLogger("")

  /**
    * Selects the schedules that must be evaluated.
    * @return Returns a tuple consisting of the schedules that have already been evaluated, together with their already
    * known evaluation result, and the schedules that must be evaluated.
    */
  def classifyForEvaluation(pairs : HashMap[Schedule, Fitness]) : Option[(HashMap[Schedule, Fitness], HashSet[Schedule])];

  /**
    * Given the set of schedules together with their fitness that form the basis for the next generation of the genetic
    * algorithm, select the next schedule that should produce an offspring.
    */
  def selectForMutation(pairs : Array[(Schedule, Fitness)]) : Schedule;

  /**
    * Method to benchmark the given schedule {@code s}. This method will be called in parallel and must, therefore, be thread-safe.
    * Put the evaluation result in {@link AbstractFitnessEvaluation#benchmarkingResult}.
    *
    * @param s The schedule to evaluate.
    * @param workedId The ID of the evaluation worked that has called the method. It can be used for the purpose of logging.
    */
  protected def benchmarkSchedule(workerId : Int)(s : Schedule);

  /**
    * Implement this function to provide access to the configuration.
    */
  protected def conf : Config;

  /**
    * Implement this function to provide access to the schedule coefficient vector space.
    */
  protected def domInfo : DomainCoeffInfo;

  /**
    * Implement this function to provide access to the data dependences in the SCoP to optimize.
    */
  protected def deps : Set[Dependence];

  /**
    * Implement this function to provide access to the SCoP to optimize.
    */
  protected def scop : ScopInfo;

  /**
    * Evaluates the given schedules.
    * @return Returns the schedules and each schedule's evaluation result. The second component of the result is a {@code Bool}
    * that indicates whether all schedules could be evaluated without unexpected errors. This is relevant, because, depending
    * on the implementation of {@link AbstractFitnessEvaluation#benchmarkingSchedule(Int)(Schedule)} benchmarking will
    * require calling external tools or submitting jobs to a cluster-management-system.
    */
  def evaluateSchedules(scheds : HashSet[Schedule]) : (HashMap[Schedule, Fitness], Boolean) = {
    if (scheds.isEmpty)
      return (HashMap.empty, true)

    var benchmarkingSuccessful : Boolean = true

    myLogger.info("Starting to benchmark " + scheds.size + " schedules.")
    try
      Util.mapInParallel(scheds, benchmarkSchedule,
        conf.numMeasurementThreads)
    catch {
      case t : Throwable => {
        myLogger.log(Level.SEVERE, "Failed to evaluate schedules.", t)
        benchmarkingSuccessful = false
      }
    }
    val result : HashMap[Schedule, Fitness] = HashMap.empty
    val schedsTmp : HashSet[Schedule] = scheds.clone()
    while (!benchmarkingResult.isEmpty()) {
      val (s : Schedule, evalResult : Fitness) = benchmarkingResult.poll()
      schedsTmp.remove(s)
      result.put(s, evalResult)
    }
    if (!schedsTmp.isEmpty) {
      myLogger.warning(schedsTmp.size + " schedules have not been evaluated.")
      for (s : Schedule <- schedsTmp) {
        result.put(s, FitnessUnknown)
      }
    }
    return (result.map(t => (t._1, t._2)), benchmarkingSuccessful)
  }

  private val evalResultCache : HashMap[String, Fitness] = HashMap.empty
  private var evalResultsCacheNQueries : Int = 0
  private var evalResultsCacheNHits : Int = 0

  /**
    * When implementing {@link AbstractFitnessEvaluation#benchmarkingSchedule(Int)(Schedule)} use this function to
    * add evaluation results to the evaluation results cache as soon as you have obtained these. The function is thread-safe.
    */
  protected def addToEvalResultCache(f : Fitness, scheds : ScheduleNode*) {
    val schedsStr : String = scheds.mkString(",")
    evalResultCache.synchronized {
      evalResultCache.put(schedsStr, f)
    }
  }

  /**
    * Call this function to check the evaluation results cache and find out whether a schedule has already been evaluated.
    * The function is thread-safe.
    */
  protected def checkEvalResultCache(scheds : ScheduleNode*) : Option[Fitness] = {
    val schedsStr : String = scheds.mkString(",")
    evalResultCache.synchronized {
      evalResultsCacheNQueries += 1
      val res : Option[Fitness] = evalResultCache.get(schedsStr)
      if (res.isDefined)
        evalResultsCacheNHits += 1
      return res
    }
  }

  /**
    * Returns the hit rate of the evaluation results cache. Returns {@code None}, iff the evaluation results cache has not been queried yet.
    */
  def getEvalResultsCacheHitRate() : Option[Double] = {
    if (evalResultsCacheNQueries <= 0)
      return None
    return Some(evalResultsCacheNHits.toDouble / evalResultsCacheNQueries.toDouble)
  }

  /**
    * When implementing {@link AbstractFitnessEvaluation#benchmarkingSchedule(Int)(Schedule)} add evaluation results
    * to this {@code ConcurrentLinkedQueue}.
    */
  protected val benchmarkingResult : ConcurrentLinkedQueue[(Schedule, Fitness)] =
    new ConcurrentLinkedQueue
}