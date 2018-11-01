package polyite.evolution

import scala.collection.mutable.HashMap
import polyite.sched_eval.Fitness
import polyite.schedule.Schedule
import polyite.config.ConfigGA
import java.util.logging.Logger
import polyite.fitness.Prediction
import polyite.config.MinimalConfig
import polyite.sched_eval.EvalResult

/**
  * A criterion that is evaluated after each iteration of the genetic algorithm to decide whether it should terminate.
  */
trait GATerminationCriterion {

  /**
    * Makes decision based on the generation of the genetic algorithm and the current population
    * @return Returns {@code true} iff the genetic algorithm should terminate.
    */
  def decide(nextGeneration : Int, population : HashMap[Schedule, Fitness]) : Boolean
}

object GATerminationCriterion {

  /**
    * Given a number of {@code GATerminationCriterion}s, {@code union} creates a new criterion that accepts, iff any
    * of the given criteria accepts.
    */
  def union(criteria : GATerminationCriterion*) : GATerminationCriterion = {
    return new GATerminationCriterion {
      override def decide(nextGeneration : Int, population : HashMap[Schedule, Fitness]) : Boolean = {
        criteria.exists(_.decide(nextGeneration, population))
      }
    }
  }
}

object FixedNIterations {
  private val myLogger : Logger = Logger.getLogger("")
}

/**
  * This criterion checks whether the index of the next generation exceeds {@code conf.maxGenerationToReach}.
  */
class FixedNIterations(conf : ConfigGA) extends GATerminationCriterion {

  override def decide(nextGeneration : Int, population : HashMap[Schedule, Fitness]) : Boolean = {
    val stop : Boolean = nextGeneration > conf.maxGenerationToReach
    if (stop)
      FixedNIterations.myLogger.info("Reached the configured maximum generation. Stopping the optimization.")
    return stop
  }
}

object MostlyGoodSchedsLeft {
  private val myLogger : Logger = Logger.getLogger("")
}

/**
  * This criterion expects that the schedules in the population have been classified during evaluation. It checks,
  * whether at least 95% of the schedules have been classified as good.
  */
class MostlyGoodSchedsLeft(conf : ConfigGA) extends GATerminationCriterion {

  override def decide(nextGeneration : Int, population : HashMap[Schedule, Fitness]) : Boolean = {
    val threshold : Double = 0.95
    val fitnessVals : Iterable[Fitness] = population.values
    val nGood : Int = fitnessVals.count { f =>
      f.getPrediction.isDefined && f.getPrediction.get.pClass.isDefined &&
        f.getPrediction.get.pClass.get == Prediction.PerfClass.GOOD
    }
    val shareGood = nGood.toDouble / fitnessVals.size.toDouble
    MostlyGoodSchedsLeft.myLogger.info(f"${shareGood * 100} percent of schedules are classified as good.")
    return shareGood >= threshold
  }
}

object Convergence {
  val myLogger : Logger = Logger.getLogger("")
}

class Convergence(conf : ConfigGA) extends GATerminationCriterion {
  //  val generationsOpt

  private var haveSeenFirstGen : Boolean = false
  private var prevGenerationsOpt : List[Double] = List.empty

  override def decide(nextGeneration : Int, population : HashMap[Schedule, Fitness]) : Boolean = {
    if (nextGeneration == 0) {
      haveSeenFirstGen = true
      return false
    }
    if (!haveSeenFirstGen)
      throw new IllegalStateException("This termination criteration cannot be used of the genetic algorithm hasn't started from the first generation.")

    if ((conf.evaluationStrategy != MinimalConfig.EvaluationStrategy.CPU && conf.evaluationStrategy != MinimalConfig.EvaluationStrategy.CLASSIFIER_AND_CPU) || !(conf.measureParExecTime || conf.measureSeqExecTime))
      throw new IllegalStateException("This termination criterion works with execution time measurement as the fitness evaluation strategy.")

    val execTimes : List[Double] = population.values.toList.filter(_.getEvalResult.isDefined).map(_.getEvalResult.get).filter(_.completelyEvaluated).map((r : EvalResult) => {
      if (conf.measureParExecTime)
        r.getNormalizedExecTimePar
      else
        r.getNormalizedExecTimeSeq
    })
    if (execTimes.isEmpty)
      throw new IllegalStateException("The current generation does not contain succesfully benchmarked schedules.")
    prevGenerationsOpt ::= execTimes.min
    if (prevGenerationsOpt.length < conf.convergenceTerminationCriterionWindowSize.get + 1)
      return false
    val window : List[Double] = prevGenerationsOpt.take(conf.convergenceTerminationCriterionWindowSize.get)
    val windowMoved : List[Double] = prevGenerationsOpt.tail.take(conf.convergenceTerminationCriterionWindowSize.get)
    val maxDiff : Double = windowMoved.zip(window).map(t => (t._1 - t._2) / t._1).max
    return maxDiff < conf.convergenceTerminationCriterionThreshold.get
  }
}