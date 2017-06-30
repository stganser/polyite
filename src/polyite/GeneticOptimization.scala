package polyite

import java.io.IOException
import java.util.logging.Logger

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.forkjoin.ForkJoinPool
import scala.math.BigInt.int2bigInt
import scala.util.Random

import polyite.config.ConfigGA
import polyite.evolution.GeneticOperatorFactory
import polyite.evolution.GeneticOperatorFactory.GeneticOperators
import polyite.sched_eval.EvalResult
import polyite.sched_eval.ScheduleEvaluation
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.schedule.ScheduleUtils
import polyite.util.Rat
import polyite.util.Util

/**
  * Implements a genetic algorithm for schedule optimization in the polyhedral
  * model.
  */
object GeneticOptimization {
  val myLogger : Logger = Logger.getLogger("")

  /**
    * This function implements the actual genetic algorithm for schedule
    * optimization. The number of generations after which the optimization stops
    * is determined primarily by {@code conf.maxGenerationToReach}. The genetic
    * algorithm stops ahead of time, if it is unable to select any schedules from
    * the current generation that could form a basis for the next generation or
    * if the genetic operators (i.e. mutation and crossover operators) are unable
    * to produce new schedules. The algorithm is parameterizable through
    * {@code conf}.
    *
    * @param evaluateSchedules function that determines the fitness of the given
    * schedules. The function must return a tuple containing the evaluated
    * schedules together with the evaluation results and a Bool that is true iff
    * the overall evaluation was successful.
    * @param population schedules to start from.
    * @param storageSinks list of function that can be used to store each
    * generation for evaluation after it has been evaluated. In case of a problem
    * this function stores the current generation of schedules before returning.
    * @param isImportedPopulation tells this function whether {@code population}
    * contains schedules that have been imported from file of if {@code population}
    * is a randomly generated set of schedules.
    * @param schedSelectionStrategy function to select the schedules that form
    * the basis for the next generation of schedules.
    *
    * @see polyite.evolution.SelectionStrategies
    */
  def optimize(evaluateSchedules : HashSet[Schedule] => (HashMap[Schedule, EvalResult], Boolean),
    population : HashMap[Schedule, EvalResult], conf : ConfigGA, scop : ScopInfo,
    storageSinks : Iterable[(Iterable[(Schedule, EvalResult)], Int) => Unit],
    isImportedPopulation : Boolean, domInfo : DomainCoeffInfo, deps : Set[Dependence],
    schedSelectionStrategy : (Int, HashMap[Schedule, EvalResult], Int) => Array[(Schedule, EvalResult)]) {
    var currentGeneration : Int = conf.currentGeneration
    var currentPopulation : HashMap[Schedule, EvalResult] = HashMap.empty
    population.map(t =>
      {
        if (!isImportedPopulation || (!conf.filterImportedPopulation || t._2.completelyEvaluated))
          currentPopulation.put(t._1, t._2)
      })

    while (currentGeneration <= conf.maxGenerationToReach) {
      myLogger.info("The current population contains " + currentPopulation.size
        + " schedules.")
      myLogger.info("Evaluating generation " + currentGeneration)
      val (alreadyEvaluated : HashMap[Schedule, EvalResult],
        scheds2Eval : HashSet[Schedule]) = ScheduleEvaluation
        .classifyForEvaluation(currentPopulation)
      myLogger.info(scheds2Eval.size + " schedules must be evaluated.")
      if (!scheds2Eval.isEmpty) {
        val (evaluatedScheds : HashMap[Schedule, EvalResult],
          evaluationSuccessful : Boolean) = evaluateSchedules(scheds2Eval)
        currentPopulation = alreadyEvaluated ++ evaluatedScheds
        if (!evaluationSuccessful) {
          myLogger.warning("Evaluation failed.")
        }
        myLogger.info("Writing the current population to file.")
        try {
          storageSinks.map(_(currentPopulation, currentGeneration))
        } catch {
          case e : IOException => {
            myLogger.info("Failed to store the current population. Terminating.")
            return
          }
        }
        if (!evaluationSuccessful) {
          myLogger.warning("Terminating.")
          return
        }
      }
      if (currentGeneration < conf.maxGenerationToReach) {
        val successfullyMeasured : HashMap[Schedule, EvalResult] = HashMap.empty
        for ((sched, res) <- currentPopulation) {
          if (res.completelyEvaluated)
            successfullyMeasured.put(sched, res)
        }
        if (successfullyMeasured.isEmpty) {
          myLogger.warning("None of the schedules could be benchmarked"
            + "successfully. Terminating.")
          return
        }
        myLogger.info("Selecting the best schedules.")
        myLogger.info("Building generation " + (currentGeneration + 1) + ".")
        val maxNumScheds2Keep : Int =
          if (conf.filterImportedPopulation && currentGeneration == conf.currentGeneration) {
            (conf.fractionOfSchedules2Keep * Rat(population.size)).intCeil.toInt
          } else {
            (conf.fractionOfSchedules2Keep * Rat(currentPopulation.size)).intCeil.toInt
          }
        val nextGenerationBasis : Array[(Schedule, EvalResult)] = schedSelectionStrategy(
          currentGeneration + 1, successfullyMeasured, maxNumScheds2Keep)
        myLogger.info("The basis for the next generation contains "
          + nextGenerationBasis.size + " schedules.")
        if (nextGenerationBasis.size < maxNumScheds2Keep)
          myLogger.warning("The basis for the next generation will contain less "
            + "schedules than planned: " + nextGenerationBasis.size
            + " instead of " + maxNumScheds2Keep)
        currentPopulation = completePopulation(nextGenerationBasis,
          currentGeneration + 1, conf, domInfo, deps)
        val popSize : Int = currentPopulation.size
        if (popSize < conf.regularPopulationSize)
          myLogger.warning("The population size could not be increased up to "
            + "the regular size: " + popSize + " < " + conf.regularPopulationSize)

        if (popSize == nextGenerationBasis.size) {
          myLogger.warning("Failed to produce new schedules. Terminating.")
          return
        }
      }
      currentGeneration += 1
    }
    if (currentGeneration > conf.maxGenerationToReach) {
      println("Reached the configured maximum generation. Stopping the optimization.")
    }
  }

  private def completePopulation(basis : Array[(Schedule, EvalResult)],
    currentGeneration : Int, conf : ConfigGA, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : HashMap[Schedule, EvalResult] = {
    val timeout = conf.evolutionTimeout * 1000
    val fullPopulation : HashMap[Schedule, EvalResult] = HashMap.empty
    basis.map(t => fullPopulation.put(t._1, t._2))

    val numRandScheds = math.min((Rat(conf.regularPopulationSize)
      * conf.shareOfRandSchedsInPopulation).intFloor.toInt,
      conf.regularPopulationSize - fullPopulation.size)
    ScheduleUtils.genRandSchedules(domInfo, deps, numRandScheds, conf.maxNumRays, conf.maxNumLines, conf)
      .map(fullPopulation.put(_, EvalResult.notEvaluated))

    def genScheds(idx : Int)(x : Unit) {
      val logPrefix : String = "(mutation worker #" + idx + ") "
      var numFailures : Int = 0

      def continue : Boolean = {
        fullPopulation.synchronized {
          var cancel : Boolean = false
          if (numFailures >= conf.genSchedsMaxAllowedConseqFailures)
            cancel = true
          return !cancel && fullPopulation.size < conf.regularPopulationSize
        }
      }
      while (continue) {
        var addedAny : Boolean = false

        // Apply crossover
        if ((basis.size > 1) && Random.nextBoolean() && !conf.activeCrossovers.isEmpty) {
          val crossoverOp : GeneticOperators.Value = conf.activeCrossovers(Random.nextInt(conf.activeCrossovers.size))
          myLogger.info(logPrefix + "Applying " + crossoverOp)
          val crossoverFunc : ((Schedule, Schedule) => HashSet[Schedule]) = GeneticOperatorFactory
            .createCrossover(crossoverOp, conf)
          val newScheds : HashSet[Schedule] = applyCrossover(crossoverFunc, basis, timeout, logPrefix)
          val newSchedsIter : Iterator[Schedule] = newScheds.iterator

          fullPopulation.synchronized {
            while (newSchedsIter.hasNext && fullPopulation.size < conf.regularPopulationSize) {
              val s : Schedule = newSchedsIter.next()
              if (!fullPopulation.contains(s)) {
                ScheduleUtils.assertValid(s)
                fullPopulation.put(s, EvalResult.notEvaluated)
                myLogger.info(logPrefix + "new schedule " + s)
                addedAny = true
              }
            }
          }

          // Apply mutation
        } else if (!conf.activeMutators.isEmpty) {
          val mutationOp : GeneticOperators.Value = conf.activeMutators(Random.nextInt(conf.activeMutators.size))
          myLogger.info(logPrefix + "Applying " + mutationOp)
          val mutationFunc : Schedule => Option[Schedule] = GeneticOperatorFactory.createMutator(mutationOp, conf,
            currentGeneration)
          val newSched = applyMutation(mutationFunc, basis, timeout, logPrefix)
          fullPopulation.synchronized {
            if (newSched.isDefined && fullPopulation.size < conf.regularPopulationSize
              && !fullPopulation.contains(newSched.get)) {
              ScheduleUtils.assertValid(newSched.get)
              fullPopulation.put(newSched.get, EvalResult.notEvaluated)
              myLogger.info(logPrefix + "new schedule " + newSched.get)
              addedAny = true
            }
          }
        }
        if (addedAny)
          numFailures = 0
        else
          numFailures += 1

        if (idx == 0) {
          System.gc()
          System.gc()
          System.gc()
        }
      }
    }
    val schedGenWorkers : ParArray[Unit => Unit] = new ParArray(conf.numScheduleGenThreads)
    for (i <- 0 until schedGenWorkers.length)
      schedGenWorkers(i) = genScheds(i)
    schedGenWorkers.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(conf.numScheduleGenThreads))
    schedGenWorkers.map(f => f(()))
    return fullPopulation
  }

  private def applyCrossover(f : (Schedule, Schedule) => HashSet[Schedule],
    basis : Array[(Schedule, EvalResult)], timeout : Long, logPrefix : String) : HashSet[Schedule] = {
    var sched1 : Schedule = null
    var sched2 : Schedule = null
    do {
      if (basis.size == 2) {
        sched1 = basis(0)._1
        sched2 = basis(1)._1
      } else {
        sched1 = basis(Random.nextInt(basis.size))._1
        sched2 = basis(Random.nextInt(basis.size))._1
      }
    } while (sched1 == sched2)
    myLogger.info(logPrefix + "Crossover of " + sched1 + " and " + sched2)
    val newSchedsMaybe : Option[HashSet[Schedule]] =
      Util.runWithTimeout((sched1, sched2), ((t : (Schedule, Schedule)) =>
        f(t._1, t._2)), timeout)
    newSchedsMaybe match {
      case Some(newScheds) => return newScheds
      case None => {
        myLogger.warning(logPrefix + "A timeout occurred during schedule crossover.")
        return HashSet.empty
      }
    }
  }

  private def applyMutation(f : Schedule => Option[Schedule],
    basis : Array[(Schedule, EvalResult)], timeout : Long, logPrefix : String) : Option[Schedule] = {
    val oldSched : Schedule = basis(Random.nextInt(basis.size))._1
    myLogger.info(logPrefix + "Mutation of " + oldSched)
    val newSchedMaybe : Option[Option[Schedule]] =
      Util.runWithTimeout(oldSched, f, timeout)
    newSchedMaybe match {
      case Some(newSched) => return newSched
      case None => {
        myLogger.warning(logPrefix + "A timeout occurred during schedule mutation.")
        return None
      }
    }
  }
}