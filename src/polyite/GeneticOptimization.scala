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
import polyite.config.MinimalConfig
import polyite.evolution.GeneticOperatorFactory
import polyite.evolution.GeneticOperatorFactory.GeneticOperators
import polyite.fitness.Feature
import polyite.sched_eval.AbstractFitnessEvaluation
import polyite.sched_eval.Fitness
import polyite.sched_eval.FitnessUnknown
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.sampling.SamplingStrategy
import polyite.schedule.Schedule
import polyite.schedule.ScheduleUtils
import polyite.util.Rat
import polyite.util.Timer
import polyite.util.Util
import polyite.pmpi._
import polyite.schedule.hash.ScheduleHash
import polyite.export.ExportStrategy
import polyite.evolution.SelectionStrategy
import polyite.config.ConfigGA.MigrationStrategy
import polyite.evolution.migration.MigrationStrategy
import polyite.evolution.GATerminationCriterion

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
    * @param storageSinks list of strategies that can be used to store each
    * generation for evaluation after it has been evaluated. In case of a problem
    * these strategies store the current generation of schedules before returning.
    * @param isImportedPopulation tells this function whether {@code population}
    * contains schedules that have been imported from file of if {@code population}
    * is a randomly generated set of schedules.
    * @param schedSelectionStrategy function to select the schedules that form
    * the basis for the next generation of schedules.
    * @param classifyForEvaluation function that classifies schedules into schedules that need evaluation and schedules
    * that have already been evaluated depending on their evaluation results.
    * @param migrationStrategy strategy for the migration of schedules between the processes of the distributed genetic
    * algorithm. {@code conf} determines whether the distributed or the sequential genetic algorithm will run. In case
    * of the sequential genetic algorithm, this parameter may be {@code None}.
    * @param conf configuration properties
    * @see polyite.evolution.SelectionStrategies
    */
  def optimize(schedEvaluator : AbstractFitnessEvaluation, population : HashMap[Schedule, Fitness], conf : ConfigGA,
    scop : ScopInfo, storageSinks : Iterable[ExportStrategy],
    isImportedPopulation : Boolean, domInfo : DomainCoeffInfo, deps : Set[Dependence],
    schedSelectionStrategy : SelectionStrategy,
    migrationStrategy : Option[MigrationStrategy],
    terminationCriterion : GATerminationCriterion, sampler : SamplingStrategy, mpi : IMPI, hashScheds : Schedule => ScheduleHash) {
    var currentGeneration : Int = conf.currentGeneration
    var currentPopulation : HashMap[Schedule, Fitness] = HashMap.empty
    population.map(t =>
      {
        if (!isImportedPopulation || (!conf.filterImportedPopulation || t._2.isCompletelyEvaluated))
          currentPopulation.put(t._1, t._2)
      })

    var terminate : Boolean = terminationCriterion.decide(currentGeneration, currentPopulation)

    while (!terminate) {
      myLogger.info("The current population contains " + currentPopulation.size
        + " schedules.")
      myLogger.info("Evaluating generation " + currentGeneration)
      val (alreadyEvaluated : HashMap[Schedule, Fitness],
        scheds2Eval : HashSet[Schedule]) = schedEvaluator.classifyForEvaluation(currentPopulation).getOrElse({
        myLogger.warning("Failed to select the schedules that require fitness evaluation. Terminating.")
        return
      })
      myLogger.info(scheds2Eval.size + " schedules must be evaluated.")
      if (!scheds2Eval.isEmpty) {
        val (evaluatedScheds : HashMap[Schedule, Fitness],
          evaluationSuccessful : Boolean) = schedEvaluator.evaluateSchedules(scheds2Eval)
        currentPopulation = alreadyEvaluated ++ evaluatedScheds
        if (!evaluationSuccessful) {
          myLogger.warning("Evaluation failed.")
        }
        myLogger.info("Writing the current population to file.")
        Timer.stopTimer("")
        try {
          val features = if (conf.evaluationStrategy == MinimalConfig.EvaluationStrategy.CLASSIFIER)
            Feature.features
          else
            List.empty
          storageSinks.map(_.export(currentPopulation, features, currentGeneration))
        } catch {
          case e : IOException => {
            myLogger.info("Failed to store the current population. Terminating.")
            mpi.abortAllAndTerminate("Failed to store the current population.")
            return
          }
        }
        Timer.restartTimer("")
        if (!evaluationSuccessful) {
          myLogger.warning("Terminating.")
          mpi.abortAllAndTerminate("Schedule evaluation failed.")
          return
        }
      }
      terminate = terminationCriterion.decide(currentGeneration + 1, currentPopulation)
      if (!terminate) {
        val successfullyMeasured : HashMap[Schedule, Fitness] = HashMap.empty
        for ((sched, res) <- currentPopulation) {
          if (res.isCompletelyEvaluated)
            successfullyMeasured.put(sched, res)
        }
        if (successfullyMeasured.isEmpty) {
          myLogger.warning("None of the schedules could be benchmarked"
            + "successfully. Terminating.")
          mpi.abortAllAndTerminate("Benchmarking has failed.")
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
        val nextGenerationBasis : Array[(Schedule, Fitness)] = schedSelectionStrategy.select(
          currentGeneration + 1, successfullyMeasured, maxNumScheds2Keep)
        myLogger.info("The basis for the next generation contains "
          + nextGenerationBasis.size + " schedules.")
        if (nextGenerationBasis.size < maxNumScheds2Keep)
          myLogger.warning("The basis for the next generation will contain less "
            + "schedules than planned: " + nextGenerationBasis.size
            + " instead of " + maxNumScheds2Keep)
        currentPopulation = completePopulation(nextGenerationBasis, currentGeneration + 1, conf, scop, domInfo, deps,
          schedEvaluator.selectForMutation, migrationStrategy, sampler, hashScheds)
        val popSize : Int = currentPopulation.size
        if (popSize < conf.regularPopulationSize)
          myLogger.warning("The population size could not be increased up to "
            + "the regular size: " + popSize + " < " + conf.regularPopulationSize)

        if (popSize == nextGenerationBasis.size) {
          myLogger.warning("Failed to produce new schedules. Terminating.")
          mpi.abortAllAndTerminate("Failed to produce an offspring population.")
          return
        }
      }
      currentGeneration += 1
    }

    myLogger.info("Time for migration exchange in seconds: " + migrTime / 1000)
  }

  private var migrTime : Long = 0

  private def completePopulation(basis : Array[(Schedule, Fitness)], currentGeneration : Int, conf : ConfigGA, scop : ScopInfo,
    domInfo : DomainCoeffInfo, deps : Set[Dependence],
    selectionStrategy : Array[(Schedule, Fitness)] => Schedule,
    migrationStrategy : Option[MigrationStrategy],
    sampler : SamplingStrategy, hashScheds : Schedule => ScheduleHash) : HashMap[Schedule, Fitness] = {
    val timeout = conf.evolutionTimeout * 1000
    val fullPopulation : HashMap[ScheduleHash, (Schedule, Fitness)] = HashMap.empty
    basis.map(t => fullPopulation.put(hashScheds(t._1), (t._1, t._2)))

    val numRandScheds = math.min(
      (Rat(conf.regularPopulationSize)
        * conf.shareOfRandSchedsInPopulation).intFloor.toInt,
      conf.regularPopulationSize - fullPopulation.size)
    ScheduleUtils.genRandSchedules(domInfo, deps, numRandScheds, conf.maxNumRays, conf.maxNumLines, conf, sampler, hashScheds)
      .map((s : Schedule) => {
        val h : ScheduleHash = hashScheds(s)
        if (!fullPopulation.contains(h))
          fullPopulation.put(h, (s, FitnessUnknown))
      })

    /*
     * Migration of schedules by the distributed genetic algorithm.
     */
    if (conf.executionMode == ConfigGA.ExecutionMode.MPI && (conf.migrationRate.get == 0 || currentGeneration % conf.migrationRate.get == 0)) {
      val timeStart = System.currentTimeMillis()
      val migrated : List[(Schedule, Fitness)] = migrationStrategy.get.migrate(basis, domInfo, deps, currentGeneration)
      for ((sched : Schedule, fit : Fitness) <- migrated) {
        if (!fit.isCompletelyEvaluated) {
          myLogger.warning("Invaild fitness/schedule made it into full population.")
          val originalPopulation : HashMap[Schedule, Fitness] = HashMap.empty
          basis.foreach((t : (Schedule, Fitness)) => originalPopulation.put(t._1, t._2))
          return originalPopulation
        }

        val h : ScheduleHash = hashScheds(sched)
        if (!fullPopulation.contains(h)) {
          fullPopulation.put(h, (sched, fit))
          myLogger.info(f"inserted migrated schedule into the population: (${sched}, ${fit})")
        }
      }
      val timeTaken = (System.currentTimeMillis() - timeStart)
      migrTime += timeTaken
    }

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
            .createCrossover(crossoverOp, conf, scop, sampler)
          val newScheds : HashSet[Schedule] = applyCrossover(crossoverFunc, selectionStrategy, basis, timeout, logPrefix)
          val newSchedsIter : Iterator[Schedule] = newScheds.iterator

          fullPopulation.synchronized {
            while (newSchedsIter.hasNext && fullPopulation.size < conf.regularPopulationSize) {
              val s : Schedule = newSchedsIter.next()
              ScheduleUtils.assertValid(s)
              val h : ScheduleHash = hashScheds(s)
              if (!fullPopulation.contains(h)) {
                fullPopulation.put(h, (s, FitnessUnknown))
                myLogger.info(logPrefix + "new schedule " + s)
                addedAny = true
              }
            }
          }

          // Apply mutation
        } else if (!conf.activeMutators.isEmpty) {
          val mutationOp : GeneticOperators.Value = conf.activeMutators(Random.nextInt(conf.activeMutators.size))
          myLogger.info(logPrefix + "Applying " + mutationOp)
          val mutationFunc : Schedule => Option[Schedule] = GeneticOperatorFactory.createMutator(mutationOp, conf, scop,
            currentGeneration, sampler)
          val newSched = applyMutation(mutationFunc, selectionStrategy, basis, timeout, logPrefix)
          fullPopulation.synchronized {
            if (newSched.isDefined && fullPopulation.size < conf.regularPopulationSize) {
              ScheduleUtils.assertValid(newSched.get)
              val h : ScheduleHash = hashScheds(newSched.get)
              if (!fullPopulation.contains(h)) {
                fullPopulation.put(h, (newSched.get, FitnessUnknown))
                myLogger.info(logPrefix + "new schedule " + newSched.get)
                addedAny = true
              }
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
    val pool = new ForkJoinPool(conf.numScheduleGenThreads)
    schedGenWorkers.tasksupport = new ForkJoinTaskSupport(pool)
    schedGenWorkers.map(f => f(()))
    pool.shutdown()
    return fullPopulation.map(t => (t._2._1, t._2._2))
  }

  private def applyCrossover(f : (Schedule, Schedule) => HashSet[Schedule], selectionStrategy : Array[(Schedule, Fitness)] => Schedule,
    basis : Array[(Schedule, Fitness)], timeout : Long, logPrefix : String) : HashSet[Schedule] = {
    var sched1 : Schedule = null
    var sched2 : Schedule = null
    do {
      if (basis.size == 2) {
        sched1 = basis(0)._1
        sched2 = basis(1)._1
      } else {
        sched1 = selectionStrategy(basis)
        sched2 = selectionStrategy(basis)
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

  private def applyMutation(f : Schedule => Option[Schedule], selectionStrategy : Array[(Schedule, Fitness)] => Schedule,
    basis : Array[(Schedule, Fitness)], timeout : Long, logPrefix : String) : Option[Schedule] = {
    val oldSched : Schedule = selectionStrategy(basis)
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
