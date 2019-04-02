package polyite

import java.io.File
import java.util.logging.FileHandler
import java.util.logging.Logger
import java.util.logging.SimpleFormatter

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.util.Random

import polyite.config.Config
import polyite.config.ConfigGA
import polyite.config.ConfigRand
import polyite.config.MinimalConfig
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.evolution.FixedNIterations
import polyite.evolution.GATerminationCriterion
import polyite.evolution.MostlyGoodSchedsLeft
import polyite.evolution.SelectionStrategy
import polyite.evolution.TakeKBestSchedules
import polyite.evolution.TakeKBestUseFitness
import polyite.evolution.migration.MigrationStrategy
import polyite.evolution.migration.MigrationStrategyFactory
import polyite.export.ExportStrategy
import polyite.export.JSCOPInterface
import polyite.export.ScheduleExport
import polyite.export.ScheduleExport.StorageException
import polyite.fitness.Feature
import polyite.pmpi.NoMPI
import polyite.pmpi.OpenMPI
import polyite.sched_eval.AbstractFitnessEvaluation
import polyite.sched_eval.EvalSchedTreeGenerationDuration
import polyite.sched_eval.EvaluationStrategyFactory
import polyite.sched_eval.Fitness
import polyite.sched_eval.FitnessUnknown
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.ScheduleUtils
import polyite.schedule.hash.ScheduleHash
import polyite.schedule.hash.ScheduleHashFunctionFactory
import polyite.schedule.sampling.SamplingStrategy
import polyite.schedule.sampling.SamplingStrategyFactory
import polyite.util.Timer
import polyite.util.Util
import polyite.evolution.Convergence

object MainUtil {

  val myLogger : Logger = Logger.getLogger("")

  /**
    * Checks whether args contains at least two arguments and prints a warning if
    * this is not the case.
    */
  def checkNumArgs(args : Array[String]) : Boolean = {
    if (args.length < 2) {
      myLogger.warning("too view arguments. <jscop file path> <config file path>")
      return false
    }
    return true
  }

  /**
    * Loads the scop from the file named by the 2nd argument in args.
    */
  def loadScop(jscopFilePath : String) : Option[(File, ScopInfo)] = {
    val jscopFile : File = new File(jscopFilePath)
    if (!(jscopFile.exists() && jscopFile.canRead())) {
      myLogger.warning("Cannot read from " + jscopFilePath)
      return None
    }
    myLogger.info("Loading SCoP from " + jscopFilePath)
    val scopMaybe = JSCOPInterface.readJSCOP(jscopFilePath)

    scopMaybe match {
      case None => {
        myLogger.warning("Failed to load SCoP")
        return None
      }
      case Some(s) => {
        myLogger.info("Imported SCoP:\n" + s)
        return Some((jscopFile, s))
      }
    }
  }

  def loadConfig[T <: Config](configFilePath : String, parseConfig : File => Option[T]) : Option[T] = {
    val configFile : File = new File(configFilePath)
    if (!configFile.exists() || !configFile.canRead()) {
      myLogger.warning("Cannot find the configuration file.")
      return None
    }

    val confMaybe : Option[T] = parseConfig(configFile)
    if (!confMaybe.isDefined) {
      myLogger.warning("Failed to load the configuration.")
      return None
    }
    return confMaybe
  }

  /**
    * Adds a file sink to the given logger according to the configuration config.
    */
  def configureLogger(l : Logger, conf : Config, makeLogFileDistinct : String => String = s => s) {
    if (conf.logToFile) {
      var logFile : File = new File(makeLogFileDistinct(conf.logFile.getAbsolutePath))
      var logFileIdx : Int = 1
      while (logFile.exists()) {
        logFile = new File(makeLogFileDistinct(conf.logFile.getAbsolutePath) + "." + logFileIdx)
        logFileIdx += 1
      }
      val fHandler : FileHandler = new FileHandler(logFile.getAbsolutePath)
      fHandler.setFormatter(new SimpleFormatter)
      myLogger.addHandler(fHandler)
    }
  }

  /**
    * Constructs a list of GA population exporters according to configuration
    * config.
    */
  def buildPopulationExportSinks(conf : Config, scop : ScopInfo, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], jscopFile : File, makeFileDistinct : String => String = s => s) : List[ExportStrategy] = {
    var populationExportSinks : List[ExportStrategy] = List.empty
    def append(f : (Iterable[(Schedule, Fitness)], List[Feature], Int) => Unit) {
      populationExportSinks ::= new ExportStrategy() {
        override def export(population : Iterable[(Schedule, Fitness)], activeFeatures : List[Feature], generation : Int) {
          f(population, activeFeatures, generation)
        }
      }
    }
    append(ScheduleExport.exportPopulationToFile(conf, makeFileDistinct))
    if (conf.exportSchedulesToJSCOPFiles)
      append(ScheduleExport.exportPopulationToJSCOPFiles(
        conf, scop, domInfo, deps, jscopFile.getName, makeFileDistinct))
    if (conf.exportPopulationToCSV)
      append(ScheduleExport.exportPopulationToCSV(conf, makeFileDistinct))
    return populationExportSinks
  }

  /**
    * Checks whether a specific isl function is available. Otherwise an exception is thrown.
    */
  def verifyIslVersion() {
    isl.Isl.ctx.getRemainingOperations()
  }

  /**
    * High level execution of the genetic algorithm for schedule optimization.
    *
    * @param scheduleSelectionStrategy Strategy that selects schedules that form
    * the basis for constructing the next generation of schedules from the
    * current generation.
    * @param args program arguments as provided to main. The first parameter is
    * the path to the JSCOP file while the second parameter points to the
    * configuration file.
    */
  def runGA(args : Array[String]) : Unit = {

    verifyIslVersion()

    if (!MainUtil.checkNumArgs(args))
      return
    val (jscopFile : File, scop : ScopInfo) = MainUtil.loadScop(args(0)) match {
      case None    => return
      case Some(t) => t
    }
    val conf : ConfigGA = MainUtil.loadConfig(args(1), ConfigGA.loadAndValidateConfig) match {
      case None    => return
      case Some(c) => c
    }

    val mpi = if (conf.executionMode == ConfigGA.ExecutionMode.MPI) OpenMPI else NoMPI;
    mpi.Init(args)

    // set the random seed if asked to
    if (conf.seed.isDefined)
      Random.setSeed(conf.seed.get)

    val makeFileDistinct : String => String = if (conf.executionMode == ConfigGA.ExecutionMode.SINGLE_PROCESS)
      s => s // Nothing to do in case of single process. Therefore, use the identity.
    else
      s => s + ".mpi" + mpi.rank() + "."
    MainUtil.configureLogger(myLogger, conf, makeFileDistinct)
    myLogger.info("Configuration:\n" + conf.toString())
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils
      .calcDepsAndDomInfo(scop)

    myLogger.info("dependences: \n" + deps.mkString("\n"))
    myLogger.info("number of dependences: " + deps.size)
    myLogger.info("machine: " + java.net.InetAddress.getLocalHost().getHostName())

    val populationExportSinks : List[ExportStrategy] = MainUtil.buildPopulationExportSinks(conf, scop, domInfo, deps, jscopFile, makeFileDistinct)

    var initialPopulation : HashMap[Schedule, Fitness] = null
    val populationFile = ScheduleExport.buildPopulationFilePath(
      conf.populationFilePrefix,
      conf.currentGeneration)

    val sampler : SamplingStrategy = SamplingStrategyFactory.createSamplingStrategy(conf)
    val hashSchedules : Schedule => ScheduleHash = ScheduleHashFunctionFactory.createHashFunction(conf, scop)

    val isImportedPopulation = populationFile.exists()

    if (populationFile.exists()) {
      if (Util.checkFileExistsAndHasRequiredPermissions(
        true, true, false, false, populationFile)) {
        myLogger.info("Importing the existing poplulation.")
        val populationMaybe : Option[HashMap[Schedule, Fitness]] = ScheduleExport
          .loadPopulationFromFileNoFilter(conf, domInfo, deps, conf.currentGeneration)
        populationMaybe match {
          case None    => return
          case Some(p) => initialPopulation = p
        }
      } else {
        mpi.Finalize()
        return
      }
    } else {
      if (conf.currentGeneration == 0) {
        Timer.startTimer("")
        initialPopulation = HashMap.empty
        ScheduleUtils.genRandSchedules(domInfo, deps, conf.regularPopulationSize, conf.initPopulationNumRays,
          conf.initPopulationNumLines, conf, sampler, hashSchedules) map { s =>
            initialPopulation.put(s, FitnessUnknown)
          }
      } else {
        myLogger.warning("Starting from generation " + conf.currentGeneration
          + ". Expected file " + populationFile + " to exist. Terminating.")
        mpi.Finalize()
        return
      }
    }

    if (Timer.exists(""))
      Timer.stopTimer("")

    val makeTmpWorkingDirDistinct : String => String = if (conf.executionMode == ConfigGA.ExecutionMode.SINGLE_PROCESS)
      s => s // Nothing to do in case of single process. Therefore, use the identity.
    else
      s => s + "mpi" + mpi.rank()

    val evalStrategy : AbstractFitnessEvaluation = EvaluationStrategyFactory.createEvaluationStrategy(conf, scop,
      domInfo, deps, makeTmpWorkingDirDistinct)

    val schedSelectionStrategy : SelectionStrategy = if (conf.evaluationStrategy == MinimalConfig.EvaluationStrategy.CLASSIFIER) {
      new TakeKBestUseFitness(conf)
    } else {
      new TakeKBestSchedules(conf)
    }

    val migrationStrategy : Option[MigrationStrategy] = MigrationStrategyFactory.createMigrationStrategy(mpi, conf)

    val terminationStrategy : GATerminationCriterion = if (conf.evaluationStrategy == MinimalConfig.EvaluationStrategy.CLASSIFIER)
      GATerminationCriterion.union(new MostlyGoodSchedsLeft(conf), new FixedNIterations(conf))
    else if ((conf.evaluationStrategy == MinimalConfig.EvaluationStrategy.CPU ||
      conf.evaluationStrategy == MinimalConfig.EvaluationStrategy.CLASSIFIER_AND_CPU) &&
      conf.gaCpuTerminationCriterion.get == ConfigGA.GACpuTerminationCriteria.CONVERGENCE)
      GATerminationCriterion.union(new FixedNIterations(conf), new Convergence(conf))
    else
      new FixedNIterations(conf)

    if (Timer.exists(""))
      Timer.restartTimer("")
    else
      Timer.startTimer("")

    try {
      GeneticOptimization.optimize(evalStrategy, initialPopulation, conf, scop, populationExportSinks,
        isImportedPopulation, domInfo, deps, schedSelectionStrategy, migrationStrategy, terminationStrategy, sampler, mpi, hashSchedules)
    } catch {
      case e : StorageException => {
        myLogger.severe("Failed to store the population to file: " + e.getMessage)
        mpi.Finalize()
        return
      }
    }
    myLogger.info(f"Cache hit rate of the evaluation results cache: ${evalStrategy.getEvalResultsCacheHitRate().getOrElse("NaN").toString()}")
    val executionDuration : Long = Timer.removeTimer("") / 1000
    myLogger.info(f"Optimization took ${executionDuration} seconds.")
    mpi.Finalize();
  }

  /**
    * High level implementation of a random schedule space exploration.
    *
    * @param T type of the configuration
    * @param args program arguments as provided to main. The first parameter is
    * the path to the JSCOP file while the second parameter points to the
    * configuration file.
    * @param parseConfig function that parses the configuration file into
    * configuration of type {@code T}.
    * @param buildRandSchedGen factory that produces the function for random
    * schedule generation. Accepts the loaded SCoP as its parameter.
    */
  def runRandExpl[T <: ConfigRand](
    args : Array[String],
    parseConfig : File => Option[T], buildRandSchedGen : ScopInfo => ((DomainCoeffInfo, Set[Dependence], Int, Set[Schedule], NumGeneratorsLimit, NumGeneratorsLimit, T, SamplingStrategy, Schedule => ScheduleHash) => Set[Schedule])) : Unit = {

    verifyIslVersion()

    if (!MainUtil.checkNumArgs(args))
      return

    // Load the SCoP
    val (jscopFile : File, scop : ScopInfo) = MainUtil.loadScop(args(0)) match {
      case None    => return
      case Some(t) => t
    }

    // Load the configuration
    val conf : T = loadConfig(args(1), parseConfig) match {
      case None    => return
      case Some(c) => c
    }

    // set the random seed if asked to
    if (conf.seed.isDefined)
      Random.setSeed(conf.seed.get)

    // configure the logger
    MainUtil.configureLogger(myLogger, conf)
    myLogger.info("Configuration:\n" + conf.toString())
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils
      .calcDepsAndDomInfo(scop)

    myLogger.info("dependences: \n" + deps.mkString("\n"))
    myLogger.info("number of dependences: " + deps.size)

    val populationExportSinks : List[ExportStrategy] = MainUtil.buildPopulationExportSinks(conf, scop, domInfo, deps, jscopFile)
    val genRandSchedules : (DomainCoeffInfo, Set[Dependence], Int, Set[Schedule], NumGeneratorsLimit, NumGeneratorsLimit, T, SamplingStrategy, Schedule => ScheduleHash) => Set[Schedule] = buildRandSchedGen(scop)
    val hashSchedule : Schedule => ScheduleHash = ScheduleHashFunctionFactory.createHashFunction(conf, scop)

    /*
     * 1. Optionally load already generated schedules from file (the file name is
     *      implicitly deduced from the configuration).
     * 2. Separate schedules that still need evaluation from already evaluated
     *      schedules.
     */
    var evaluatedScheds : HashMap[Schedule, Fitness] = HashMap.empty
    var scheds2Eval : HashSet[Schedule] = HashSet.empty
    var allScheds : Set[Schedule] = Set.empty

    if (conf.importScheds) {
      myLogger.info("Importing schedules from file.")
      val importedScheds : HashMap[Schedule, Fitness] = ScheduleExport
        .loadPopulationFromFileNoFilter(conf, domInfo, deps, 0) match {
          case None => {
            myLogger.warning("Failed to import schedule from file. Terminating.")
            return
          }
          case Some(importedScheds) => importedScheds
        }
      myLogger.info("Imported " + importedScheds.size + " schedules.")
      if (conf.filterImportedPopulation) {
        myLogger.info("Schedules that could not be evaluated previously will be omitted this time.")
        evaluatedScheds = importedScheds
      } else {
        def constraint(fit : Fitness) : Boolean = if (conf.evaluationStrategy == MinimalConfig.EvaluationStrategy.CLASSIFIER) {
          !fit.getPrediction.isDefined || !fit.getPrediction.get.pClass.isDefined
        } else {
          !fit.getEvalResult.isDefined || !fit.getEvalResult.get.completelyEvaluated
        }
        importedScheds.filterNot(x => constraint(x._2)).map(t => evaluatedScheds.put(t._1, t._2))
        importedScheds.filter(x => constraint(x._2)).map(t => scheds2Eval.add(t._1))
        myLogger.info(scheds2Eval.size + " of the imported schedules must be evaluated.")
      }
      allScheds = importedScheds.keySet.toSet
    }

    // Extend the number of random schedules to the configured maximum.
    myLogger.info(f"Generating ${conf.numScheds - allScheds.size} random schedules.")
    Timer.startTimer("")

    val sampler : SamplingStrategy = SamplingStrategyFactory.createSamplingStrategy(conf)

    allScheds = genRandSchedules(domInfo, deps, conf.numScheds, allScheds, conf.maxNumRays, conf.maxNumLines, conf, sampler, hashSchedule)
    allScheds.filterNot(evaluatedScheds.keySet.contains).map(scheds2Eval.add)

    if (conf.evaluateScheds) {
      if (conf.numSchedTreeSimplDurationMeasurements.isDefined) {
        val nMeasurements : Int = conf.numSchedTreeSimplDurationMeasurements.get
        myLogger.info(f"Evaluating the duration of schedule tree simplification ${nMeasurements} times for each schedule.")
        val evalResult : Map[Schedule, Fitness] = EvalSchedTreeGenerationDuration.eval(scheds2Eval, conf, nMeasurements, scop)
        evaluatedScheds = evaluatedScheds ++ evalResult
      } else {
        // Evaluate the schedules.
        Timer.stopTimer("")
        val schedEvalStrategy : AbstractFitnessEvaluation = EvaluationStrategyFactory.createEvaluationStrategy(conf, scop, domInfo, deps, s => s)
        myLogger.info("Evaluating each schedule.")
        Timer.restartTimer("")
        val (evalResult : HashMap[Schedule, Fitness], evalSuccess : Boolean) =
          schedEvalStrategy.evaluateSchedules(scheds2Eval)
        evaluatedScheds = evaluatedScheds ++ evalResult
        myLogger.info(f"Cache hit rate of the evaluation results cache: ${schedEvalStrategy.getEvalResultsCacheHitRate().getOrElse("NaN").toString()}")
      }
    } else {
      myLogger.info(scheds2Eval.size + " schedules require evaluation but schedule evaluation is disabled.")
      val unevaluated : HashMap[Schedule, Fitness] = HashMap.empty
      scheds2Eval.foreach { (s : Schedule) => unevaluated.put(s, FitnessUnknown) }
      evaluatedScheds = evaluatedScheds ++ unevaluated
    }

    val executionDuration : Long = Timer.removeTimer("") / 1000

    myLogger.info("Storing the evaluated schedules.")
    populationExportSinks.map(_.export(evaluatedScheds, Feature.features, 0))
    myLogger.info(f"Optimization took ${executionDuration} seconds.")
  }
}
