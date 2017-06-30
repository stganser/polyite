package polyite

import java.io.File
import java.util.logging.FileHandler
import java.util.logging.Logger
import java.util.logging.SimpleFormatter

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import polyite.config.Config
import polyite.config.MinimalConfig
import polyite.config.ConfigGA
import polyite.config.ConfigRand
import polyite.export.JSCOPInterface
import polyite.export.ScheduleExport
import polyite.export.ScheduleExport.StorageException
import polyite.sched_eval.EvalResult
import polyite.sched_eval.ScheduleEvaluation
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.ScheduleUtils
import polyite.util.Util
import polyite.config.MinimalConfig.NumGeneratorsLimit
import scala.util.Random
import polyite.sched_eval.EvalSchedTreeGenerationDuration

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
  def configureLogger(l : Logger, conf : Config) {
    if (conf.logToFile) {
      var logFile : File = conf.logFile
      var logFileIdx : Int = 1
      while (logFile.exists()) {
        logFile = new File(conf.logFile.getAbsolutePath + "." + logFileIdx)
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
    deps : Set[Dependence], jscopFile : File) : List[(Iterable[(Schedule, EvalResult)], Int) => Unit] = {
    var populationExportSinks : List[(Iterable[(Schedule, EvalResult)], Int) => Unit] = List.empty
    populationExportSinks ::= ScheduleExport.exportPopulationToFile(conf)
    if (conf.exportSchedulesToJSCOPFiles)
      populationExportSinks ::= ScheduleExport.exportPopulationToJSCOPFiles(conf,
        scop, domInfo, deps, jscopFile.getName)
    if (conf.exportPopulationToCSV)
      populationExportSinks ::= ScheduleExport.exportPopulationToCSV(conf)
    return populationExportSinks
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
  def runGA(args : Array[String], scheduleSelectionStrategy : ConfigGA => (Int, HashMap[Schedule, EvalResult], Int) => Array[(Schedule, EvalResult)]) : Unit = {

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

    // set the random seed if asked to
    if (conf.seed.isDefined)
      Random.setSeed(conf.seed.get)

    MainUtil.configureLogger(myLogger, conf)
    myLogger.info("Configuration:\n" + conf.toString())
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils
      .calcDepsAndDomInfo(scop)

    myLogger.info("dependences: \n" + deps.mkString("\n"))
    myLogger.info("number of dependences: " + deps.size)

    val populationExportSinks : List[(Iterable[(Schedule, EvalResult)], Int) => Unit] = MainUtil.buildPopulationExportSinks(conf, scop, domInfo, deps, jscopFile)

    var initialPopulation : HashMap[Schedule, EvalResult] = null
    val populationFile = ScheduleExport.buildPopulationFilePath(conf.populationFilePrefix,
      conf.currentGeneration)

    val isImportedPopulation = populationFile.exists()

    if (populationFile.exists()) {
      if (Util.checkFileExistsAndHasRequiredPermissions(
        true, true, false, false, populationFile)) {
        myLogger.info("Importing the existing poplulation.")
        val populationMaybe : Option[HashMap[Schedule, EvalResult]] = ScheduleExport
          .loadPopulationFromFile(conf, domInfo, deps, conf.currentGeneration)
        populationMaybe match {
          case None    => return
          case Some(p) => initialPopulation = p
        }
      } else {
        return
      }
    } else {
      if (conf.currentGeneration == 0) {
        initialPopulation = HashMap.empty
        ScheduleUtils.genRandSchedules(domInfo, deps, conf.regularPopulationSize, conf.initPopulationNumRays,
          conf.initPopulationNumLines, conf) map { s =>
            initialPopulation.put(s, EvalResult.notEvaluated)
          }
      } else {
        myLogger.warning("Starting from generation " + conf.currentGeneration
          + ". Expected file " + populationFile + " to exist. Terminating.")
        return
      }
    }
    try {
      GeneticOptimization.optimize(ScheduleEvaluation.evaluateSchedules(scop,
        domInfo, deps, conf), initialPopulation, conf, scop,
        populationExportSinks, isImportedPopulation, domInfo, deps,
        scheduleSelectionStrategy(conf))
    } catch {
      case e : StorageException => {
        myLogger.severe("Failed to store the population to file: " + e.getMessage)
        return
      }
    }
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
  def runRandExpl[T <: ConfigRand](args : Array[String],
    parseConfig : File => Option[T], buildRandSchedGen : ScopInfo => ((DomainCoeffInfo, Set[Dependence], Int, Set[Schedule], NumGeneratorsLimit, NumGeneratorsLimit, T) => Set[Schedule])) : Unit = {
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

    val populationExportSinks : List[(Iterable[(Schedule, EvalResult)], Int) => Unit] = MainUtil.buildPopulationExportSinks(conf, scop, domInfo, deps, jscopFile)

    val genRandSchedules : (DomainCoeffInfo, Set[Dependence], Int, Set[Schedule], NumGeneratorsLimit, NumGeneratorsLimit, T) => Set[Schedule] = buildRandSchedGen(scop)

    /* 
     * 1. Optionally load already generated schedules from file (the file name is
     *      implicitly deduced from the configuration).
     * 2. Separate schedules that still need evaluation from already evaluated
     *      schedules.
     */
    var evaluatedScheds : HashMap[Schedule, EvalResult] = HashMap.empty
    var scheds2Eval : HashSet[Schedule] = HashSet.empty
    var allScheds : Set[Schedule] = Set.empty

    if (conf.importScheds) {
      myLogger.info("Importing schedules from file.")
      val importedScheds : HashMap[Schedule, EvalResult] = ScheduleExport
        .loadPopulationFromFile(conf, domInfo, deps, 0) match {
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
        importedScheds.filter(_._2.completelyEvaluated).map(t => evaluatedScheds.put(t._1, t._2))
        importedScheds.filterNot(_._2.completelyEvaluated).map(t => scheds2Eval.add(t._1))
        myLogger.info(scheds2Eval.size + " of the imported schedules must be evaluated.")
      }
      allScheds = importedScheds.keySet.toSet
    }

    // Extend the number of random schedules to the configured maximum.
    myLogger.info(f"Generating ${conf.numScheds - allScheds.size} random schedules.")
    allScheds = genRandSchedules(domInfo, deps, conf.numScheds, allScheds, conf.maxNumRays, conf.maxNumLines, conf)

    allScheds.filterNot(evaluatedScheds.keySet.contains).map(scheds2Eval.add)

    if (conf.evaluateScheds) {
      if (conf.numSchedTreeSimplDurationMeasurements.isDefined) {
        val nMeasurements : Int = conf.numSchedTreeSimplDurationMeasurements.get
        myLogger.info(f"Evaluating the duration of schedule tree simplification ${nMeasurements} times for each schedule.")
        val evalResult : Map[Schedule, EvalResult] = EvalSchedTreeGenerationDuration.eval(scheds2Eval, conf, nMeasurements, scop)
        evaluatedScheds = evaluatedScheds ++ evalResult
      } else {
        // Evaluate the schedules.
        myLogger.info("Evaluating each schedule.")
        val (evalResult : HashMap[Schedule, EvalResult], evalSuccess : Boolean) =
          ScheduleEvaluation.evaluateSchedules(scop, domInfo, deps, conf)(scheds2Eval)
        evaluatedScheds = evaluatedScheds ++ evalResult
      }
    } else {
      myLogger.info(scheds2Eval.size + " schedules require evaluation but schedule evaluation is disabled.")
      val unevaluated : HashMap[Schedule, EvalResult] = HashMap.empty
      scheds2Eval.foreach { (s : Schedule) => unevaluated.put(s, EvalResult.notEvaluated) }
      evaluatedScheds = evaluatedScheds ++ unevaluated
    }

    myLogger.info("Storing the evaluated schedules.")
    populationExportSinks.map(f => f(evaluatedScheds, 0))
  }
}