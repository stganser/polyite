package polyite.export

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.util.logging.Level
import java.util.logging.Logger

import scala.collection.mutable.HashMap

import polyite.ScopInfo
import polyite.config.Config
import polyite.config.ConfigRand
import polyite.config.MinimalConfig.EvaluationStrategy
import polyite.fitness.Feature
import polyite.fitness.FeatureVect
import polyite.sched_eval.EvalResult
import polyite.sched_eval.EvalResultOnly
import polyite.sched_eval.Fitness
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.util.Util

/**
  * Provides functions for exporting sets of schedules together with evaluation
  * results to different formats.
  */
object ScheduleExport {
  private val myLogger : Logger = Logger.getLogger("")

  /**
    * Export the schedules in {@code population} together with their evaluation
    * results to a JSON file. The name of the file is inferred from
    * {@code conf.populationFilePrefix} and {@code generation}. The exported data
    * can be re-imported using the load-functions.
    */
  def exportPopulationToFileEvalRes(conf : Config)(
    population : Iterable[(Schedule, EvalResult)],
    generation : Int) {
    val populationFile = buildPopulationFilePath(conf.populationFilePrefix, generation)
    exportPopulationToFileEvalRes(populationFile, population, generation)
  }

  def exportPopulationToFileEvalRes(populationFile : File, population : Iterable[(Schedule, EvalResult)], generation : Int) {
    val popWrapped : Iterable[(Schedule, Fitness)] = population.map(t => (t._1, EvalResultOnly(t._2)))
    exportPopulationToFile(populationFile, popWrapped, generation)
  }

  /**
    * Export the schedules in {@code population} together with their evaluation
    * results to a JSON file. The name of the file is inferred from
    * {@code conf.populationFilePrefix} and {@code generation}. The exported data
    * can be re-imported using the load-functions.
    */
  def exportPopulationToFile(conf : Config, makeFileDistinct: String => String = s => s)(population : Iterable[(Schedule, Fitness)], features : List[Feature],
    generation : Int) {
    val populationFile = buildPopulationFilePath(makeFileDistinct(conf.populationFilePrefix), generation)
    exportPopulationToFile(populationFile, population, generation)
  }

  /**
    * Export the schedules in {@code population} together with their evaluation results to a JSON file. The exported
    * data can be re-imported using the load-functions.
    */
  def exportPopulationToFile(populationFile : File, population : Iterable[(Schedule, Fitness)], generation : Int) {

    try {
      JSONLogExporter.writePopulationToFile(populationFile, population, generation)
    } catch {
      case e : IOException => {
        val ex : StorageException = new StorageException(
          "Failed to export the population.")
        ex.initCause(e)
        throw ex
      }
    }
    myLogger.info("The current population has been exported to "
      + populationFile.getPath())
  }

  class StorageException(msg : String) extends Exception(msg)

  /**
    * Export each of the given schedules to a JSCOP file with file name prefix
    * {@code jscopFileNamePrefix}. All files are put into a folder whose name is
    * {@code conf.jscopFolderPrefix + generation}. The ordering of the schedules
    * is deterministic but cannot be influenced.
    */
  def exportPopulationToJSCOPFiles(conf : Config, scop : ScopInfo,
    domInfo : DomainCoeffInfo, deps : Set[Dependence], jscopFileNamePrefix : String, makeFileDistinct : String => String = s => s)(
      population : Iterable[(Schedule, Fitness)], features : List[Feature], generation : Int) {
    val populationDir : File = new File(makeFileDistinct(conf.jscopFolderPrefix) + generation)
    exportPopulationToJSCOPFiles(conf, scop, domInfo, deps, jscopFileNamePrefix,
      population, populationDir)
  }

  /**
    * Export each of the given schedules to a JSCOP file with file name prefix
    * {@code jscopFileNamePrefix}. All files are put into the folder
    * {@code populationDir}. The ordering of the schedules is deterministic but
    * cannot be influenced.
    */
  def exportPopulationToJSCOPFiles(conf : Config, scop : ScopInfo,
    domInfo : DomainCoeffInfo, deps : Set[Dependence],
    jscopFileNamePrefix : String, population : Iterable[(Schedule, Fitness)],
    populationDir : File) {
    populationDir.mkdirs()
    if (!Util.checkFileExistsAndHasRequiredPermissions(true, true, true, true,
      populationDir))
      throw new StorageException("Cannot export JSCOP files to "
        + populationDir.getPath + ".")
    val contents : Array[File] = populationDir.listFiles()
    if (!contents.isEmpty) {
      myLogger.warning(populationDir.getPath + " is not empty. Deleting "
        + "previous contents.")
      contents.map { f => f.delete() }
    }
    var schedIdx : Int = 0
    sortSchedules(population).map((t : (Schedule, Fitness)) => {
      val s : Schedule = t._1
      val schedMap : isl.UnionMap = s.getSchedule
      val schedTree : isl.Schedule = ScheduleTreeConstruction
        .islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf)
      val f : File = new File(populationDir, jscopFileNamePrefix + "." + schedIdx)
      try {
        JSCOPInterface.exportScheduleExt(f, scop, schedMap, schedTree)
      } catch {
        case e : IOException => {
          val ex : StorageException = new StorageException(
            "Failed to write JSCOP-file.")
          ex.initCause(e)
          throw ex
        }
      }
      schedIdx += 1
    })
  }

  private def sortSchedules(
    population : Iterable[(Schedule, Fitness)]) : List[(Schedule, Fitness)] =
    population.toList.sortBy(t => t._1.getSchedule.toString())

  private def sortSchedulesUnionMap(
    population : Iterable[(isl.UnionMap, Fitness)]) : List[(isl.UnionMap, Fitness)] =
    population.toList.sortBy(t => t._1.toString())

  /**
    * Exports the given schedules together with their evaluation results to a
    * CSV-file. The name of the CSV.file is {@code conf.csvFilePrefix + generation + ".csv"}.
    */
  def exportPopulationToCSVEvalRes(conf : Config)(population : Iterable[(Schedule, EvalResult)], generation : Int) {
    val populationWrapped : List[(Schedule, Fitness)] = population.toList.map(t => (t._1, EvalResultOnly(t._2)))
    exportPopulationToCSV(conf, populationWrapped, true, false, List.empty, generation)
  }

  def exportPopulationToCSV(conf : Config, makeFileDistinct: String => String = s => s)(population : Iterable[(Schedule, Fitness)], features : List[Feature],
    generation : Int) {
    val popSchedMaps : Iterable[(isl.UnionMap, Fitness)] = population.map(t => (t._1.getSchedule, t._2))
    exportPopulationToCSVUnionMap(conf, makeFileDistinct)(popSchedMaps,
      conf.evaluationStrategy == EvaluationStrategy.CPU || conf.evaluationStrategy == EvaluationStrategy.GPU,
      conf.evaluationStrategy == EvaluationStrategy.CLASSIFIER, features, generation)
  }

  /**
    * Exports the given schedules together with their evaluation results to a
    * CSV-file. The name of the CSV.file is {@code conf.csvFilePrefix + generation + ".csv"}.
    */
  def exportPopulationToCSV(conf : Config, population : Iterable[(Schedule, Fitness)], exportEvalRes : Boolean,
    exportPerfClass : Boolean, features : List[Feature], generation : Int) {
    val popSchedMaps : Iterable[(isl.UnionMap, Fitness)] = population.map(t => (t._1.getSchedule, t._2))
    exportPopulationToCSVUnionMap(conf)(popSchedMaps, exportEvalRes, exportPerfClass, features, generation)
  }

  def exportPopulationToCSVUnionMap(csvFile : File, numCompilatonDurationMeasurements : Int,
    numSchedTreeSimplDurationMeasurements : Int, numExecutionTimeMeasurements : Int,
    population : Iterable[(isl.UnionMap, Fitness)], exportEvalRes : Boolean, exportPerfClass : Boolean, features : List[Feature], generation : Int) = {
    try {
      if (!csvFile.createNewFile()) {
        myLogger.warning("File " + csvFile + " already existed. Overwritting "
          + "the old file.")
      }
    } catch {
      case e : IOException => throw new StorageException("Failed to create "
        + csvFile.getPath + ".")
    }
    if (!Util.checkFileExistsAndHasRequiredPermissions(true, true, false, false, csvFile))
      throw new StorageException("Cannot write to " + csvFile.getPath + ".")
    var writer : BufferedWriter = null
    try {
      def timeHeaders(suffix : String) : String = {
        if (numExecutionTimeMeasurements == 0)
          return ""
        return (0 until numExecutionTimeMeasurements)
          .map { x => "t" + x + suffix }.mkString("\t", "\t", "")
      }

      def codegenDurationHeaders(prefix : String) : String = {
        if (numCompilatonDurationMeasurements == 0)
          return ""
        return (0 until numCompilatonDurationMeasurements)
          .map { x => prefix + x }.mkString("\t", "\t", "")
      }

      writer = new BufferedWriter(new FileWriter(csvFile))
      writer.write("index\tschedule")

      var exportSchedTreeSimplDurations : Boolean = false

      if (exportEvalRes) {
        writer.write("\tcompletelyEvaluated\t")

        if (numSchedTreeSimplDurationMeasurements > 0) {
          exportSchedTreeSimplDurations = true
          writer.write((0 until numSchedTreeSimplDurationMeasurements).map("schedTreeSimplDuration" + _.toString).mkString("\t"))
          writer.write("\t")
        }
        writer.write("isCodegenSuccessful"
          + codegenDurationHeaders("parallelCodegenDuration") + codegenDurationHeaders("sequentialCodegenDuration")
          + "\texecutionValidateResultParSuccessful\thasValidResultPar\t"
          + "executionValidateResultSeqSuccessful\thasValidResultSeq\t"
          + "executionMeasureRuntimeParSuccessful" + timeHeaders("par") + "\t"
          + "executionMeasureRuntimeSeqSuccessful" + timeHeaders("seq") + "\t"
          + "executionMeasureCacheHitRateParSuccessful\tcacheHitRatePar\texecutionMeasureCacheHitRateSeqSuccessful\t"
          + "cacheHitRateSeq\ttimedOut")
      }

      if (!features.isEmpty) {
        val featuresSorted : List[Feature] = features.sorted
        writer.write(featuresSorted.mkString("\t", "\t", ""))
      }

      if (exportPerfClass)
        writer.write("\tclass")

      sortSchedulesUnionMap(population).foldLeft(0)((idx : Int, t) => {

        val defaultVal : String = "-"

        writer.newLine()
        val s : isl.UnionMap = t._1
        writer.write(idx.toString())
        val sep : Char = '\t'

        def append(s : String) {
          writer.write(sep)
          writer.write(s)
        }

        def appendOptional[T](v : Option[T]) {
          append(v match {
            case None    => defaultVal
            case Some(t) => t.toString()
          })
        }

        append('\"' + s.toString() + '\"')

        if (exportEvalRes) {
          val res : EvalResult = t._2.getEvalResult.getOrElse(EvalResult.notEvaluated)
          append(res.completelyEvaluated.toString)

          if (exportSchedTreeSimplDurations)
            appendExecTimes(res.schedTreeSimplDurations, res.schedTreeSimplDurations.get.length)
          append(res.isCodegenSuccessful.toString)
          appendExecTimes(res.parallelCodegenDurations, numCompilatonDurationMeasurements)
          appendExecTimes(res.sequentialCodegenDurations, numCompilatonDurationMeasurements)
          appendOptional(res.executionValidateResultParSuccessful)
          appendOptional(res.hasValidResultPar)
          appendOptional(res.executionValidateResultSeqSuccessful)
          appendOptional(res.hasValidResultSeq)
          appendOptional(res.executionMeasureRuntimeParSuccessful)

          def appendExecTimes(l : Option[List[Double]], num : Int) {
            if (!l.isDefined)
              (0 until num)
                .map { _ => append(defaultVal) }
            else
              l.get.map { x => append(x.toString) }
          }
          appendExecTimes(res.executionTimesPar, numExecutionTimeMeasurements)
          appendOptional(res.executionMeasureRuntimeSeqSuccessful)
          appendExecTimes(res.executionTimesSeq, numExecutionTimeMeasurements)
          appendOptional(res.executionMeasureCacheHitRateParSuccessful)
          appendOptional(res.cacheHitRatePar)
          appendOptional(res.executionMeasureCacheHitRateSeqSuccessful)
          appendOptional(res.cacheHitRateSeq)
          append(res.timedOut.toString)
        }

        if (!features.isEmpty) {
          val featuresSorted : List[Feature] = features.sorted
          val fVect : FeatureVect = if (t._2.getPrediction.isDefined)
            t._2.getPrediction.get.fVect
          else
            new FeatureVect(Map.empty)
          featuresSorted.map(fVect.getFeature(_)).foreach(appendOptional)
        }

        if (exportPerfClass) {
          val fit : Fitness = t._2
          writer.write("\t")
          if (fit.getPrediction.isDefined && fit.getPrediction.get.pClass.isDefined)
            writer.write(fit.getPrediction.get.pClass.get.toString())
          else
            writer.write("-")
        }

        idx + 1
      })
    } catch {
      case e : IOException => {
        val ex : StorageException = new StorageException(
          "Failed to export the population to CSV.")
        ex.initCause(e)
        throw ex
      }
    } finally {
      if (writer != null)
        try {
          writer.close()
        } catch {
          case e : IOException => {
            myLogger.log(Level.SEVERE, "Failed to close writer.", e)
          }
        }
    }
  }

  /**
    * Exports the given schedules together with their evaluation results to a
    * CSV-file. The name of the CSV.file is {@code conf.csvFilePrefix + generation + ".csv"}.
    */
  def exportPopulationToCSVUnionMap(conf : Config, makeFileDistinct: String => String = s => s)(population : Iterable[(isl.UnionMap, Fitness)],
    exportEvalRes : Boolean, exportPerfClass : Boolean, features : List[Feature], generation : Int) {
    val csvFile : File = new File(makeFileDistinct(conf.csvFilePrefix) + generation + ".csv")
    val numSchedTreeSimplDurationMeasurements : Int = if (conf.isInstanceOf[ConfigRand]) {
      val confRand : ConfigRand = conf.asInstanceOf[ConfigRand]
      confRand.numSchedTreeSimplDurationMeasurements.getOrElse(0)
    } else
      0
    exportPopulationToCSVUnionMap(csvFile,
      conf.numCompilatonDurationMeasurements,
      numSchedTreeSimplDurationMeasurements,
      conf.numExecutionTimeMeasurements,
      population,
      exportEvalRes,
      exportPerfClass,
      features,
      generation)
  }

  /**
    * Loads a population of schedules and evaluation results from file. The name
    * of the file is inferred from {@code conf.populationFilePrefix} and
    * {@code generation}. The loaded schedule instances contain all previously
    * exported information.
    */
  def loadPopulationFromFile(conf : Config, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], generation : Int) : Option[HashMap[Schedule, Fitness]] = {
    val populationFile : File = buildPopulationFilePath(
      conf.populationFilePrefix, generation)
    return loadPopulationFromFile(populationFile, domInfo, deps, generation)
  }

  /**
    * Loads a population of schedules and evaluation results from file. The name
    * of the file is inferred from {@code conf.populationFilePrefix} and
    * {@code generation}. The loaded schedule instances contain all previously
    * exported information.
    */
  def loadPopulationFromFileNoFilter(conf : Config, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], generation : Int) : Option[HashMap[Schedule, Fitness]] = {
    val populationFile : File = buildPopulationFilePath(
      conf.populationFilePrefix, generation)
    return loadPopulationFromFileNoFilter(populationFile, domInfo, deps, generation).map(_._1)
  }

  def loadPopulationFromFile(populationFile : File, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], generation : Int) : Option[HashMap[Schedule, Fitness]] = {
    val populationRaw : HashMap[Schedule, Fitness] = loadPopulationFromFileNoFilter(
      populationFile, domInfo, deps, generation) match {
        case None    => return None
        case Some(p) => p._1
      }
    val filtered : HashMap[Schedule, Fitness] = populationRaw.map(t => {
      val fit : Fitness = t._2
      if (!fit.getEvalResult.isDefined) {
        myLogger.warning("schedule has no evaluation result: " + t._1)
        return None
      }
      (t._1, fit)
    })
    return Some(filtered)
  }

  def loadPopulationFromFileNoFilter(populationFile : File, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], generation : Int) : Option[(HashMap[Schedule, Fitness], Int)] = {
    if (!Util.checkFileExistsAndHasRequiredPermissions(true, false, false,
      false, populationFile)) {
      myLogger.warning("File " + populationFile + " cannot be accessed.")
      return None
    }

    val (population : HashMap[Schedule, Fitness], importedGeneration : Int) = JSONLogExporter
      .readPopulationFromFile(populationFile, domInfo, deps)
    if (importedGeneration != generation) {
      myLogger.warning("The given generation (" + generation
        + ") doesn't match the generation specified in " + populationFile
        + " (" + importedGeneration + ")")
      return None
    }
    return Some((population, importedGeneration))
  }

  /**
    * Works similar to {@code loadPopulationFromFileLight(conf : Config,
    * domInfo : DomainCoeffInfo, deps : Set[isl.BasicMap], generation : Int)}
    * but the file can be loaded from a different directory than the present
    * working directory.
    */
  def loadPopulationFromFile(parentDir : String, conf : Config, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], generation : Int) : Option[HashMap[Schedule, Fitness]] = {
    val populationFile : File = buildPopulationFilePath(parentDir,
      conf.populationFilePrefix, generation)
    return loadPopulationFromFile(populationFile, domInfo, deps, generation)
  }

  /**
    * Loads a population from file. The name of the file is inferred from
    * @code conf.populationFilePrefix} and {@code generation}. Instead of loading
    * all information about a schedule only the schedule map string and the
    * evaluation results are loaded.
    */
  def loadPopulationFromFileLight(conf : Config,
    generation : Int) : Option[HashMap[String, Fitness]] = {
    val populationFile : File = buildPopulationFilePath(
      conf.populationFilePrefix, generation)
    return loadPopulationFromFileLight(populationFile, generation)
  }

  /**
    * Works similar to {@code loadPopulationFromFileLight(conf: Config, generation: Int)}
    * but the file can be loaded from a different directory than the present
    * working directory.
    */
  def loadPopulationFromFileLight(parentDir : String, conf : Config,
    generation : Int) : Option[HashMap[String, Fitness]] = {
    val populationFile : File = buildPopulationFilePath(parentDir,
      conf.populationFilePrefix, generation)
    return loadPopulationFromFileLight(populationFile, generation)
  }

  def loadPopulationFromFileLight(f : File, generation : Int) : Option[HashMap[String, Fitness]] = {
    val populationRaw : HashMap[String, Fitness] = loadPopulationFromFileNoFilter(f, generation) match {
      case None    => return None
      case Some(p) => p._1
    }
    val filtered : HashMap[String, Fitness] = populationRaw.filter((p : (String, Fitness)) => p._2.getEvalResult.isDefined)
    return Some(filtered)
  }

  def loadPopulationFromFileNoFilter(f : File, generation : Int) : Option[(HashMap[String, Fitness], Int)] = {

    if (!Util.checkFileExistsAndHasRequiredPermissions(true, false, false,
      false, f)) {
      myLogger.warning("File " + f + " cannot be accessed.")
      return None
    }

    val (population : HashMap[String, Fitness],
      importedGeneration : Int) = JSONLogExporter.readPopulationFromFileLight(f)
    if (importedGeneration != generation) {
      myLogger.warning("The given generation (" + generation
        + ") doesn't match the generation specified in " + f
        + " (" + importedGeneration + ")")
      return None
    }
    return Some((population, importedGeneration))
  }

  /**
    * Constructs the file path for a population JSON file.
    */
  def buildPopulationFilePath(prefix : String,
    generation : Int) : File = new File(prefix + generation + ".json")

  /**
    * Constructs the file path for a population JSON file. An additional parent
    * directory can be added as a prefix.
    */
  def buildPopulationFilePath(parentDir : String, prefix : String,
    generation : Int) : File = new File(parentDir.replace("/$", "") + "/"
    + prefix + generation + ".json")
}
