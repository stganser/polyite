package polyite.config

import java.io.File
import java.io.FileReader
import java.util.Properties
import java.util.logging.Logger
import polyite.util.Rat
import polyite.util.Util
import polyite.util.ParseArgs
import scala.collection.mutable.HashMap
import polyite.config.MinimalConfig.EvaluationStrategy
import polyite.fitness.scikit_learn.Classifier

/**
  * Config is the basic type for configurations in the search based schedule
  * optimization framework.
  */

object Config {

  val myLogger : Logger = Logger.getLogger("")

  def loadAndValidateConfig(f : File) : Option[Config] = {
    return parseConfig(MinimalConfig.loadProperties(f))
  }

  /**
    * Constructs an instance of {@code Config} from a given instance of
    * Properties.
    * @return Returns {@code Some(conf)} if {@code rawConf} contains the required
    * properties in the required format fulfilling all constraints. Otherwise
    * returns {@code None}.
    */
  def parseConfig(rawConf : Properties) : Option[Config] = {
    val conf : MinimalConfig = MinimalConfig.parseConfig(rawConf) match {
      case None    => return None
      case Some(c) => c
    }

    val functionName : Option[String] = MinimalConfig.getProperty("functionName", rawConf)
    if (!functionName.isDefined) return None

    val scopRegionStart : Option[String] = MinimalConfig.getProperty("scopRegionStart", rawConf)
    if (!scopRegionStart.isDefined) return None

    val scopRegionEnd : Option[String] = MinimalConfig.getProperty("scopRegionEnd", rawConf)
    if (!scopRegionEnd.isDefined) return None

    val irFilesLocation : Option[File] = MinimalConfig.getFileProperty(
      "irFilesLocation",
      rawConf)
    if (!Util.checkFileExistsAndHasRequiredPermissions(true, false, true, true,
      irFilesLocation.get)) return None

    var propName = "seqPollyOptFlags"
    val seqPollyOptFlags : Option[String] = MinimalConfig.getProperty(propName, rawConf)
    if (!seqPollyOptFlags.isDefined)
      return None

    propName = "parPollyOptFlags"
    val parPollyOptFlags : Option[String] = MinimalConfig.getProperty(propName, rawConf)
    if (!parPollyOptFlags.isDefined)
      return None

    propName = "numactlConf"
    val numactlConf : Option[Option[String]] = MinimalConfig.getOptionalProperty(propName, rawConf, (MinimalConfig.getProperty))
    if (!numactlConf.isDefined)
      return None

    return Some(new Config(
      conf.numMeasurementThreads,
      conf.rayCoeffsRange,
      conf.lineCoeffsRange,
      conf.maxNumRays,
      conf.maxNumLines,
      conf.probabilityToCarryDep,
      conf.maxNumSchedsAtOnce,
      conf.measurementCommand,
      conf.measurementWorkingDir,
      conf.measurementTmpDirBase,
      conf.benchmarkName,
      functionName.get,
      scopRegionStart.get,
      scopRegionEnd.get,
      irFilesLocation.get,
      conf.referenceOutputFile,
      conf.numExecutionTimeMeasurements,
      conf.populationFilePrefix,
      conf.exportSchedulesToJSCOPFiles,
      conf.jscopFolderPrefix,
      conf.measurementTimeout,
      conf.exportPopulationToCSV,
      conf.csvFilePrefix,
      conf.logToFile,
      conf.logFile,
      conf.evaluationSigIntExitCode,
      conf.randSchedsTimeout,
      conf.measurementTmpDirNamePrefix,
      conf.genSchedsMaxAllowedConseqFailures,
      conf.numScheduleGenThreads,
      conf.filterImportedPopulation,
      conf.islComputeout,
      conf.paramValMappings,
      conf.measureParExecTime,
      conf.measureSeqExecTime,
      conf.moveVertices,
      conf.rayPruningThreshold,
      seqPollyOptFlags.get,
      parPollyOptFlags.get,
      conf.insertSetNodes,
      conf.compilationTimeout,
      conf.benchmarkingSurrenderTimeout,
      conf.measureCacheHitRateSeq,
      conf.measureCacheHitRatePar,
      conf.seed,
      numactlConf.get,
      conf.linIndepVectsDoNotFixDims,
      conf.simplifySchedTrees,
      conf.splitLoopBodies,
      conf.numCompilatonDurationMeasurements,
      conf.validateOutput,
      conf.tilingPermitInnerSeq,
      conf.schedTreeSimplRebuildDimScheds,
      conf.schedTreeSimplRemoveCommonOffset,
      conf.schedTreeSimplDivideCoeffsByGCD,
      conf.schedTreeSimplElimSuperfluousSubTrees,
      conf.schedTreeSimplElimSuperfluousDimNodes,
      conf.barvinokBinary,
      conf.barvinokLibraryPath,
      conf.normalizeFeatures,
      conf.evaluationStrategy,
      conf.learningSet,
      conf.decTreeMinSamplesLeaf,
      conf.learningAlgorithm,
      conf.randForestNTree,
      conf.randForestMaxFeatures,
      conf.pythonVEnvLocation,
      conf.samplingStrategy,
      conf.schedCoeffsMin,
      conf.schedCoeffsMax,
      conf.schedCoeffsExpectationValue,
      conf.scheduleEquivalenceRelation,
      conf.schedCoeffsAbsMax))
  }
}

class Config(
  numMeasurementThreads : Int,
  rayCoeffsRange : Int,
  lineCoeffsRange : Int,
  maxNumRays : MinimalConfig.NumGeneratorsLimit,
  maxNumLines : MinimalConfig.NumGeneratorsLimit,
  probabilityToCarryDep : Double,
  maxNumSchedsAtOnce : Int,
  measurementCommand : String,
  measurementWorkingDir : File,
  measurementTmpDirBase : File,
  benchmarkName : String,
  val functionName : String,
  val scopRegionStart : String,
  val scopRegionEnd : String,
  val irFilesLocation : File,
  referenceOutputFile : File,
  numExecutionTimeMeasurements : Int,
  populationFilePrefix : String,
  exportSchedulesToJSCOPFiles : Boolean,
  jscopFolderPrefix : String,
  measurementTimeout : Long,
  exportPopulationToCSV : Boolean,
  csvFilePrefix : String,
  logToFile : Boolean,
  logFile : File,
  evaluationSigIntExitCode : Int,
  randSchedsTimeout : Long,
  measurementTmpDirNamePrefix : String,
  genSchedsMaxAllowedConseqFailures : Int,
  numScheduleGenThreads : Int,
  filterImportedPopulation : Boolean,
  islComputeout : Int,
  paramValMappings : Map[String, Int],
  measureParExecTime : Boolean,
  measureSeqExecTime : Boolean,
  moveVertices : Boolean,
  rayPruningThreshold : Option[Rat],
  val seqPollyOptFlags : String,
  val parPollyOptFlags : String,
  insertSetNodes : Boolean,
  compilationTimeout : Option[Long],
  benchmarkingSurrenderTimeout : Option[Double],
  measureCacheHitRateSeq : Boolean,
  measureCacheHitRatePar : Boolean,
  seed : Option[Long],
  val numactlConf : Option[String],
  linIndepVectsDoNotFixDims : Boolean,
  simplifySchedTrees : Boolean,
  splitLoopBodies : Boolean,
  numCompilatonDurationMeasurements : Int,
  validateOutput : Boolean,
  tilingPermitInnerSeq : Boolean,
  schedTreeSimplRebuildDimScheds : Boolean,
  schedTreeSimplRemoveCommonOffset : Boolean,
  schedTreeSimplDivideCoeffsByGCD : Boolean,
  schedTreeSimplElimSuperfluousSubTrees : Boolean,
  schedTreeSimplElimSuperfluousDimNodes : Boolean,
  barvinokBinary : File,
  barvinokLibraryPath : File,
  normalizeFeatures : Boolean,
  evaluationStrategy : EvaluationStrategy.Value,
  learningSet : Option[List[File]],
  decTreeMinSamplesLeaf : Option[Int],
  learningAlgorithm : Option[Classifier.LearningAlgorithms.Value],
  randForestNTree : Option[Int],
  randForestMaxFeatures : Option[Int],
  pythonVEnvLocation : Option[File],
  samplingStrategy : MinimalConfig.SamplingStrategy.Value,
  schedCoeffsMin : Option[Int],
  schedCoeffsMax : Option[Int],
  schedCoeffsExpectationValue : Option[Double],
  scheduleEquivalenceRelation : MinimalConfig.ScheduleEquivalenceRelation.Value,
  schedCoeffsAbsMax : Option[Int]) extends MinimalConfig(
  numMeasurementThreads,
  rayCoeffsRange,
  lineCoeffsRange,
  maxNumRays,
  maxNumLines,
  probabilityToCarryDep,
  maxNumSchedsAtOnce,
  measurementCommand,
  measurementWorkingDir,
  measurementTmpDirBase,
  benchmarkName,
  referenceOutputFile,
  numExecutionTimeMeasurements,
  populationFilePrefix,
  exportSchedulesToJSCOPFiles,
  jscopFolderPrefix,
  measurementTimeout,
  exportPopulationToCSV,
  csvFilePrefix,
  logToFile,
  logFile,
  evaluationSigIntExitCode,
  randSchedsTimeout,
  measurementTmpDirNamePrefix,
  genSchedsMaxAllowedConseqFailures,
  numScheduleGenThreads,
  filterImportedPopulation,
  islComputeout,
  paramValMappings,
  measureParExecTime,
  measureSeqExecTime,
  moveVertices,
  rayPruningThreshold,
  insertSetNodes,
  compilationTimeout,
  benchmarkingSurrenderTimeout,
  measureCacheHitRateSeq,
  measureCacheHitRatePar,
  seed,
  linIndepVectsDoNotFixDims,
  simplifySchedTrees,
  splitLoopBodies,
  numCompilatonDurationMeasurements,
  validateOutput,
  tilingPermitInnerSeq,
  schedTreeSimplRebuildDimScheds,
  schedTreeSimplRemoveCommonOffset,
  schedTreeSimplDivideCoeffsByGCD,
  schedTreeSimplElimSuperfluousSubTrees,
  schedTreeSimplElimSuperfluousDimNodes,
  barvinokBinary,
  barvinokLibraryPath,
  normalizeFeatures,
  evaluationStrategy,
  learningSet,
  decTreeMinSamplesLeaf,
  learningAlgorithm,
  randForestNTree,
  randForestMaxFeatures,
  pythonVEnvLocation,
  samplingStrategy,
  schedCoeffsMin,
  schedCoeffsMax,
  schedCoeffsExpectationValue,
  scheduleEquivalenceRelation,
  schedCoeffsAbsMax) {

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    sb.append(super.toString());

    MinimalConfig.toStringAppend("functionName", functionName, sb)
    MinimalConfig.toStringAppend("scopRegionStart", scopRegionStart, sb)
    MinimalConfig.toStringAppend("scopRegionEnd", scopRegionEnd, sb)
    MinimalConfig.toStringAppend("irFilesLocation", irFilesLocation, sb)
    MinimalConfig.toStringAppend("seqPollyOptFlags", seqPollyOptFlags, sb)
    MinimalConfig.toStringAppend("parPollyOptFlags", parPollyOptFlags, sb)
    MinimalConfig.toStringAppendOptional("numactlConf", numactlConf, sb)

    return sb.toString()
  }
}
