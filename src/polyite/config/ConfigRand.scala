package polyite.config

import java.io.File
import java.util.Properties
import java.util.logging.Logger
import polyite.util.Rat

object ConfigRand {
  val myLogger : Logger = Logger.getLogger("")

  def loadAndValidateConfig(f : File) : Option[ConfigRand] = {
    return parseConfig(MinimalConfig.loadProperties(f))
  }

  def parseConfig(rawConf : Properties) : Option[ConfigRand] = {
    val basicConf : Config = Config.parseConfig(rawConf) match {
      case None    => return None
      case Some(c) => c
    }

    var propName = "numScheds"
    val numScheds : Option[Int] = MinimalConfig.getIntProperty(propName, rawConf)
    if (!numScheds.isDefined) return None
    if (!MinimalConfig.checkMin(0, numScheds, propName)) return None

    propName = "importScheds"
    val importScheds : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!importScheds.isDefined) return None

    propName = "evaluateScheds"
    val evaluateScheds : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!evaluateScheds.isDefined) return None

    propName = "numSchedTreeSimplDurationMeasurements"
    val numSchedTreeSimplDurationMeasurements : Option[Option[Int]] = MinimalConfig.getOptionalProperty(propName, rawConf, MinimalConfig.getIntProperty)
    if (!numSchedTreeSimplDurationMeasurements.isDefined)
      return None
    if (numSchedTreeSimplDurationMeasurements.get.isDefined && !MinimalConfig.checkMin(1, numSchedTreeSimplDurationMeasurements.get, propName))
      return None

    return Some(new ConfigRand(
      basicConf.numMeasurementThreads,
      basicConf.rayCoeffsRange,
      basicConf.lineCoeffsRange,
      basicConf.maxNumRays,
      basicConf.maxNumLines,
      basicConf.probabilityToCarryDep,
      basicConf.maxNumSchedsAtOnce,
      basicConf.measurementCommand,
      basicConf.measurementWorkingDir,
      basicConf.measurementTmpDirBase,
      basicConf.benchmarkName,
      basicConf.functionName,
      basicConf.scopRegionStart,
      basicConf.scopRegionEnd,
      basicConf.irFilesLocation,
      basicConf.referenceOutputFile,
      basicConf.numExecutionTimeMeasurements,
      basicConf.populationFilePrefix,
      basicConf.exportSchedulesToJSCOPFiles,
      basicConf.jscopFolderPrefix,
      basicConf.measurementTimeout,
      basicConf.exportPopulationToCSV,
      basicConf.csvFilePrefix,
      basicConf.logToFile,
      basicConf.logFile,
      basicConf.evaluationSigIntExitCode,
      basicConf.randSchedsTimeout,
      basicConf.measurementTmpDirNamePrefix,
      basicConf.genSchedsMaxAllowedConseqFailures,
      basicConf.numScheduleGenThreads,
      basicConf.filterImportedPopulation,
      basicConf.islComputeout,
      basicConf.paramValMappings,
      basicConf.measureParExecTime,
      basicConf.measureSeqExecTime,
      basicConf.moveVertices,
      basicConf.rayPruningThreshold,
      basicConf.seqPollyOptFlags,
      basicConf.parPollyOptFlags,
      basicConf.insertSetNodes,
      basicConf.compilationTimeout,
      basicConf.benchmarkingSurrenderTimeout,
      basicConf.measureCacheHitRateSeq,
      basicConf.measureCacheHitRatePar,
      basicConf.seed,
      basicConf.numactlConf,
      basicConf.linIndepVectsDoNotFixDims,
      basicConf.simplifySchedTrees,
      basicConf.splitLoopBodies,
      basicConf.numCompilatonDurationMeasurements,
      basicConf.validateOutput,
      basicConf.tilingPermitInnerSeq,
      basicConf.schedTreeSimplRebuildDimScheds,
      basicConf.schedTreeSimplRemoveCommonOffset,
      basicConf.schedTreeSimplDivideCoeffsByGCD,
      basicConf.schedTreeSimplElimSuperfluousSubTrees,
      basicConf.schedTreeSimplElimSuperfluousDimNodes,
      basicConf.barvinokBinary,
      basicConf.barvinokLibraryPath,
      basicConf.normalizeFeatures,
      basicConf.gpu,

      numScheds.get,
      importScheds.get,
      evaluateScheds.get,
      numSchedTreeSimplDurationMeasurements.get))
  }
}

class ConfigRand(
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
  functionName : String,
  scopRegionStart : String,
  scopRegionEnd : String,
  irFilesLocation : File,
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
  seqPollyOptFlags : String,
  parPollyOptFlags : String,
  insertSetNodes : Boolean,
  compilationTimeout : Option[Long],
  benchmarkingSurrenderTimeout : Option[Double],
  measureCacheHitRateSeq : Boolean,
  measureCacheHitRatePar : Boolean,
  seed : Option[Long],
  numactlConf : Option[String],
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
  gpu : Boolean,

  val numScheds : Int,
  val importScheds : Boolean,
  val evaluateScheds : Boolean,
  val numSchedTreeSimplDurationMeasurements : Option[Int]) extends Config(
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
  functionName,
  scopRegionStart,
  scopRegionEnd,
  irFilesLocation,
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
  seqPollyOptFlags,
  parPollyOptFlags,
  insertSetNodes,
  compilationTimeout,
  benchmarkingSurrenderTimeout,
  measureCacheHitRateSeq,
  measureCacheHitRatePar,
  seed,
  numactlConf,
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
  gpu) {

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    sb.append(super.toString())
    MinimalConfig.toStringAppend("numScheds", numScheds, sb)
    MinimalConfig.toStringAppend("importScheds", importScheds, sb)
    MinimalConfig.toStringAppend("evaluateScheds", evaluateScheds, sb)
    MinimalConfig.toStringAppendOptional("numSchedTreeSimplDurationMeasurements", numSchedTreeSimplDurationMeasurements, sb)
    return sb.toString()
  }
}