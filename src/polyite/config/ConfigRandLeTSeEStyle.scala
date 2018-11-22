package polyite.config

import java.io.File
import java.util.Properties
import java.util.logging.Logger

import polyite.config.MinimalConfig.EvaluationStrategy
import polyite.fitness.scikit_learn.Classifier
import polyite.util.Rat

object ConfigRandLeTSeEStyle {
  val myLogger : Logger = Logger.getLogger("")

  object DepsWeighingMethod extends Enumeration {
    val DEP_CACHE_FOOT_PRINT, STMT_MEM_TRAFFIC, APPROX_STMT_MEM_TRAFFIC = Value
  }

  def loadAndValidateConfig(f : File) : Option[ConfigRandLeTSeEStyle] = {
    return parseConfig(MinimalConfig.loadProperties(f))
  }

  def parseConfig(rawConf : Properties) : Option[ConfigRandLeTSeEStyle] = {
    val basicConf : ConfigRand = ConfigRand.parseConfig(rawConf) match {
      case None    => return None
      case Some(c) => c
    }

    var propName : String = "boundSchedCoeffs"
    val boundSchedCoeffs : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!boundSchedCoeffs.isDefined) return None

    propName = "completeSchedules"
    val completeSchedules : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!completeSchedules.isDefined)
      return None

    propName = "depsWeighingMethod"
    val depsWeighingMethodStr : Option[String] = MinimalConfig.getProperty(propName, rawConf)
    if (!depsWeighingMethodStr.isDefined)
      return None
    val depsWeighingMethod : DepsWeighingMethod.Value = try {
      DepsWeighingMethod.withName(depsWeighingMethodStr.get)
    } catch {
      case (e : NoSuchElementException) => {
        myLogger.warning(f"The method for weighing of dependence polyhedra '${depsWeighingMethodStr.get}' is unknown.")
        return None
      }
    }

    return Some(new ConfigRandLeTSeEStyle(
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
      basicConf.evaluationStrategy,
      basicConf.learningSet,
      basicConf.decTreeMinSamplesLeaf,
      basicConf.learningAlgorithm,
      basicConf.randForestNTree,
      basicConf.randForestMaxFeatures,
      basicConf.pythonVEnvLocation,
      MinimalConfig.SamplingStrategy.CHERNIKOVA,
      None,
      None,
      None,
      basicConf.scheduleEquivalenceRelation,
      None,
      basicConf.expectPrevectorization,

      basicConf.numScheds,
      basicConf.importScheds,
      basicConf.evaluateScheds,
      basicConf.numSchedTreeSimplDurationMeasurements,

      boundSchedCoeffs.get,
      completeSchedules.get,
      depsWeighingMethod))
  }
}

class ConfigRandLeTSeEStyle(
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
  schedCoeffsAbsMax : Option[Int],
  expectPrevectorization : Boolean,

  numScheds : Int,
  importScheds : Boolean,
  evaluateScheds : Boolean,
  numSchedTreeSimplDurationMeasurements : Option[Int],

  val boundSchedCoeffs : Boolean,
  val completeSchedules : Boolean,
  val depsWeighingMethod : ConfigRandLeTSeEStyle.DepsWeighingMethod.Value) extends ConfigRand(
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
  schedCoeffsAbsMax,
  expectPrevectorization,

  numScheds,
  importScheds,
  evaluateScheds,
  numSchedTreeSimplDurationMeasurements) {

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    sb.append(super.toString())

    MinimalConfig.toStringAppend("boundSchedCoeffs", boundSchedCoeffs, sb)
    MinimalConfig.toStringAppend("completeSchedules", completeSchedules, sb)
    MinimalConfig.toStringAppend("depsWeighingMethod", depsWeighingMethod, sb)
    return sb.toString()
  }
}