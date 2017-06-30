package polyite.config

import java.io.File
import java.io.FileReader
import java.util.Properties
import java.util.logging.Logger
import polyite.util.Rat
import polyite.util.Util
import polyite.util.ParseArgs
import scala.collection.mutable.HashMap

/**
  * MinimalConfig is the basic type for configurations in the search based schedule
  * optimization framework.
  */

object MinimalConfig {

  val myLogger : Logger = Logger.getLogger("")

  abstract class NumGeneratorsLimit

  case class LimitedGenerators(n : Int) extends NumGeneratorsLimit {
    override def toString() : String = n.toString()
  }

  object AllGenerators extends NumGeneratorsLimit {
    override def toString() : String = "ALL"
  }

  object RandLimit extends NumGeneratorsLimit {
    override def toString() : String = "RAND"
  }

  /**
    * Loads the contents of a properties file into an instance of
    * {@code java.util.Properties}.
    */
  def loadProperties(f : File) : Properties = {
    val rawConf : Properties = new Properties()
    var reader : FileReader = null
    try {
      reader = new FileReader(f)
      rawConf.load(reader)
    } finally {
      if (reader != null)
        reader.close()
    }
    return rawConf
  }

  def loadAndValidateConfig(f : File) : Option[MinimalConfig] = {
    return parseConfig(MinimalConfig.loadProperties(f))
  }

  /**
    * Constructs an instance of {@code MinimalConfig} from a given instance of
    * Properties.
    * @return Returns {@code Some(conf)} if {@code rawConf} contains the required
    * properties in the required format fulfilling all constraints. Otherwise
    * returns {@code None}.
    */
  def parseConfig(rawConf : Properties) : Option[MinimalConfig] = {
    var propName : String = "numMeasurementThreads"
    val numMeasurementThreads : Option[Int] = getIntProperty(propName, rawConf)
    if (!numMeasurementThreads.isDefined) return None
    if (!checkMin(1, numMeasurementThreads, propName)) return None

    propName = "rayCoeffsRange"
    val rayCoeffsRange : Option[Int] = getIntProperty(propName, rawConf)
    if (!rayCoeffsRange.isDefined) return None
    if (!checkMin(0, rayCoeffsRange, propName)) return None

    propName = "lineCoeffsRange"
    val lineCoeffsRange : Option[Int] = getIntProperty(propName, rawConf)
    if (!lineCoeffsRange.isDefined) return None

    val maxNumRays : Option[MinimalConfig.NumGeneratorsLimit] = parseNumGeneratorsLimit("maxNumRays", rawConf)
    if (!maxNumRays.isDefined) return None

    val maxNumLines : Option[MinimalConfig.NumGeneratorsLimit] = parseNumGeneratorsLimit("maxNumLines", rawConf)
    if (!maxNumLines.isDefined) return None

    propName = "probabilityToCarryDep"
    val probabilityToCarryDep : Option[Double] = getDoubleProperty(propName, rawConf)
    if (!probabilityToCarryDep.isDefined) return None
    if (!checkMinMax(0, 1, probabilityToCarryDep, propName)) return None

    propName = "maxNumSchedsAtOnce"
    val maxNumSchedsAtOnce : Option[Int] = getIntProperty(propName, rawConf)
    if (!maxNumSchedsAtOnce.isDefined) return None
    if (!checkMin(0, maxNumSchedsAtOnce, propName)) return None

    val measurementCommand : Option[String] = getProperty(
      "measurementCommand", rawConf)
    if (!measurementCommand.isDefined) return None

    val measurementWorkingDir : Option[File] = getFileProperty(
      "measurementWorkingDir", rawConf)
    if (!measurementWorkingDir.isDefined) return None
    if (!Util.checkFileExistsAndHasRequiredPermissions(true, true, true, true,
      measurementWorkingDir.get)) return None

    val measurementTmpDirBase : Option[File] = getFileProperty(
      "measurementTmpDirBase", rawConf)
    if (!measurementTmpDirBase.isDefined) return None

    val benchmarkName : Option[String] = getProperty("benchmarkName", rawConf)
    if (!benchmarkName.isDefined) return None

    val referenceOutputFile : Option[File] = getFileProperty(
      "referenceOutputFile", rawConf)
    if (!referenceOutputFile.isDefined) return None
    if (!Util.checkFileExistsAndHasRequiredPermissions(true, false, false, false,
      referenceOutputFile.get))
      return None

    propName = "numExecutionTimeMeasurements"
    val numExecutionTimeMeasurements : Option[Int] = getIntProperty(propName,
      rawConf)
    if (!numExecutionTimeMeasurements.isDefined) return None
    if (!checkMin(0, numExecutionTimeMeasurements, propName)) return None

    val populationFilePrefix : Option[String] = getProperty(
      "populationFilePrefix", rawConf)
    if (!populationFilePrefix.isDefined) return None

    propName = "exportSchedulesToJSCOPFiles"
    val exportSchedulesToJSCOPFilesEnabled : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!exportSchedulesToJSCOPFilesEnabled.isDefined)
      return None

    val jscopFolderPrefix : String =
      if (exportSchedulesToJSCOPFilesEnabled.get) {
        propName = "jscopFolderPrefix"
        val v : Option[String] = getProperty(propName, rawConf)
        if (!v.isDefined)
          return None
        v.get
      } else
        null

    propName = "measurementTimeout"
    val measurementTimeout : Option[Long] = getLongProperty(propName, rawConf)
    if (!measurementTimeout.isDefined)
      return None

    propName = "exportPopulationToCSV"
    val exportPopulationToCSV : Option[Boolean] = getBooleanProperty(propName,
      rawConf)
    if (!exportPopulationToCSV.isDefined)
      return None

    val csvFilePrefix : String =
      if (exportPopulationToCSV.get) {
        propName = "csvFilePrefix"
        val v : Option[String] = getProperty(propName, rawConf)
        if (!v.isDefined)
          return None
        v.get
      } else
        null

    propName = "logToFile"
    val logToFile : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!logToFile.isDefined)
      return None

    val logFile : File = if (logToFile.get) {
      propName = "logFile"
      val v : Option[File] = getFileProperty(propName, rawConf)
      if (!v.isDefined)
        return None
      v.get
    } else
      null

    propName = "evaluationSigIntExitCode"
    val evaluationSigIntExitCode : Option[Int] = getIntProperty(propName, rawConf)
    if (!evaluationSigIntExitCode.isDefined)
      return None

    propName = "randSchedsTimeout"
    val randSchedsTimeout : Option[Long] = getLongProperty(propName, rawConf)
    if (!randSchedsTimeout.isDefined)
      return None

    propName = "measurementTmpDirNamePrefix"
    val measurementTmpDirNamePrefix : Option[String] = getProperty(propName,
      rawConf)
    if (!measurementTmpDirNamePrefix.isDefined)
      return None

    propName = "genSchedsMaxAllowedConseqFailures"
    val genSchedsMaxAllowedConseqFailures : Option[Int] = getIntProperty(
      propName, rawConf)
    if (!genSchedsMaxAllowedConseqFailures.isDefined)
      return None
    if (!checkMin(0, genSchedsMaxAllowedConseqFailures, propName))
      return None

    propName = "numScheduleGenThreads"
    val numScheduleGenThreads : Option[Int] = getIntProperty(propName, rawConf)
    if (!numScheduleGenThreads.isDefined)
      return None
    if (!checkMin(1, numScheduleGenThreads, propName))
      return None

    propName = "filterImportedPopulation"
    val filterImportedPopulation : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!filterImportedPopulation.isDefined)
      return None

    propName = "islComputeout"
    val islComputeout : Option[Int] = getIntProperty(propName, rawConf)
    if (!islComputeout.isDefined)
      return None
    if (!checkMin(0, islComputeout, propName))
      return None

    propName = "paramValMappings"
    val paramValMappingsStr : Option[String] = MinimalConfig.getProperty(propName, rawConf)
    if (!paramValMappingsStr.isDefined)
      return None
    val mappingRegex : String = "[A-Za-z0-9-_]+=[0-9]+"
    val mappingsListRegex : String = f"(${mappingRegex}(,${mappingRegex})*)?"
    val paramValMappingsStrCleaned : String = paramValMappingsStr.get.replaceAll("\\s", "")
    if (!paramValMappingsStrCleaned.matches(mappingsListRegex)) {
      myLogger.warning("The value of " + propName
        + " must match the regular expression " + mappingRegex + ".")
      return None
    }
    val mappingStrs : Array[String] = paramValMappingsStrCleaned.split(",")
    val paramValMappings : HashMap[String, Int] = HashMap.empty
    for (mStr : String <- mappingStrs) {
      val tokens : Array[String] = mStr.split("=")
      paramValMappings.put(tokens(0), tokens(1).toInt)
    }

    propName = "measureSeqExecTime"
    val measureSeqExecTime : Option[Boolean] = MinimalConfig
      .getBooleanProperty(propName, rawConf)
    if (!measureSeqExecTime.isDefined)
      return None

    propName = "measureParExecTime"
    val measureParExecTime : Option[Boolean] = MinimalConfig
      .getBooleanProperty(propName, rawConf)
    if (!measureParExecTime.isDefined)
      return None

    propName = "moveVertices"
    val moveVertices : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!moveVertices.isDefined)
      return None

    propName = "rayPruningThreshold"
    def parser(key : String, props : Properties) : Option[Rat] = {
      MinimalConfig.getIntProperty(key, props) match {
        case None    => return None
        case Some(x) => Some(Rat(x))
      }
    }
    val rayPruningThreshold : Option[Option[Rat]] = getOptionalProperty(propName, rawConf, parser)
    if (!rayPruningThreshold.isDefined)
      return None

    propName = "insertSetNodes"
    val insertSetNodes : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!insertSetNodes.isDefined)
      return None

    propName = "compilationTimeout"
    val compilationTimeout : Option[Option[Long]] = getOptionalProperty(propName, rawConf, getLongProperty)
    if (!compilationTimeout.isDefined)
      return None
    if (compilationTimeout.get.isDefined && !checkMin(0 : Long, compilationTimeout.get, propName))
      return None

    propName = "benchmarkingSurrenderTimeout"
    val benchmarkingSurrenderTimeout : Option[Option[Double]] = getOptionalProperty(propName, rawConf, getDoubleProperty)
    if (!benchmarkingSurrenderTimeout.isDefined)
      return None
    if (benchmarkingSurrenderTimeout.isDefined && !checkMin(0 : Double, benchmarkingSurrenderTimeout.get, propName))
      return None

    propName = "measureCacheHitRateSeq"
    val measureCacheHitRateSeq : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!measureCacheHitRateSeq.isDefined)
      return None

    propName = "measureCacheHitRatePar"
    val measureCacheHitRatePar : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!measureCacheHitRatePar.isDefined)
      return None

    propName = "seed"
    val seed : Option[Option[Long]] = getOptionalProperty(propName, rawConf, getLongProperty)
    if (!seed.isDefined)
      return None

    propName = "linIndepVectsDoNotFixDims"
    val linIndepVectsDoNotFixDims : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!linIndepVectsDoNotFixDims.isDefined)
      return None

    propName = "simplifySchedTrees"
    val simplifySchedTrees : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!simplifySchedTrees.isDefined)
      return None

    propName = "splitLoopBodies"
    val splitLoopBodies : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!splitLoopBodies.isDefined)
      return None

    propName = "numCompilatonDurationMeasurements"
    val numCompilatonDurationMeasurements : Option[Int] = getIntProperty(propName, rawConf)
    if (!numCompilatonDurationMeasurements.isDefined)
      return None
    if (!MinimalConfig.checkMin(0, numCompilatonDurationMeasurements, propName))
      return None

    propName = "validateOutput"
    val validateOutput : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!validateOutput.isDefined)
      return None

    propName = "tilingPermitInnerSeq"
    val tilingPermitInnerSeq : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!tilingPermitInnerSeq.isDefined)
      return None

    propName = "schedTreeSimplRebuildDimScheds"
    val schedTreeSimplRebuildDimScheds : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!schedTreeSimplRebuildDimScheds.isDefined)
      return None

    propName = "schedTreeSimplRemoveCommonOffset"
    val schedTreeSimplRemoveCommonOffset : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!schedTreeSimplRemoveCommonOffset.isDefined)
      return None

    propName = "schedTreeSimplDivideCoeffsByGCD"
    val schedTreeSimplDivideCoeffsByGCD : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!schedTreeSimplDivideCoeffsByGCD.isDefined)
      return None

    propName = "schedTreeSimplElimSuperfluousSubTrees"
    val schedTreeSimplElimSuperfluousSubTrees : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!schedTreeSimplElimSuperfluousSubTrees.isDefined)
      return None

    propName = "schedTreeSimplElimSuperfluousDimNodes"
    val schedTreeSimplElimSuperfluousDimNodes : Option[Boolean] = getBooleanProperty(propName, rawConf)
    if (!schedTreeSimplElimSuperfluousDimNodes.isDefined)
      return None

    propName = "barvinokBinary"
    val barvinokBinary : Option[File] = MinimalConfig.getFileProperty(propName, rawConf)
    if (!barvinokBinary.isDefined) return None
//    if (!Util.checkFileExistsAndHasRequiredPermissions(true, false, true, false,
//      barvinokBinary.get))
//      return None

    propName = "barvinokLibraryPath"
    val barvinokLibraryPath : Option[File] = MinimalConfig.getFileProperty(propName, rawConf)
    if (!barvinokLibraryPath.isDefined) return None
    if (!Util.checkFileExistsAndHasRequiredPermissions(true, false, true, true,
      barvinokLibraryPath.get))
      return None

    propName = "normalizeFeatures"
    val normalizeFeatures : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!normalizeFeatures.isDefined)
      return None
      
    propName= "gpu"
    val gpu : Option[Boolean] = MinimalConfig.getBooleanProperty(propName, rawConf)
    if (!gpu.isDefined)
      return None

    return Some(new MinConfig(
      numMeasurementThreads.get,
      rayCoeffsRange.get,
      lineCoeffsRange.get,
      maxNumRays.get,
      maxNumLines.get,
      probabilityToCarryDep.get,
      maxNumSchedsAtOnce.get,
      measurementCommand.get,
      measurementWorkingDir.get,
      measurementTmpDirBase.get,
      benchmarkName.get,
      referenceOutputFile.get,
      numExecutionTimeMeasurements.get,
      populationFilePrefix.get,
      exportSchedulesToJSCOPFilesEnabled.get,
      jscopFolderPrefix,
      measurementTimeout.get,
      exportPopulationToCSV.get,
      csvFilePrefix,
      logToFile.get,
      logFile,
      evaluationSigIntExitCode.get,
      randSchedsTimeout.get,
      measurementTmpDirNamePrefix.get,
      genSchedsMaxAllowedConseqFailures.get,
      numScheduleGenThreads.get,
      filterImportedPopulation.get,
      islComputeout.get,
      paramValMappings.toMap,
      measureParExecTime.get,
      measureSeqExecTime.get,
      moveVertices.get,
      rayPruningThreshold.get,
      insertSetNodes.get,
      compilationTimeout.get,
      benchmarkingSurrenderTimeout.get,
      measureCacheHitRateSeq.get,
      measureCacheHitRatePar.get,
      seed.get,
      linIndepVectsDoNotFixDims.get,
      simplifySchedTrees.get,
      splitLoopBodies.get,
      numCompilatonDurationMeasurements.get,
      validateOutput.get,
      tilingPermitInnerSeq.get,
      schedTreeSimplRebuildDimScheds.get,
      schedTreeSimplRemoveCommonOffset.get,
      schedTreeSimplDivideCoeffsByGCD.get,
      schedTreeSimplElimSuperfluousSubTrees.get,
      schedTreeSimplElimSuperfluousDimNodes.get,
      barvinokBinary.get,
      barvinokLibraryPath.get,
      normalizeFeatures.get,
      gpu.get))
  }

  /**
    * Retrieve the property named {@code propName} from {@rawConf} and try to parse it as a value of type
    * {@code NumGeneratorsLimit}.
    */
  def parseNumGeneratorsLimit(propName : String, rawConf : Properties) : Option[NumGeneratorsLimit] = {
    var propValStr : String = getProperty(propName, rawConf) match {
      case None    => return None
      case Some(s) => s
    }
    propValStr match {
      case "ALL"  => return Some(MinimalConfig.AllGenerators)
      case "RAND" => return Some(MinimalConfig.RandLimit)
      case _ => {
        getIntProperty(propName, rawConf) match {
          case None => return None
          case s @ Some(n) => {
            if (!checkMin(0, s, propName))
              return None
            return Some(MinimalConfig.LimitedGenerators(n))
          }
        }
      }
    }
  }

  /**
    * Checks whether v.get >= min is true. Logs a warning if this is not true.
    * v must be defined.
    */
  def checkMin(min : Int, v : Option[Int], prop : String) : Boolean = Util
    .checkMin(min, v.get, prop)

  /**
    * Checks whether v.get >= min is true. Logs a warning if this is not true.
    * v must be defined.
    */
  def checkMin(min : Double, v : Option[Double], prop : String) : Boolean = Util
    .checkMin(min, v.get, prop)

  /**
    * Checks whether v.get >= min is true. Logs a warning if this is not true.
    * v must be defined.
    */
  def checkMin(min : Long, v : Option[Long], prop : String) : Boolean = Util
    .checkMin(min, v.get, prop)

  /**
    * Checks whether min <= v.get <= max is true. Logs a warning if this is not
    * true. v must be defined.
    */
  def checkMinMax(min : Double, max : Double, v : Option[Double],
    prop : String) : Boolean = Util.checkMinMax(min, max, v.get, prop)

  /**
    * Checks whether min <= v.get <= max is true. Logs a warning if this is not
    * true. v must be defined.
    */
  def checkMinMax(min : Long, max : Long, v : Option[Long],
    prop : String) : Boolean = Util.checkMinMax(min, max, v.get, prop)

  /**
    * Checks whether min <= v.get <= max is true. Logs a warning if this is not
    * true. v must be defined.
    */
  def checkMinMax(min : Int, max : Int, v : Option[Int],
    prop : String) : Boolean = Util.checkMinMax(min, max, v.get, prop)

  /**
    * Load property key from props. Logs a warning if key does not exist.
    *
    * @return Returns Some(v) if value v for key exists. Otherwise returns None.
    */
  def getProperty(key : String, props : Properties) : Option[String] = {
    if (!props.containsKey(key)) {
      complainAboutMissingProp(key)
      return None
    }
    return Some(props.getProperty(key))
  }

  /**
    * Load property key from props. The value of the property may either be a value of type {@code T} or NONE. In the
    * latter case {@code None} is returned. Logs a warning if key does not exist or if parsing has failed.
    */
  def getOptionalProperty[T](key : String, props : Properties, parser : (String, Properties) => Option[T]) : Option[Option[T]] = {
    return getProperty(key, props) match {
      case None => None
      case Some(s) => {
        s match {
          case "NONE" => Some(None)
          case _ => parser(key, props) match {
            case None        => None
            case s @ Some(_) => Some(s)
          }
        }
      }
    }
  }

  /**
    * Logs that property key is missing.
    */
  def complainAboutMissingProp(key : String) {
    myLogger.warning("The configuration property \"" + key + "\" is missing.")
  }

  /**
    * Load a boolean property. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @return Returns Some(v) if value v for key exists. Otherwise returns None.
    */
  def getBooleanProperty(key : String, props : Properties) : Option[Boolean] = {
    getProperty(key, props) match {
      case None => return None
      case Some(v) => {
        return ParseArgs.parseBoolean(v, key)
      }
    }
  }

  /**
    * Load a file path property. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @return Returns Some(v) if value v for key exists. Otherwise returns None.
    */
  def getFileProperty(key : String, props : Properties) : Option[File] = {
    val s : Option[String] = getProperty(key, props)
    s match {
      case None    => None
      case Some(x) => Some(new File(x))
    }
  }

  /**
    * Load a double property. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @return Returns Some(v) if value v for key exists. Otherwise returns None.
    */
  def getDoubleProperty(key : String,
    props : Properties) : Option[Double] = getNumberProperty(key, props,
    ParseArgs.parseDouble, "double")

  /**
    * Load a rational property. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @return Returns Some(v) if value v for key exists. Otherwise returns None.
    */
  def getRatProperty(key : String, props : Properties) : Option[Rat] =
    getNumberProperty(key, props, ParseArgs.parseRational, "rational")

  /**
    * Load an int property. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @return Returns Some(v) if value v for key exists. Otherwise returns None.
    */
  def getIntProperty(key : String,
    props : Properties) : Option[Int] = getNumberProperty(key, props,
    ParseArgs.parseInt, "integer")

  /**
    * Load a long int property. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @return Returns Some(v) if value v for key exists. Otherwise returns None.
    */
  def getLongProperty(key : String,
    props : Properties) : Option[Long] = getNumberProperty(key, props,
    ParseArgs.parseLong, "long integer")

  private def getNumberProperty[T](key : String, props : Properties,
    convert : (String, String) => Option[T], typeName : String) : Option[T] = {
    getProperty(key, props) match {
      case None => return None
      case Some(v) => {
        return convert(v, key)
      }
    }
  }

  def toStringAppendOptional(name : String, v : Option[Any], sb : StringBuilder) {
    sb.append(name).append("=").append(if (v.isDefined) v.get.toString else "NONE").append('\n')
  }

  def toStringAppend(name : String, v : Any, sb : StringBuilder) {
    sb.append(name).append("=").append(v.toString).append('\n')
  }
}

abstract class MinimalConfig(
    val numMeasurementThreads : Int,
    val rayCoeffsRange : Int,
    val lineCoeffsRange : Int,
    val maxNumRays : MinimalConfig.NumGeneratorsLimit,
    val maxNumLines : MinimalConfig.NumGeneratorsLimit,
    val probabilityToCarryDep : Double,
    val maxNumSchedsAtOnce : Int,
    val measurementCommand : String,
    val measurementWorkingDir : File,
    val measurementTmpDirBase : File,
    val benchmarkName : String,
    val referenceOutputFile : File,
    val numExecutionTimeMeasurements : Int,
    val populationFilePrefix : String,
    val exportSchedulesToJSCOPFiles : Boolean,
    val jscopFolderPrefix : String,
    val measurementTimeout : Long,
    val exportPopulationToCSV : Boolean,
    val csvFilePrefix : String,
    val logToFile : Boolean,
    val logFile : File,
    val evaluationSigIntExitCode : Int,
    val randSchedsTimeout : Long,
    val measurementTmpDirNamePrefix : String,
    val genSchedsMaxAllowedConseqFailures : Int,
    val numScheduleGenThreads : Int,
    val filterImportedPopulation : Boolean,
    val islComputeout : Int,
    val paramValMappings : Map[String, Int],
    val measureParExecTime : Boolean,
    val measureSeqExecTime : Boolean,
    val moveVertices : Boolean,
    val rayPruningThreshold : Option[Rat],
    val insertSetNodes : Boolean,
    val compilationTimeout : Option[Long],
    val benchmarkingSurrenderTimeout : Option[Double],
    val measureCacheHitRateSeq : Boolean,
    val measureCacheHitRatePar : Boolean,
    val seed : Option[Long],
    val linIndepVectsDoNotFixDims : Boolean,
    val simplifySchedTrees : Boolean,
    val splitLoopBodies : Boolean,
    val numCompilatonDurationMeasurements : Int,
    val validateOutput : Boolean,
    val tilingPermitInnerSeq : Boolean,
    val schedTreeSimplRebuildDimScheds : Boolean,
    val schedTreeSimplRemoveCommonOffset : Boolean,
    val schedTreeSimplDivideCoeffsByGCD : Boolean,
    val schedTreeSimplElimSuperfluousSubTrees : Boolean,
    val schedTreeSimplElimSuperfluousDimNodes : Boolean,
    val barvinokBinary : File,
    val barvinokLibraryPath : File,
    val normalizeFeatures : Boolean,
    val gpu : Boolean) {

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder

    MinimalConfig.toStringAppend("numMeasurementThreads", numMeasurementThreads, sb)
    MinimalConfig.toStringAppend("rayCoeffsRange", rayCoeffsRange, sb)
    MinimalConfig.toStringAppend("lineCoeffsRange", lineCoeffsRange, sb)
    MinimalConfig.toStringAppend("maxNumRays", maxNumRays, sb)
    MinimalConfig.toStringAppend("maxNumLines", maxNumLines, sb)
    MinimalConfig.toStringAppend("probabilityToCarryDep", probabilityToCarryDep, sb)
    MinimalConfig.toStringAppend("maxNumSchedsAtOnce", maxNumSchedsAtOnce, sb)
    MinimalConfig.toStringAppend("measurementCommand", measurementCommand, sb)
    MinimalConfig.toStringAppend("measurementWorkingDir", measurementWorkingDir, sb)
    MinimalConfig.toStringAppend("measurementTmpDirBase", measurementTmpDirBase, sb)
    MinimalConfig.toStringAppend("benchmarkName", benchmarkName, sb)
    MinimalConfig.toStringAppend("referenceOutputFile", referenceOutputFile, sb)
    MinimalConfig.toStringAppend("numExecutionTimeMeasurements", numExecutionTimeMeasurements, sb)
    MinimalConfig.toStringAppend("populationFilePrefix", populationFilePrefix, sb)
    MinimalConfig.toStringAppend("exportSchedulesToJSCOPFiles", exportSchedulesToJSCOPFiles, sb)
    if (exportSchedulesToJSCOPFiles)
      MinimalConfig.toStringAppend("jscopFolderPrefix", jscopFolderPrefix, sb)
    MinimalConfig.toStringAppend("measurementTimeout", measurementTimeout, sb)
    MinimalConfig.toStringAppend("exportPopulationToCSV", exportPopulationToCSV, sb)
    if (exportPopulationToCSV)
      MinimalConfig.toStringAppend("csvFilePrefix", csvFilePrefix, sb)
    MinimalConfig.toStringAppend("logToFile", logToFile, sb)
    if (logToFile)
      MinimalConfig.toStringAppend("logFile", logFile, sb)
    MinimalConfig.toStringAppend("evaluationSigIntExitCode", evaluationSigIntExitCode, sb)
    MinimalConfig.toStringAppend("randSchedsTimeout", randSchedsTimeout, sb)
    MinimalConfig.toStringAppend("measurementTmpDirNamePrefix", measurementTmpDirNamePrefix, sb)
    MinimalConfig.toStringAppend("genSchedsMaxAllowedConseqFailures", genSchedsMaxAllowedConseqFailures, sb)
    MinimalConfig.toStringAppend("numScheduleGenThreads", numScheduleGenThreads, sb)
    MinimalConfig.toStringAppend("filterImportedPopulation", filterImportedPopulation, sb)
    MinimalConfig.toStringAppend("islComputeout", islComputeout, sb)
    MinimalConfig.toStringAppend("paramValMappings", paramValMappings, sb)
    MinimalConfig.toStringAppend("measureSeqExecTime", measureSeqExecTime, sb)
    MinimalConfig.toStringAppend("moveVertices", moveVertices, sb)
    MinimalConfig.toStringAppendOptional("rayPruningThreshold", rayPruningThreshold, sb)
    MinimalConfig.toStringAppend("insertSetNodes", insertSetNodes, sb)
    MinimalConfig.toStringAppend("compilationTimeout", compilationTimeout, sb)
    MinimalConfig.toStringAppendOptional("benchmarkingSurrenderTimeout", benchmarkingSurrenderTimeout, sb)
    MinimalConfig.toStringAppend("measureCacheHitRateSeq", measureCacheHitRateSeq, sb)
    MinimalConfig.toStringAppend("measureCacheHitRatePar", measureCacheHitRatePar, sb)
    MinimalConfig.toStringAppendOptional("seed", seed, sb)
    MinimalConfig.toStringAppend("linIndepVectsDoNotFixDims", linIndepVectsDoNotFixDims, sb)
    MinimalConfig.toStringAppend("simplifySchedTrees", simplifySchedTrees, sb)
    MinimalConfig.toStringAppend("splitLoopBodies", splitLoopBodies, sb)
    MinimalConfig.toStringAppend("numCompilatonDurationMeasurements", numCompilatonDurationMeasurements, sb)
    MinimalConfig.toStringAppend("validateOutput", validateOutput, sb)
    MinimalConfig.toStringAppend("tilingPermitInnerSeq", tilingPermitInnerSeq, sb)
    MinimalConfig.toStringAppend("schedTreeSimplRebuildDimScheds", schedTreeSimplRebuildDimScheds, sb)
    MinimalConfig.toStringAppend("schedTreeSimplRemoveCommonOffset", schedTreeSimplRemoveCommonOffset, sb)
    MinimalConfig.toStringAppend("schedTreeSimplDivideCoeffsByGCD", schedTreeSimplDivideCoeffsByGCD, sb)
    MinimalConfig.toStringAppend("schedTreeSimplElimSuperfluousSubTrees", schedTreeSimplElimSuperfluousSubTrees, sb)
    MinimalConfig.toStringAppend("schedTreeSimplElimSuperfluousDimNodes", schedTreeSimplElimSuperfluousDimNodes, sb)
    MinimalConfig.toStringAppend("barvinokBinary", barvinokBinary, sb)
    MinimalConfig.toStringAppend("barvinokLibraryPath", barvinokLibraryPath, sb)
    MinimalConfig.toStringAppend("normalizeFeatures", normalizeFeatures, sb)
    MinimalConfig.toStringAppend("gpu", gpu, sb)
    return sb.toString()
  }
}

/**
  * Basic implementation of {@code MinimalConfig}.
  */
class MinConfig(
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
  insertSetNodes : Boolean,
  compilationTimeout : Option[Long],
  benchmarkingSurrenderTimeout : Option[Double],
  measureCacheHitRateSeq : Boolean,
  measureCacheHitRatePar : Boolean,
  seed : Option[Long],
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
  gpu : Boolean) extends MinimalConfig(
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
  gpu) {
}
