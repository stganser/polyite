package polyite.config

import java.util.Properties
import org.junit.Before
import java.io.File
import polyite.util.Rat
import org.junit.Test
import polyite.evolution.GeneticOperatorFactory.GeneticOperators
import java.util.logging.Logger

class ConfigGATest {

  val myLogger : Logger = Logger.getLogger("")
  val props : Properties = new Properties()

  @Before
  def prepare() {
    val refOutputDummy : File = new File("/tmp/refOutDummy")
    refOutputDummy.createNewFile()
    refOutputDummy.deleteOnExit()

    var i : Int = 1
    props.put("numMeasurementThreads", i.toString())
    i += 1
    props.put("rayCoeffsRange", i.toString())
    i += 1
    props.put("lineCoeffsRange", i.toString())
    i += 1
    props.put("maxNumRays", "ALL")
    i += 1
    props.put("maxNumLines", i.toString())
    i += 1
    props.put("probabilityToCarryDep", f"0.${i}1")
    i += 1
    props.put("maxNumSchedsAtOnce", i.toString())
    i += 1
    props.put("measurementCommand", i.toString())
    i += 1
    props.put("measurementWorkingDir", "/tmp")
    i += 1
    props.put("measurementTmpDirBase", i.toString())
    i += 1
    props.put("benchmarkName", i.toString())
    i += 1
    props.put("functionName", i.toString())
    i += 1
    props.put("scopRegionStart", i.toString())
    i += 1
    props.put("scopRegionEnd", i.toString())
    i += 1
    props.put("irFilesLocation", "/tmp")
    i += 1
    props.put("referenceOutputFile", refOutputDummy.toString())
    i += 1
    props.put("numExecutionTimeMeasurements", i.toString())
    i += 1
    props.put("populationFilePrefix", i.toString())
    i += 1
    props.put("exportSchedulesToJSCOPFiles", "true")
    i += 1
    props.put("jscopFolderPrefix", i.toString())
    i += 1
    props.put("measurementTimeout", i.toString())
    i += 1
    props.put("exportPopulationToCSV", "true")
    i += 1
    props.put("csvFilePrefix", i.toString())
    i += 1
    props.put("logToFile", "true")
    i += 1
    props.put("logFile", i.toString())
    i += 1
    props.put("evaluationSigIntExitCode", i.toString())
    i += 1
    props.put("randSchedsTimeout", i.toString())
    i += 1
    props.put("measurementTmpDirNamePrefix", i.toString())
    i += 1
    props.put("genSchedsMaxAllowedConseqFailures", i.toString())
    i += 1
    props.put("probabilityToMutateSchedRow", f"0.${i}1")
    i += 1
    props.put("probabilityToMutateGeneratorCoeff", f"0.${i}1")
    i += 1
    props.put("generatorCoeffMaxDenominator", i.toString())
    i += 1
    props.put("currentGeneration", i.toString())
    i += 1
    props.put("maxGenerationToReach", i.toString())
    i += 1
    props.put("regularPopulationSize", i.toString())
    i += 1
    props.put("fractionOfSchedules2Keep", Rat(1, i).toString())
    i += 1
    props.put("maxNumNewSchedsFromCrossover", i.toString())
    i += 1
    props.put("optimizeParallelExecTime", "true")
    i += 1
    props.put("filterImportedPopulation", "false")
    i += 1
    props.put("evolutionTimeout", i.toString())
    i += 1
    props.put("shareOfRandSchedsInPopulation", Rat(2, i).toString())
    i += 1
    props.put("numScheduleGenThreads", i.toString())
    i += 1
    props.put("islComputeout", i.toString())
    props.put("paramValMappings", "a=3,b=4")
    props.put("measureSeqExecTime", "true")
    props.put("measureParExecTime", "false")
    props.put("moveVertices", "false")
    props.put("rayPruningThreshold", "NONE")

    props.put("replaceDimsEnabled", "true")
    props.put("replacePrefixEnabled", "false")
    props.put("replaceSuffixEnabled", "true")
    props.put("mutateGeneratorCoeffsEnabled", "false")
    props.put("geometricCrossoverEnabled", "true")
    props.put("rowCrossoverEnabled", "false")
    i += 1
    props.put("initPopulationNumLines", i.toString())
    i += 1
    props.put("initPopulationNumRays", "ALL")

    props.put("seqPollyOptFlags", "bliblablubb bla -blubb")
    props.put("parPollyOptFlags", "Hallo -Welt=43 droeflf")
    props.put("insertSetNodes", "false")
    i += 1
    props.put("compilationTimeout", i.toString)
    i += 1
    props.put("benchmarkingSurrenderTimeout", f"${i.toString}.0")
    props.put("measureCacheHitRateSeq", "true")
    props.put("measureCacheHitRatePar", "false")
    props.put("useConvexAnnealingFunction", "true")
    props.put("seed", "123")
    props.put("numactlConf", "NONE")
    props.put("linIndepVectsDoNotFixDims", "false")
    props.put("simplifySchedTrees", "true")
    props.put("splitLoopBodies", "false")
    i += 1
    props.put("numCompilatonDurationMeasurements", i.toString)
    props.put("validateOutput", "false")
    props.put("tilingPermitInnerSeq", "true")
    props.put("schedTreeSimplRebuildDimScheds", "true")
    props.put("schedTreeSimplRemoveCommonOffset", "false")
    props.put("schedTreeSimplDivideCoeffsByGCD", "true")
    props.put("schedTreeSimplElimSuperfluousSubTrees", "false")
    props.put("schedTreeSimplElimSuperfluousDimNodes", "true")
    props.put("barvinokBinary", "/bin/true")
    props.put("barvinokLibraryPath", "/lib")
    props.put("normalizeFeatures", "false")
  }

  @Test
  def test() {
    val conf : ConfigGA = ConfigGA.parseConfig(props) match {
      case None => {
        myLogger.warning("Failed to load the configuration.")
        return
      } case Some(c) => c
    }

    assert(conf.numMeasurementThreads.toString().equals(props.getProperty("numMeasurementThreads")))
    assert(conf.rayCoeffsRange.toString().equals(props.getProperty("rayCoeffsRange")))
    assert(conf.lineCoeffsRange.toString().equals(props.getProperty("lineCoeffsRange")))
    assert(conf.maxNumRays.toString().equals(props.getProperty("maxNumRays")))
    assert(conf.maxNumLines.toString().equals(props.getProperty("maxNumLines")))
    assert(conf.probabilityToCarryDep.toString().equals(props.getProperty("probabilityToCarryDep")))
    assert(conf.maxNumSchedsAtOnce.toString().equals(props.getProperty("maxNumSchedsAtOnce")))
    assert(conf.measurementCommand.toString().equals(props.getProperty("measurementCommand")))
    assert(conf.measurementWorkingDir.toString().equals(props.getProperty("measurementWorkingDir")))
    assert(conf.measurementTmpDirBase.toString().equals(props.getProperty("measurementTmpDirBase")))
    assert(conf.benchmarkName.toString().equals(props.getProperty("benchmarkName")))
    assert(conf.functionName.toString().equals(props.getProperty("functionName")))
    assert(conf.scopRegionStart.toString().equals(props.getProperty("scopRegionStart")))
    assert(conf.scopRegionEnd.toString().equals(props.getProperty("scopRegionEnd")))
    assert(conf.irFilesLocation.toString().equals(props.getProperty("irFilesLocation")))
    assert(conf.referenceOutputFile.toString().equals(props.getProperty("referenceOutputFile")))
    assert(conf.numExecutionTimeMeasurements.toString().equals(props.getProperty("numExecutionTimeMeasurements")))
    assert(conf.populationFilePrefix.toString().equals(props.getProperty("populationFilePrefix")))
    assert(conf.exportSchedulesToJSCOPFiles.toString().equals(props.getProperty("exportSchedulesToJSCOPFiles")))
    assert(conf.jscopFolderPrefix.toString().equals(props.getProperty("jscopFolderPrefix")))
    assert(conf.measurementTimeout.toString().equals(props.getProperty("measurementTimeout")))
    assert(conf.exportPopulationToCSV.toString().equals(props.getProperty("exportPopulationToCSV")))
    assert(conf.csvFilePrefix.toString().equals(props.getProperty("csvFilePrefix")))
    assert(conf.logToFile.toString().equals(props.getProperty("logToFile")))
    assert(conf.logFile.toString().equals(props.getProperty("logFile")))
    assert(conf.evaluationSigIntExitCode.toString().equals(props.getProperty("evaluationSigIntExitCode")))
    assert(conf.randSchedsTimeout.toString().equals(props.getProperty("randSchedsTimeout")))
    assert(conf.measurementTmpDirNamePrefix.toString().equals(props.getProperty("measurementTmpDirNamePrefix")))
    assert(conf.genSchedsMaxAllowedConseqFailures.toString().equals(props.getProperty("genSchedsMaxAllowedConseqFailures")))
    assert(conf.probabilityToMutateSchedRow.toString().equals(props.getProperty("probabilityToMutateSchedRow")))
    assert(conf.probabilityToMutateGeneratorCoeff.toString().equals(props.getProperty("probabilityToMutateGeneratorCoeff")))
    assert(conf.generatorCoeffMaxDenominator.toString().equals(props.getProperty("generatorCoeffMaxDenominator")))
    assert(conf.currentGeneration.toString().equals(props.getProperty("currentGeneration")))
    assert(conf.maxGenerationToReach.toString().equals(props.getProperty("maxGenerationToReach")))
    assert(conf.regularPopulationSize.toString().equals(props.getProperty("regularPopulationSize")))
    assert(conf.fractionOfSchedules2Keep.toString().equals(props.getProperty("fractionOfSchedules2Keep")))
    assert(conf.maxNumNewSchedsFromCrossover.toString().equals(props.getProperty("maxNumNewSchedsFromCrossover")))
    assert(conf.optimizeParallelExecTime.toString().equals(props.getProperty("optimizeParallelExecTime")))
    assert(conf.filterImportedPopulation.toString().equals(props.getProperty("filterImportedPopulation")))
    assert(conf.evolutionTimeout.toString().equals(props.getProperty("evolutionTimeout")))
    assert(conf.shareOfRandSchedsInPopulation.toString().equals(props.getProperty("shareOfRandSchedsInPopulation")))
    assert(conf.numScheduleGenThreads.toString().equals(props.getProperty("numScheduleGenThreads")))
    assert(conf.islComputeout.toString().equals(props.getProperty("islComputeout")))
    assert(conf.measureSeqExecTime.toString().equals(props.getProperty("measureSeqExecTime")))
    assert(conf.measureParExecTime.toString().equals(props.getProperty("measureParExecTime")))
    assert(conf.moveVertices.toString().equals(props.getProperty("moveVertices")))
    assert((if (conf.rayPruningThreshold.isDefined) conf.rayPruningThreshold.get.intCeil.toString() else "NONE") equals (props.getProperty("rayPruningThreshold")))
    assert(conf.seqPollyOptFlags.toString().equals(props.getProperty("seqPollyOptFlags")))
    assert(conf.parPollyOptFlags.toString().equals(props.getProperty("parPollyOptFlags")))
    assert(conf.insertSetNodes.toString().equals(props.getProperty("insertSetNodes")))
    assert((if (conf.compilationTimeout.isDefined) conf.compilationTimeout.get.toString() else "NONE") equals (props.getProperty("compilationTimeout")))
    assert((if (conf.benchmarkingSurrenderTimeout.isDefined) conf.benchmarkingSurrenderTimeout.get.toString() else "NONE") equals (props.getProperty("benchmarkingSurrenderTimeout")))
    assert((if (conf.seed.isDefined) conf.seed.get.toString() else "NONE") equals (props.getProperty("seed")))
    assert(conf.measureCacheHitRateSeq.toString().equals(props.getProperty("measureCacheHitRateSeq")))
    assert(conf.measureCacheHitRatePar.toString().equals(props.getProperty("measureCacheHitRatePar")))

    assert(conf.activeMutators.contains(GeneticOperators.ReplaceDims))
    assert(!conf.activeMutators.contains(GeneticOperators.ReplacePrefix))
    assert(conf.activeMutators.contains(GeneticOperators.ReplaceSuffix))
    assert(!conf.activeMutators.contains(GeneticOperators.MutateGeneratorCoeffs))
    assert(conf.activeCrossovers.contains(GeneticOperators.GeometricCrossover))
    assert(!conf.activeCrossovers.contains(GeneticOperators.RowCrossover))

    assert(conf.initPopulationNumLines.toString().equals(props.getProperty("initPopulationNumLines")))
    assert(conf.initPopulationNumRays.toString().equals(props.getProperty("initPopulationNumRays")))
    assert(conf.useConvexAnnealingFunction.toString().equals(props.getProperty("useConvexAnnealingFunction")))
    assert((if (conf.numactlConf.isDefined) conf.numactlConf.get.toString() else "NONE") equals (props.getProperty("numactlConf")))
    assert(conf.linIndepVectsDoNotFixDims.toString().equals(props.getProperty("linIndepVectsDoNotFixDims")))
    assert(conf.simplifySchedTrees.toString().equals(props.getProperty("simplifySchedTrees")))
    assert(conf.splitLoopBodies.toString().equals(props.getProperty("splitLoopBodies")))
    assert(conf.numCompilatonDurationMeasurements.toString().equals(props.getProperty("numCompilatonDurationMeasurements")))
    assert(conf.tilingPermitInnerSeq.toString().equals(props.getProperty("tilingPermitInnerSeq")))
    assert(conf.validateOutput.toString().equals(props.getProperty("validateOutput")))
    assert(conf.schedTreeSimplRebuildDimScheds.toString().equals(props.getProperty("schedTreeSimplRebuildDimScheds")))
    assert(conf.schedTreeSimplRemoveCommonOffset.toString().equals(props.getProperty("schedTreeSimplRemoveCommonOffset")))
    assert(conf.schedTreeSimplDivideCoeffsByGCD.toString().equals(props.getProperty("schedTreeSimplDivideCoeffsByGCD")))
    assert(conf.schedTreeSimplElimSuperfluousSubTrees.toString().equals(props.getProperty("schedTreeSimplElimSuperfluousSubTrees")))
    assert(conf.schedTreeSimplElimSuperfluousDimNodes.toString().equals(props.getProperty("schedTreeSimplElimSuperfluousDimNodes")))
    assert(conf.barvinokBinary.toString().equals(props.getProperty("barvinokBinary")))
    assert(conf.barvinokLibraryPath.toString().equals(props.getProperty("barvinokLibraryPath")))
    assert(conf.normalizeFeatures.toString().equals(props.getProperty("normalizeFeatures")))
  }
}