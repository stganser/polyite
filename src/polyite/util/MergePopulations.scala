package polyite.util

import polyite.MainUtil
import java.io.File
import polyite.ScopInfo
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.export.JSONExporter
import scala.collection.mutable.HashMap
import polyite.schedule.Schedule
import polyite.sched_eval.EvalResult
import java.util.logging.Logger
import polyite.fitness.FeatureVect
import polyite.export.ScheduleExport
import polyite.export.JSONLogExporter
import polyite.sched_eval.Fitness
import polyite.fitness.Prediction
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import polyite.sched_eval.EvalResultOnly

/**
  * Loads a list of population files and merges the first population with the rest. Drops predictions and feature vectors.
  */
object MergePopulations {

  val myLogger : Logger = Logger.getLogger("")

  def main(args : Array[String]) : Unit = {

    if (!checkNumArgs(args))
      return

    val jscopFilePath : String = args(0)
    val (_ : File, scop : ScopInfo) = MainUtil.loadScop(jscopFilePath) match {
      case None    => return
      case Some(t) => t
    }
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)

    val outputJSONFile : File = new File(args(1))
    val outputCSVFile : File = new File(args(2))

    val numCompilatonDurationMeasurements : Int = args(3).toInt
    val numExecutionTimeMeasurements : Int = args(4).toInt

    val populationFiles : List[File] = args.drop(5).toList.map(new File(_))

    val populations : List[HashMap[Schedule, Fitness]] = populationFiles.map((f : File) => {
      myLogger.info("Loading schedules from " + f)
      val (scheds2Results : HashMap[Schedule, Fitness], generation2 : Int) = load(f, domInfo, deps)
      scheds2Results
    })

    myLogger.info("Merging populations.")

    val scheds2EvalResults : HashMap[Schedule, EvalResult] = HashMap.empty

    for (scheds2Results : HashMap[Schedule, Fitness] <- populations)
      for ((s : Schedule, fit : Fitness) <- scheds2Results)
        if (fit.getEvalResult.isDefined)
          scheds2EvalResults.put(s, fit.getEvalResult.get)
    myLogger.info("The merged set contains " + scheds2EvalResults.size + " schedules.")
    myLogger.info("JSON export.")
    ScheduleExport.exportPopulationToFile(outputJSONFile, scheds2EvalResults.map(t => (t._1, EvalResultOnly(t._2))), 0)
    myLogger.info("CSV export")
    ScheduleExport.exportPopulationToCSVUnionMap(outputCSVFile,
      numCompilatonDurationMeasurements,
      0,
      numExecutionTimeMeasurements,
      scheds2EvalResults.map(t => (t._1.getSchedule, EvalResultOnly(t._2))), true, false, List.empty, 0)
  }

  myLogger.info("Storing the merged population")

  private def load(f : File, domInfo : DomainCoeffInfo, deps : Set[Dependence]) : (HashMap[Schedule, Fitness], Int) = {
    val (populationRaw : HashMap[Schedule, Fitness], generation : Int) = JSONLogExporter.readPopulationFromFile(f, domInfo, deps)
    return (populationRaw, generation)
  }

  def checkNumArgs(args : Array[String]) : Boolean = {
    if (args.length < 6) {
      myLogger.warning("Expected at least six arguments: <JSCOP file> <output JSON file> <output CSV file> <# compile duration measurements> <# execution time measurements> <population files>")
      return false
    }
    return true
  }
}