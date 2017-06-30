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

/**
  * Loads a list of population files and merges the first population with the rest.
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

    val numSchedTreeSimplDurationMeasurements : Int = args(3).toInt
    val numCompilatonDurationMeasurements : Int = args(4).toInt
    val numExecutionTimeMeasurements : Int = args(5).toInt

    val populationFiles : List[File] = args.drop(6).toList.map(new File(_))

    val populations : List[HashMap[Schedule, EvalResult]] = populationFiles.map((f : File) => {
      myLogger.info("Loading schedules from " + f)
      val (scheds2Results : HashMap[Schedule, EvalResult], generation2 : Int) = load(f, domInfo, deps) match {
        case None    => return
        case Some(p) => p
      }
      scheds2Results
    })

    myLogger.info("Merging populations.")

    val result : HashMap[Schedule, EvalResult] = HashMap.empty

    for (scheds2Results : HashMap[Schedule, EvalResult] <- populations)
      for ((s : Schedule, res : EvalResult) <- scheds2Results)
        result.put(s, res)

    myLogger.info("The merged set contains " + result.size + " schedules.")
    myLogger.info("JSON export.")
    ScheduleExport.exportPopulationToFile(outputJSONFile, result, 0)
    myLogger.info("CSV export")
    ScheduleExport.exportPopulationToCSVVectUnionMap(outputCSVFile,
      numCompilatonDurationMeasurements,
      numSchedTreeSimplDurationMeasurements,
      numExecutionTimeMeasurements,
      result.map(t => (t._1.getSchedule, Some(t._2), Option.empty[FeatureVect])), true, List.empty, 0)
  }

  myLogger.info("Storing the merged population")

  private def load(f : File, domInfo : DomainCoeffInfo, deps : Set[Dependence]) : Option[(HashMap[Schedule, EvalResult], Int)] = {
    val (populationRaw : HashMap[Schedule, (Option[EvalResult], Option[FeatureVect])], generation : Int) = JSONLogExporter.readPopulationFromFile(f, domInfo, deps)
    val filtered : HashMap[Schedule, EvalResult] = populationRaw.map(t => {
      if (!t._2._1.isDefined) {
        myLogger.warning("schedule has no evaluation result: " + t._1)
        return None
      }
      (t._1, t._2._1.get)
    })
    return Some((filtered, generation))
  }

  def checkNumArgs(args : Array[String]) : Boolean = {
    if (args.length < 7) {
      myLogger.warning("Expected at least seven arguments: <JSCOP file> <output JSON file> <output CSV file> <# schedule tree simplification measurements> <# compile duration measurements> <# execution time measurements> <population file>+")
      return false
    }
    return true
  }
}