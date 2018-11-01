package polyite.util.fitness

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.util.logging.Level
import java.util.logging.Logger

import isl.Isl
import polyite.MainUtil
import polyite.ScopInfo
import polyite.config.Config
import polyite.config.ConfigRand
import polyite.fitness.Feature
import polyite.fitness.FeatureVect
import polyite.fitness.TilableBands
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.schedule_tree.GroupDimsVisitor
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.schedule.schedule_tree.normalization.DivideCoeffsByGCDVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousDimNodesVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousSubTreesVisitor
import polyite.schedule.schedule_tree.normalization.RemoveCommonOffsetVisitor
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.util.CSVUtil
import polyite.util.SCoPMetrics

/**
  * Imports a set of schedules from CSV, calculates feature values for each schedule and exports the results to CSV.
  */
object FeatureCalculator {

  val myLogger : Logger = Logger.getLogger("")

  def main(args : Array[String]) : Unit = {
    Logger.getGlobal.setLevel(Level.INFO)
    if (!checkNumArgs(args))
      return
    val jscopFile : File = new File(args(0))
    val confFile : File = new File(args(1))
    val inputCSVFile : File = new File(args(2))
    val outputCSVFile : File = new File(args(3))

    val scop : ScopInfo = MainUtil.loadScop(jscopFile.getAbsolutePath) match {
      case None    => return
      case Some(t) => t._2
    }
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)

    val conf : ConfigRand = MainUtil.loadConfig(confFile.getAbsolutePath, ConfigRand.loadAndValidateConfig) match {
      case None    => return
      case Some(c) => c
    }

    myLogger.info("Loading the schedules.")

    val input : List[Map[String, String]] = CSVUtil.loadCSV(inputCSVFile, '\t')

    myLogger.info("Starting to parse " + input.size + " schedules.")

    val sched2ExecTime : Map[isl.UnionMap, Double] = input.zipWithIndex.map((t : (Map[String, String], Int)) => {
      val sched : isl.UnionMap = t._1.get("schedule") match {
        case None => {
          myLogger.warning("column schedule is missing from row " + (t._2 + 1))
          return
        }
        case Some(s) => isl.UnionMap.readFromStr(domInfo.ctx, s.replaceAll("\"", ""))
      }
      val completelyEvaluated : Boolean = CSVUtil.getBooleanValueFromColumnKey(t._1, "completelyEvaluated", t._2 + 1, inputCSVFile.getPath) match {
        case None    => return
        case Some(b) => b
      }
      val execTime : Option[Double] = if (completelyEvaluated)
        Some((0 until conf.numExecutionTimeMeasurements).map(n => f"t${n}par").map(CSVUtil.getDoubleValueFromColumnKey(t._1, _, t._2, inputCSVFile.getPath) match {
          case None    => return
          case Some(d) => d
        }).min)
      else
        None
      (sched, execTime)
    }).filter(_._2.isDefined).map(t => (t._1, t._2.get)).toMap

    val features : List[Feature] = if (Isl.islUnionSetGetTupleNames(scop.getDomain).size == 1)
      Feature.features.filterNot(_.isMultiStmt())
    else
      Feature.features

    myLogger.info("Calculating the features.")

    val scopSize : SCoPMetrics = SCoPMetrics.calc(scop)

    val populationEvaluated : List[(isl.UnionMap, Double, FeatureVect)] = sched2ExecTime.toList
      .map((t : (isl.UnionMap, Double)) => {
        val schedMap : isl.UnionMap = t._1
        val evalRes : Double = t._2
        myLogger.info("Calculating feature vector of schedule " + schedMap)
        val fVect : FeatureVect = calcFeatures(schedMap, None, None, domInfo, scop, scopSize, deps, features, conf)
        (schedMap, evalRes, fVect)
      })
    val minExecTime : Double = populationEvaluated.map(_._2).min
    val popEvalClassified : List[(isl.UnionMap, Double, FeatureVect, Int)] = populationEvaluated.map(t => {
      val schedClass : Int = if (t._2 <= minExecTime * 2)
        1
      else
        0
      (t._1, t._2, t._3, schedClass)
    })
    myLogger.info("Storing the results.")
    store(outputCSVFile, popEvalClassified, features)
  }

  private def store(outputCSVFile : File, data : List[(isl.UnionMap, Double, FeatureVect, Int)], features : List[Feature]) {
    val featuresSorted : List[Feature] = features.sorted
    val writer : BufferedWriter = new BufferedWriter(new FileWriter(outputCSVFile))
    writer.write("schedule\texecTime")
    for (feature <- featuresSorted) {
      writer.write("\t")
      writer.write(feature.toString)
    }
    writer.write("\tclass")
    for ((sched : isl.UnionMap, execTime : Double, fVect : FeatureVect, schedClass : Int) <- data) {
      writer.newLine()
      writer.write("\"")
      writer.write(sched.toString)
      writer.write("\"\t")
      writer.write(execTime.toString())
      for (feature <- featuresSorted) {
        writer.write("\t")
        writer.write(fVect.getFeature(feature).get.toString)
      }
      writer.write("\t")
      writer.write(schedClass.toString())
    }
    writer.close()
  }

  /**
    * Calculate the feature vector from the given schedule matrix and SCoP. The Function will calculate all of the features
    * in the given list. Optionally, tree representations of the schedule (one for the tiling feature and one for the other features) can be passed in order to avoid recalculating it.
    */
  def calcFeatures(s : isl.UnionMap, schedTreeTiling : Option[ScheduleNode], schedTree : Option[ScheduleNode], domInfo : DomainCoeffInfo, scop : ScopInfo, scopMetrics : SCoPMetrics,
    deps : Set[Dependence], features : List[Feature], conf : Config) : FeatureVect = {
    val schedTreeSome : ScheduleNode = schedTree.getOrElse(getSimplifiedSchedTree(s, domInfo, scop, deps, true))
    val schedTreeTilingSome : ScheduleNode = schedTreeTiling.getOrElse(getSimplifiedSchedTree(s, domInfo, scop, deps, conf.splitLoopBodies))
    return features.foldLeft(new FeatureVect())((v : FeatureVect, f : Feature) => {
      val fVal : Double = if (f == TilableBands) {
        f.calc(schedTreeTilingSome, conf, scop, scopMetrics, domInfo, deps)
      } else {
        f.calc(schedTreeSome, conf, scop, scopMetrics, domInfo, deps)
      }
      v.setFeature(f, fVal)
    })
  }

  def getSimplifiedSchedTree(sched : isl.UnionMap, domInfo : DomainCoeffInfo, scop : ScopInfo, deps : Set[Dependence], splitLoopBodies : Boolean) : ScheduleNode = {
    return simplifySchedTree(ScheduleTreeConstruction
      .islUnionMap2BasicScheduleTree(sched, domInfo, scop, deps, false, splitLoopBodies), deps)
  }

  /**
    * Simplifies the given schedule tree as far as possible and detects permutable bands.
    */
  def simplifySchedTree(s : ScheduleNode, deps : Set[Dependence]) : ScheduleNode = {
    val sSimpl : ScheduleNode = s.accept(new RemoveCommonOffsetVisitor)
      .accept(new DivideCoeffsByGCDVisitor)
      .accept(new ElimSuperfluousSubTreesVisitor)
      .accept(new ElimSuperfluousDimNodesVisitor)
      .accept(new GroupDimsVisitor, deps.map((d : Dependence) => (d, d.map)).toMap)
    return SchedTreeUtil.markLoops(sSimpl)
  }

  private def checkNumArgs(args : Array[String]) : Boolean = {
    if (args.size < 4) {
      myLogger.warning("Expected 3 arguments: <JSCOP file> <configuration file> <input CSV file> <output CSV file>")
      return false
    }
    return true
  }
}
