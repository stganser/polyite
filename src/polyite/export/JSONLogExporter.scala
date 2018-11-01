package polyite.export

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.util.logging.Level
import java.util.logging.Logger

import scala.BigDecimal
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.io.Source
import scala.math.BigInt.int2bigInt
import scala.util.parsing.json.JSON

import polyite.ScopInfo
import polyite.config.Config
import polyite.config.ConfigGA
import polyite.sched_eval.EvalResult
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.sampling.LineSummand
import polyite.schedule.sampling.RaySummand
import polyite.schedule.Schedule
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.sampling.ScheduleSummand
import polyite.schedule.ScheduleUtils
import polyite.schedule.sampling.VertexSummand
import polyite.util.Rat
import polyite.fitness.FeatureVect
import polyite.fitness.Feature
import polyite.sched_eval.Fitness
import polyite.fitness.Prediction
import polyite.sched_eval.FitnessUnknown
import polyite.sched_eval.EvalResultOnly
import polyite.sched_eval.PredictionOnly
import polyite.sched_eval.PredictionAndEvalResult

/**
  * Functionality for exporting a given population to JSON and re-importing the
  * exported data.
  */
object JSONLogExporter {
  
  implicit object DepsOrder extends Ordering[Dependence]  {
      def compare(d1 : Dependence, d2 : Dependence) = d1.toString() compare d2.toString()
    }
  
  val myLogger : Logger = Logger.getLogger("")

  class ParseException(msg : String) extends Exception(msg)

  def writePopulationToFile(f : File, population : Iterable[(Schedule, Fitness)], generation : Int) {
    var writer : BufferedWriter = null
    try {
      writer = new BufferedWriter(new FileWriter(f))
      writer.write(population2JSON(population, generation))
      writer.flush()
    } finally {
      if (writer != null)
        try {
          writer.close()
        } catch {
          case e : IOException => {
            Logger.getLogger("jsonLog").log(Level.SEVERE, "Failed to close writer", e)
          }
        }
    }
  }

  def population2JSON(population : Iterable[(Schedule, Fitness)], generation : Int, exportDeps : Boolean = false) : String
        = JSONExporter.generate(population2JSONAbsSyn(population, generation, exportDeps), 2)

  private def population2JSONAbsSyn(population : Iterable[(Schedule, Fitness)], generation : Int, exportDeps : Boolean) : Map[String, Any]
        = Map(("schedules", scheduleEvalList2JSONAbsSyn(population, exportDeps)), ("generation", generation))

  private def scheduleEvalList2JSONAbsSyn(scheds : Iterable[(Schedule, Fitness)], exportDeps : Boolean) : List[Any] = {
    var result : List[Any] = List.empty
    scheds.map { t => result = scheduleEval2JSONAbsSyn(t._1, t._2, exportDeps) :: result }
    return result.reverse
  }

  private def scheduleEval2JSONAbsSyn(s : Schedule, f : Fitness, exportDeps : Boolean) : Map[String, Any] = {
    var dimsRepr : List[Map[String, Any]] = List.empty
    
    val depslist = s.deps.toList.sortBy(x => x)

    for (dim <- 0 until s.numDims) {
      val schedSummands : Set[ScheduleSummand] = s.getSchedSummands(dim)
      var verticesRepr : List[Any] = List.empty
      var raysRepr : List[Any] = List.empty
      var linesRepr : List[Any] = List.empty
      for (ss <- schedSummands) {
        ss match {
          case VertexSummand(_, _) => verticesRepr = buildSchedSummandRepr(ss) :: verticesRepr
          case RaySummand(_, _)    => raysRepr = buildSchedSummandRepr(ss) :: raysRepr
          case LineSummand(_, _)   => linesRepr = buildSchedSummandRepr(ss) :: linesRepr
        }
      }
      val schedSummandsRepr : Map[String, Any] = Map(("vertexSummands", verticesRepr),
        ("raySummands", raysRepr), ("lineSummands", linesRepr))

      var indexes : List[Int] = List.empty
      if(exportDeps) {
        val deps = s.getDependencesSatisfiedStronglyByDim(dim)
        for(d <- deps) {
          indexes = depslist.indexOf(d) :: indexes
        }
      }

      val coeffsRepr : String = vector2String(s.getScheduleVector(dim))
      val drep = if(exportDeps)
                      Map(("coeffs", coeffsRepr), ("generators", schedSummandsRepr), ("depindex", indexes.mkString(",")))
                 else
                      Map(("coeffs", coeffsRepr), ("generators", schedSummandsRepr))
      dimsRepr =  drep :: dimsRepr
    }
    var scheduleRepr : Map[String, Any] = Map(("schedule", s.getSchedule.toString()),
      ("scheduleVectors", dimsRepr.reverse))

    var result : Map[String, Any] = Map(("schedule", scheduleRepr))

    if (f.getEvalResult.isDefined)
      result += (("evalResult", evalRes2JSONAbsSyn(f.getEvalResult.get)))

    if (f.getPrediction.isDefined)
      result += (("prediction", prediction2JSONAbsSyn(f.getPrediction.get)))

    return result
  }

  private def evalRes2JSONAbsSyn(res : EvalResult) : Any = {
    var evalResultRepr : Map[String, Any] = Map(
      ("completelyEvaluated", res.completelyEvaluated),
      ("isCodegenSuccessful", res.isCodegenSuccessful),
      ("timedOut", res.timedOut))

    def maybeAddToEvalResult(v : Option[Any], name : String) {
      if (v.isDefined)
        evalResultRepr += ((name, v.get.toString))
    }

    val schedTreeSimplDurations : List[Any] = buildExecutionTimesRepr(res.schedTreeSimplDurations)
    if (schedTreeSimplDurations != null)
      evalResultRepr += (("schedTreeSimplDurations", schedTreeSimplDurations))

    val codegenDurationsPar : List[Any] = buildExecutionTimesRepr(res.parallelCodegenDurations)
    if (codegenDurationsPar != null)
      evalResultRepr += (("parallelCodegenDuration", codegenDurationsPar))
    val codegenDurationsSeq : List[Any] = buildExecutionTimesRepr(res.sequentialCodegenDurations)
    if (codegenDurationsSeq != null)
      evalResultRepr += (("sequentialCodegenDuration", codegenDurationsSeq))
    maybeAddToEvalResult(res.executionValidateResultParSuccessful, "executionValidateResultParSuccessful")
    maybeAddToEvalResult(res.hasValidResultPar, "hasValidResultPar")
    maybeAddToEvalResult(res.executionValidateResultSeqSuccessful, "executionValidateResultSeqSuccessful")
    maybeAddToEvalResult(res.hasValidResultSeq, "hasValidResultSeq")
    maybeAddToEvalResult(res.executionMeasureRuntimeParSuccessful, "executionMeasureRuntimeParSuccessful")
    val executionTimesReprPar : List[Any] = buildExecutionTimesRepr(res.executionTimesPar)
    if (executionTimesReprPar != null)
      evalResultRepr += (("executionTimesPar", executionTimesReprPar))
    maybeAddToEvalResult(res.executionMeasureRuntimeSeqSuccessful, "executionMeasureRuntimeSeqSuccessful")
    val executionTimesReprSeq : List[Any] = buildExecutionTimesRepr(res.executionTimesSeq)
    if (executionTimesReprSeq != null)
      evalResultRepr += (("executionTimesSeq", executionTimesReprSeq))
    maybeAddToEvalResult(res.executionMeasureCacheHitRateParSuccessful, "executionMeasureCacheHitRateParSuccessful")
    maybeAddToEvalResult(res.cacheHitRatePar, "cacheHitRatePar")
    maybeAddToEvalResult(res.executionMeasureCacheHitRateSeqSuccessful, "executionMeasureCacheHitRateSeqSuccessful")
    maybeAddToEvalResult(res.cacheHitRateSeq, "cacheHitRateSeq")
    return evalResultRepr
  }

  private def prediction2JSONAbsSyn(pred : Prediction) : Any = {
    val fVect : Map[String, Any] = pred.fVect.getVect().map((t : (Feature, Double)) => (t._1.getClass.getCanonicalName, t._2)).toMap
    val res = Map(("fVect", fVect))
    if (pred.pClass.isDefined)
      return res + (("pClass", pred.pClass.get.toString()))
    return res
  }

  private def buildExecutionTimesRepr(execTimes : Option[List[Double]]) : List[Any] = {
    if (!execTimes.isDefined)
      return null
    return execTimes.get.map { t => t.toString() }
  }

  private def vector2String(v : List[Rat]) : String = v.mkString("[", ",", "]")

  private def buildSchedSummandRepr(s : ScheduleSummand) : Map[String, Any] = {
    val coeffStr : String = s.coeff.toString()
    val vStr : String = vector2String(s.v)
    return Map(("coeff", coeffStr), ("v", vStr))
  }

  /**
    * Imports the schedules stored in file {@code f} together with their evaluation
    * results. Rebuilds complete {@code Schedule} objects.
    *
    * @return Returns a tuple consisting of the schedules and their evaluation
    * results as its first component and the GA generation of the set of schedules
    * as its second component.
    */
  def readPopulationFromFile(f : File, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : (HashMap[Schedule, Fitness], Int) = {
    val input : Source = Source.fromFile(f)
    val jsonStr : String = try input.mkString finally input.close()
    return json2Population(jsonStr, domInfo, deps)
  }

  /**
    * Imports the schedules stored in file {@code f} together with their evaluation
    * results. Instead of rebuilding complete {@code Schedule} objects only the
    * schedule map strings are imported.
    *
    * @return Returns a tuple consisting of the schedules and their evaluation
    * results as its first component and the GA generation of the set of schedules
    * as its second component.
    */
  def readPopulationFromFileLight(f : File) : (HashMap[String, Fitness], Int) = {
    val input : Source = Source.fromFile(f)
    val jsonStr : String = try input.mkString finally input.close()
    return json2PopulationLight(jsonStr)
  }

  def json2Population(s : String, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], doCheckIntegrity : Boolean = true) : (HashMap[Schedule, Fitness], Int) = {
    val myConversionFunc = { input : String => BigDecimal(input) }
    JSON.globalNumberParser = myConversionFunc
    val parseTree : Option[Any] = JSON.parseFull(s)

    parseTree match {
      case None =>
        throw new ParseException("Failed to parse the JSON input.")
      case Some(parseTree) => {
        return jsonAbsSyn2Population(parseTree, domInfo, deps, doCheckIntegrity)
      }
    }
  }

  private def json2PopulationLight(s : String) : (HashMap[String, Fitness], Int) = {
    val myConversionFunc = { input : String => BigDecimal(input) }
    JSON.globalNumberParser = myConversionFunc
    val parseTree : Option[Any] = JSON.parseFull(s)

    parseTree match {
      case None =>
        throw new ParseException("Failed to parse the JSON input.")
      case Some(parseTree) => {
        return jsonAbsSyn2PopulationLight(parseTree)
      }
    }
  }

  private def jsonAbsSyn2Population(abssyn : Any, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], doCheckIntegrity : Boolean) : (HashMap[Schedule, Fitness], Int) = {
    if (doCheckIntegrity && !abssyn.isInstanceOf[Map[_, Any]])
      throw new ParseException("Expected an object consisting of the "
        + "generation number and a list of individual schedules.")
    val generationRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    if (doCheckIntegrity && !generationRepr.contains("schedules"))
      throw new ParseException("Member \"schedules\" is missing.")
    if (doCheckIntegrity && !generationRepr.contains("generation"))
      throw new ParseException("Member \"generation\" is missing.")
    val generationVal : Any = generationRepr("generation")
    if (doCheckIntegrity && !generationVal.isInstanceOf[String])
      throw new ParseException("Expected generation to contain a string.")
    val generation : Int = generationVal.asInstanceOf[String].toInt
    val scheds : HashMap[Schedule, Fitness] = jsonAbsSyn2ScheduleEvalList(
      generationRepr("schedules"), domInfo, deps, doCheckIntegrity)
    return (scheds, generation)
  }

  private def jsonAbsSyn2PopulationLight(abssyn : Any) : (HashMap[String, Fitness], Int) = {
    if (!abssyn.isInstanceOf[Map[_, Any]])
      throw new ParseException("Expected an object consisting of the "
        + "generation number and a list of individual schedules.")
    val generationRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    if (!generationRepr.contains("schedules"))
      throw new ParseException("Member \"schedules\" is missing.")
    if (!generationRepr.contains("generation"))
      throw new ParseException("Member \"generation\" is missing.")
    val generationVal : Any = generationRepr("generation")
    if (!generationVal.isInstanceOf[String])
      throw new ParseException("Expected generation to contain a string.")
    val generation : Int = generationVal.asInstanceOf[String].toInt
    val scheds : HashMap[String, Fitness] = jsonAbsSyn2ScheduleStrEvalList(
      generationRepr("schedules"))
    return (scheds, generation)
  }

  private def jsonAbsSyn2ScheduleEvalList(abssyn : Any, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], doCheckIntegrity : Boolean) : HashMap[Schedule, Fitness] = {
    if (doCheckIntegrity && !abssyn.isInstanceOf[List[Any]])
      throw new ParseException("Expected a List of schedule-evaluation-result-pairs.")
    val schedListRepr : List[Any] = abssyn.asInstanceOf[List[Any]]
    val result : HashMap[Schedule, Fitness] = HashMap.empty
    schedListRepr.map((abssyn : Any) => {
      val (sched : Schedule, fit : Fitness) = jsonAbsSyn2ScheduleEval(
        domInfo, deps, abssyn, doCheckIntegrity)
      result.put(sched, fit)
      System.gc()
    })
    return result
  }

  private def jsonAbsSyn2ScheduleStrEvalList(abssyn : Any) : HashMap[String, Fitness] = {
    if (!abssyn.isInstanceOf[List[Any]])
      throw new ParseException("Expected a List of schedule-evaluation-result-pairs.")
    val schedListRepr : List[Any] = abssyn.asInstanceOf[List[Any]]
    val result : HashMap[String, Fitness] = HashMap.empty
    schedListRepr.map((abssyn : Any) => {
      val (schedStr : String, fit : Fitness) = jsonAbsSyn2ScheduleStrEval(abssyn)
      result.put(schedStr, fit)
    })
    return result
  }

  private def jsonAbsSyn2ScheduleEval(domInfo : DomainCoeffInfo,
    deps : Set[Dependence], abssyn : Any, doCheckIntegrity : Boolean) : (Schedule, Fitness) = {
    if (doCheckIntegrity && !abssyn.isInstanceOf[Map[_, Any]])
      throw new ParseException("Expected the schedule-evluation results pair to be contained in a single object.")
    val scheduleEvalRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    if (doCheckIntegrity && !scheduleEvalRepr.contains("schedule"))
      throw new ParseException("Couldn't find the schedule")
    val sched : Schedule = jsonAbsSyn2Schedule(scheduleEvalRepr("schedule"), domInfo, deps, doCheckIntegrity)
    val evalRes : Option[EvalResult] = if (scheduleEvalRepr.contains("evalResult"))
      Some(jsonAbsSyn2EvalResult(scheduleEvalRepr("evalResult")))
    else
      None
    val pred : Option[Prediction] = if (scheduleEvalRepr.contains("prediction"))
      Some(jsonAbsSyn2Prediction(scheduleEvalRepr("prediction")))
    else
      None
    return (sched, Fitness.create(evalRes, pred))
  }

  private def jsonAbsSyn2ScheduleStrEval(abssyn : Any) : (String, Fitness) = {
    if (!abssyn.isInstanceOf[Map[_, Any]])
      throw new ParseException("Expected the schedule-evluation results pair to be contained in a single object.")
    val scheduleEvalRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    if (!scheduleEvalRepr.contains("schedule"))
      throw new ParseException("Couldn't find the schedule")
    val schedStr : String = jsonAbsSyn2ScheduleStr(scheduleEvalRepr("schedule"))
    val evalRes : Option[EvalResult] = if (scheduleEvalRepr.contains("evalResult"))
      Some(jsonAbsSyn2EvalResult(scheduleEvalRepr("evalResult")))
    else
      None
    val pred : Option[Prediction] = if (scheduleEvalRepr.contains("prediction"))
      Some(jsonAbsSyn2Prediction(scheduleEvalRepr("prediction")))
    else
      None
    return (schedStr, Fitness.create(evalRes, pred))
  }

  private def jsonAbsSyn2Schedule(abssyn : Any, domInfo : DomainCoeffInfo, deps : Set[Dependence], doCheckIntegrity : Boolean) : Schedule = {
    if (doCheckIntegrity && !abssyn.isInstanceOf[Map[_, Any]]) {
      throw new ParseException("abssyn must be a map")
    }
    val scheduleRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    if (doCheckIntegrity && !scheduleRepr.contains("scheduleVectors"))
      throw new ParseException("couldn't find the schedule vectors.")

    val dimsVal : Any = scheduleRepr("scheduleVectors")
    val dimsRepr : List[Any] = dimsVal.asInstanceOf[List[Any]]
    val sched : Schedule = new Schedule(domInfo, deps)

    val depsList = deps.toList.sortBy(x => x)

    for (jsonVal : Any <- dimsRepr) {
      if (doCheckIntegrity && !jsonVal.isInstanceOf[Map[_, Any]])
        throw new ParseException("A schedule dimension must be represented by a map.")
      val dimRepr : Map[String, Any] = jsonVal.asInstanceOf[Map[String, Any]]
      if (doCheckIntegrity && !dimRepr.contains("coeffs"))
        throw new ParseException("The coefficients of a schedule vector are missing.")
      if (doCheckIntegrity && !dimRepr.contains("generators"))
        throw new ParseException("The generators of a schedule vector are missing.")
      val coeffsVal : Any = dimRepr("coeffs")
      if (doCheckIntegrity && !coeffsVal.isInstanceOf[String])
        throw new ParseException("the value of \"coeffs\" has the wrong type.")
      val coeffs : List[Rat] = parseVector(coeffsVal.asInstanceOf[String], domInfo, doCheckIntegrity)
      val generatorsVal : Any = dimRepr("generators")
      if (doCheckIntegrity && !generatorsVal.isInstanceOf[Map[_, Any]])
        throw new ParseException("could not identifiy the generators object")
      val generatorsRepr : Map[String, Any] = generatorsVal.asInstanceOf[Map[String, Any]]
      if (doCheckIntegrity && !generatorsRepr.contains("vertexSummands"))
        throw new ParseException("vertex summands are missing")
      if (doCheckIntegrity && !generatorsRepr.contains("raySummands"))
        throw new ParseException("ray summands are missing")
      if (doCheckIntegrity && !generatorsRepr.contains("lineSummands"))
        throw new ParseException("line summands are missing")
      val schedSummands : HashSet[ScheduleSummand] = HashSet.empty

      def parseSchedSummands(abssyn : Any, constr : (List[Rat], Rat) => ScheduleSummand) {
        if (doCheckIntegrity && !abssyn.isInstanceOf[List[Any]])
          throw new ParseException("Expected a list of generators.")
        val schedSummandsRepr : List[Any] = abssyn.asInstanceOf[List[Any]]
        for (jsonVal : Any <- schedSummandsRepr)
          schedSummands.add(parseSchedSummand(jsonVal, domInfo, constr, doCheckIntegrity))
      }
      parseSchedSummands(generatorsRepr("vertexSummands"), VertexSummand.apply)
      parseSchedSummands(generatorsRepr("raySummands"), RaySummand.apply)
      parseSchedSummands(generatorsRepr("lineSummands"), LineSummand.apply)

      if(dimRepr.contains("depindex")) {
        //use quick index without recalculating dependencies
        val indexStr : String = dimRepr("depindex").toString()
        val indexes = if(indexStr.isEmpty()) Array.empty else indexStr.split(",").map(_.toInt)
        var dependences : HashSet[Dependence] = HashSet.empty
        for(i <- indexes) {
          dependences += depsList(i)
        }
        sched.addScheduleVector(coeffs, schedSummands.toSet, dependences.toSet)
      } else {
        sched.addScheduleVector(coeffs, schedSummands.toSet)
      }
    }
    if (doCheckIntegrity && !ScheduleUtils.isValidSchedule(sched))
      throw new ParseException("Found an illegal schedule: " + sched)
    return sched
  }

  private def jsonAbsSyn2ScheduleStr(abssyn : Any) : String = {
    if (!abssyn.isInstanceOf[Map[_, Any]]) {
      throw new ParseException("abssyn must be a map")
    }
    val scheduleRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    if (!scheduleRepr.contains("schedule"))
      throw new ParseException("couldn't find the string representation of the schedule.")
    return scheduleRepr("schedule").toString()
  }

  private def getProp(name : String, evalResultRepr : Map[String, Any]) : String = {
    if (!evalResultRepr.contains(name))
      throw new ParseException("Couldn't find property \"" + name
        + "\" of evalulation result.")
    val v : Any = evalResultRepr(name)
    if (!v.isInstanceOf[String])
      throw new ParseException("Expected a String literal as property value.")
    return v.asInstanceOf[String]
  }

  def getOptionalProp[T](name : String, evalResultRepr : Map[String, Any], parser : String => T) : Option[T] = {
    if (evalResultRepr.contains(name)) {
      val v : Any = evalResultRepr(name)
      if (!v.isInstanceOf[String])
        throw new ParseException("Expected a String literal as property value.")

      try {
        return Some(parser(v.asInstanceOf[String]))
      } catch {
        case e : NumberFormatException => {
          throw new ParseException("")
        }
      }
    } else {
      return None
    }
  }

  private def jsonAbsSyn2EvalResult(abssyn : Any) : EvalResult = {
    if (!abssyn.isInstanceOf[Map[_, Any]])
      throw new ParseException("Expected the evaluation results to be contained in an object.")
    val evalResultRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]

    val schedTreeSimplDurations : Option[List[Double]] = jsonAbsSyn2ExecTimesList(evalResultRepr, "schedTreeSimplDurations")
    val executionTimesPar : Option[List[Double]] = jsonAbsSyn2ExecTimesList(
      evalResultRepr, "executionTimesPar")
    val executionTimesSeq : Option[List[Double]] = jsonAbsSyn2ExecTimesList(
      evalResultRepr, "executionTimesSeq")
    return EvalResult.create(
      getProp("completelyEvaluated", evalResultRepr).toBoolean,
      schedTreeSimplDurations,
      getProp("isCodegenSuccessful", evalResultRepr).toBoolean,
      getCodegenDurations(evalResultRepr, "parallelCodegenDuration"),
      getCodegenDurations(evalResultRepr, "sequentialCodegenDuration"),
      getOptionalProp("executionValidateResultParSuccessful", evalResultRepr, _.toBoolean),
      getOptionalProp("hasValidResultPar", evalResultRepr, _.toBoolean),
      getOptionalProp("executionValidateResultSeqSuccessful", evalResultRepr, _.toBoolean),
      getOptionalProp("hasValidResultSeq", evalResultRepr, _.toBoolean),
      getOptionalProp("executionMeasureRuntimeParSuccessful", evalResultRepr, _.toBoolean),
      executionTimesPar,
      getOptionalProp("executionMeasureRuntimeSeqSuccessful", evalResultRepr, _.toBoolean),
      executionTimesSeq,
      getOptionalProp("executionMeasureCacheHitRateParSuccessful", evalResultRepr, _.toBoolean),
      getOptionalProp("cacheHitRatePar", evalResultRepr, _.toDouble),
      getOptionalProp("executionMeasureCacheHitRateSeqSuccessful", evalResultRepr, _.toBoolean),
      getOptionalProp("cacheHitRateSeq", evalResultRepr, _.toDouble),
      getOptionalProp("timedOut", evalResultRepr, _.toBoolean) getOrElse false)
  }

  private def getCodegenDurations(evalResultRepr : Map[String, Any], fieldName : String) : Option[List[Double]] = {
    if (evalResultRepr.contains(fieldName)) {
      val codegenDurationsVal : Any = evalResultRepr(fieldName)
      if (codegenDurationsVal.isInstanceOf[String])
        return Some(List(codegenDurationsVal.asInstanceOf[String].toDouble))
      else if (codegenDurationsVal.isInstanceOf[List[Any]]) {
        return jsonAbsSyn2ExecTimesList(evalResultRepr, fieldName)
      } else
        throw new ParseException("value of field \"" + fieldName + "\" is neither an instance of String nor List[Any].")
    }
    return None
  }

  private def jsonAbsSyn2ExecTimesList(evalResultRepr : Map[String, Any],
    fieldName : String) : Option[List[Double]] = {
    if (evalResultRepr.contains(fieldName)) {
      val executionTimesVal : Any = evalResultRepr(fieldName)
      if (!executionTimesVal.isInstanceOf[List[Any]])
        throw new ParseException("Expected \"" + fieldName
          + "\" to contain a list of values.")
      val executionTimesRepr : List[Any] = executionTimesVal.asInstanceOf[List[Any]]
      var executionTimes : List[Double] = List.empty
      executionTimesRepr.foreach { t =>
        {
          if (!t.isInstanceOf[String])
            throw new ParseException("Expected to find the measured values in "
              + "a list of strings.")
          executionTimes = t.asInstanceOf[String].toDouble :: executionTimes
        }
      }
      return Some(executionTimes.reverse)
    } else {
      return None
    }
  }

  private def jsonAbsSyn2Prediction(abssyn : Any) : Prediction = {
    if (!abssyn.isInstanceOf[Map[_, Any]])
      throw new ParseException("Expected the prediction to be represented by a map.")
    val predM : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    val fVectAbssyn : Any = predM.get("fVect") match {
      case None    => throw new ParseException("Key \"fVect\" is missing.")
      case Some(v) => v
    }
    val fVect : Map[Feature, Double] = fVectAbssyn.asInstanceOf[Map[String, Any]].map((t : (String, Any)) => {
      val f : Feature = Feature.getFeature(t._1) match {
        case None => throw new ParseException("unknown feature: " + t._1)
        case Some(ff) => ff
      }

      val v : Double = try {
        t._2.toString.toDouble
      } catch {
        case _ : NumberFormatException => throw new ParseException("Expected a double: " + t._2)
      }
      (f, v)
    })
    val predClass : Option[Prediction.PerfClass.Value] = predM.get("pClass").map { s =>
      try {
        Prediction.PerfClass.withName(s.toString())
      } catch {
        case e : NoSuchElementException => throw new ParseException("Failed to parse predicted class from " + s)
      }
    }
    return Prediction(new FeatureVect(fVect), predClass)
  }

  private def parseSchedSummand(abssyn : Any, domInfo : DomainCoeffInfo, constr : (List[Rat], Rat) => ScheduleSummand, doCheckIntegrity : Boolean) : ScheduleSummand = {
    if (doCheckIntegrity && !abssyn.isInstanceOf[Map[_, Any]])
      throw new ParseException("Expected an object for each generator.")
    val schedSummandRepr : Map[String, Any] = abssyn.asInstanceOf[Map[String, Any]]
    if (doCheckIntegrity && !schedSummandRepr.contains("coeff"))
      throw new ParseException("A generator coefficient is missing.")
    if (doCheckIntegrity && !schedSummandRepr.contains("v"))
      throw new ParseException("A generator is missing")
    val coeffVal : Any = schedSummandRepr("coeff")
    if (doCheckIntegrity && !coeffVal.isInstanceOf[String])
      throw new ParseException("The coefficient of a schedule summand must" +
        "be a Rat literal.")
    val coeff : Rat = Rat.fromStringOptCheck(doCheckIntegrity)(coeffVal.asInstanceOf[String])
    val vVal : Any = schedSummandRepr("v")
    if (doCheckIntegrity && !vVal.isInstanceOf[String])
      throw new ParseException("A generator vector must be given as a String.")
    val v : List[Rat] = parseVector(vVal.asInstanceOf[String], domInfo, doCheckIntegrity)
    return constr(v, coeff)
  }

  private def parseVector(s : String, domInfo : DomainCoeffInfo, doCheckIntegrity : Boolean) : List[Rat] = {
    val ratPattern : String = "\\(-?[0-9]+( / [0-9]+)?\\)"
    val vectorPattern : String = "\\[(" + ratPattern + "(," + ratPattern + ")+)?\\]"
    if (doCheckIntegrity && !s.matches(vectorPattern))
      throw new ParseException("The given string doesn't match the pattern "
        + vectorPattern + ": " + s)
    val v : List[Rat] = s.replaceFirst("\\[", "").replaceFirst("\\]", "").split(",").map(Rat.fromStringOptCheck(doCheckIntegrity)).toList
    if (v.size != domInfo.dim)
      throw new ParseException("vector has the wrong number of dimensions: " + s)
    return v
  }
}
