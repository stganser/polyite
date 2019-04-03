package polyite.evolution

import java.io.File
import java.util.logging.Logger

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.math.BigInt.int2bigInt
import scala.util.Random

import polyite.schedule.DomainCoeffInfo
import polyite.export.JSCOPInterface
import polyite.ScopInfo
import polyite.config.ConfigGA
import polyite.export.JSONLogExporter
import polyite.schedule.sampling.LineSummand
import polyite.schedule.sampling.RaySummand
import polyite.schedule.Schedule
import polyite.schedule.sampling.ScheduleSummand
import polyite.schedule.ScheduleUtils
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.ScheduleVectorUtils
import polyite.schedule.sampling.VertexSummand
import polyite.util.Rat
import polyite.util.Util
import polyite.schedule.Dependence
import polyite.export.ScheduleExport
import polyite.schedule.sampling.SamplingStrategy
import polyite.schedule.sampling.SamplingStrategyParams
import polyite.schedule.sampling.ChernikovaSamplingStrategy

object CrossoverStrategies {
  var myLogger : Logger = Logger.getLogger("")

  def rowCrossover(conf : ConfigGA, scop : ScopInfo, sampler : SamplingStrategy)(s1 : Schedule, s2 : Schedule) : HashSet[Schedule] = {
    val dep2DimsSched1 : Map[Dependence, Set[Int]] = constructDep2Dim(s1)
    val dep2DimsSched2 : Map[Dependence, Set[Int]] = constructDep2Dim(s2)
    val samplerParams : SamplingStrategyParams = sampler.createSamplingStrategyParamsFromConf(conf)
    return rowCrossoverRec(s1, s2, new Schedule(s1.domInfo, s1.deps), dep2DimsSched1,
      dep2DimsSched2) match {
        case Some(s) => {
          val sSimplified : Schedule = ScheduleUtils.simplify(s)
          HashSet[Schedule](ScheduleUtils.expandToFullSchedule(conf, sampler, samplerParams, sSimplified,
              ScheduleUtils.generateLinIndepScheduleVector))
        }
        case None => HashSet.empty[Schedule]
      }
  }

  private def constructDep2Dim(sched : Schedule) : Map[Dependence, Set[Int]] = {
    var dep2Dim : Map[Dependence, Set[Int]] = Map.empty[Dependence, Set[Int]]
    for (d : Dependence <- sched.deps) {
      for (i <- 0 until sched.numDims) {
        if (sched.getDependencesSatisfiedStronglyByDim(i).contains(d)) {
          if (dep2Dim.contains(d)) {
            val oldSet = dep2Dim(d)
            dep2Dim -= d
            dep2Dim += ((d, oldSet + i))
          } else {
            dep2Dim += ((d, Set(i)))
          }
        }
      }
    }
    return dep2Dim
  }

  private def rowCrossoverRec(s1 : Schedule, s2 : Schedule,
    newSched : Schedule, dep2DimsSched1 : Map[Dependence, Set[Int]],
    dep2DimsSched2 : Map[Dependence, Set[Int]]) : Option[Schedule] = {

    if (Thread.interrupted())
      throw new InterruptedException("row crossover has been canceled.")

    val carriedDeps : Set[Dependence] = newSched.getCarriedDeps

    if (carriedDeps.size == s1.deps.size)
      return Some(newSched)

    val uncarriedDeps : Set[Dependence] = s1.deps -- carriedDeps
    val currDim : Int = newSched.numDims
    val domInfo = s1.domInfo

    var mustUse1 = false
    var mustUse2 = false

    if (s1.numDims <= currDim && s2.numDims <= currDim) {
      return None
    } else if (s1.numDims <= currDim) {
      mustUse2 = true
    } else if (s2.numDims <= currDim) {
      mustUse1 = true
    }
    uncarriedDeps.map { d =>
      {
        val max1 : Int = dep2DimsSched1(d).maxBy { x => x }
        val max2 : Int = dep2DimsSched2(d).maxBy { x => x }
        if (max1 <= currDim && max2 <= currDim) {
          if (max1 == currDim && max2 < currDim)
            mustUse1 ||= true
          if (max2 == currDim && max1 < currDim)
            mustUse2 ||= true
        }
      }
    }
    if (mustUse1 && mustUse2)
      return None
    var schedsToTry : List[Schedule] = List.empty[Schedule]
    if (mustUse1)
      schedsToTry :+= s1
    else if (mustUse2)
      schedsToTry :+= s2
    else {
      if (Random.nextBoolean())
        schedsToTry = schedsToTry :+ s1 :+ s2
      else
        schedsToTry = schedsToTry :+ s2 :+ s1
    }

    for (sched : Schedule <- schedsToTry) {
      if (uncarriedDeps.forall { d =>
        !ScheduleUtils.getDirectionOfDep(
          sched.getSchedule(currDim), d).isNegative
      }) {
        val newSchedTmp = newSched.clone()
        newSchedTmp.addForeignDim(sched, currDim)
        val result : Option[Schedule] = rowCrossoverRec(s1, s2, newSchedTmp,
          dep2DimsSched1, dep2DimsSched2)

        if (result.isDefined)
          return result
      }
    }
    return None
  }

  // TODO: one could perform this crossover between different dimensions, too.
  def geometricCrossover(conf : ConfigGA, scop : ScopInfo, sampler : SamplingStrategy)(s1 : Schedule, s2 : Schedule) : HashSet[Schedule] = {
    /*
     * Find dims i that are suitable for crossover:
     * preconditions:
     *  1) The uncarried deps of one schedule must be a subset of the uncarried
     *     deps of the other schedule, both in dim i - 1. Use the prefix of the
     *     schedule with less uncarried deps in dim i - 1 as a prefix for the
     *     new schedule.
     *  2) In at least one dimension the distance of v1 and v2 is >=2
     */
    val deps : Set[Dependence] = s1.deps
    val suitableDims : List[Int] = getSuitableDims(s1, s2)
    val newSchedules : HashSet[Schedule] = HashSet.empty

    for (dim <- Random.shuffle(suitableDims)) {
      val v1 : List[Rat] = s1.getScheduleVector(dim)
      val v2 : List[Rat] = s2.getScheduleVector(dim)
      val segment : List[Rat] = calcSegment(v1, v2)
      val segAbsMax : BigInt = ScheduleVectorUtils.getMaxAbsoluteComponent(segment).intFloor
      val uncarriedS1 : Set[Dependence] = if (dim == 0) deps else deps
        .filterNot(s1.getDependencesCarriedUpToDim(dim - 1).contains)
      val uncarriedS2 : Set[Dependence] = if (dim == 0) deps else deps
        .filterNot(s2.getDependencesCarriedUpToDim(dim - 1).contains)
      val schedPrefixSrc : Schedule =
        if (uncarriedS1.equals(uncarriedS2)) {
          if (Random.nextBoolean())
            s1
          else
            s2
        } else if (uncarriedS1.subsetOf(uncarriedS2)) {
          s1
        } else {
          s2
        }

      val s1Summands : Set[ScheduleSummand] = s1.getSchedSummands(dim)
      val s2Summands : Set[ScheduleSummand] = s2.getSchedSummands(dim)

      val schedPref : Schedule = new Schedule(s1.domInfo, deps)
      for (i <- 0 until dim)
        schedPref.addForeignDim(schedPrefixSrc, i)

      val numVariantsForDim = Random.nextInt(conf.maxNumNewSchedsFromCrossover - newSchedules.size)
      var chosenFs : HashSet[BigInt] = HashSet.empty
      while (chosenFs.size < segAbsMax - 1 && chosenFs.size < numVariantsForDim) {
        val f = Util.getNextRandomBigInt(segAbsMax - 1) + 1
        chosenFs.add(f)
        if (Thread.interrupted())
          throw new InterruptedException("geometric crossover has been interrupted.")
      }

      for (f <- chosenFs) {
        val alpha : Rat = Rat(f, segAbsMax)
        val schedVNew : List[Rat] = ScheduleVectorUtils.add(v1, segment, alpha)
        val schedSummandsNew : Set[ScheduleSummand] = if (sampler == ChernikovaSamplingStrategy)
          calcScheduleSummands(s1Summands, s2Summands, alpha)
        else
          Set.empty
        schedPref.addScheduleVector(schedVNew, schedSummandsNew)
        // the completed schedule is a full schedule by definition
        val schedComplSet : HashSet[Schedule] = ScheduleUtils.completeSchedule(
          schedPref, conf.maxNumRays, conf.maxNumLines, conf, 1, schedPref.deps -- schedPref.getCarriedDeps, sampler)
        newSchedules.add(schedComplSet.head)
        schedPref.removeLastScheduleVector
      }
    }
    return newSchedules
  }

  /**
    * Select dimensions from s1 and s2 that are suitable for geometric crossover.
    * The following conditions must be true:
    *  1) The uncarried deps of one schedule must be a subset of the uncarried
    *     deps of the other schedule, both in dim i - 1. Use the prefix of the
    *     schedule with less uncarried deps in dim i - 1 as a prefix for the
    *     new schedule.
    *  2) In at least one dimension the distance of v1 = s1[i] and v2 = s2[i] is
    *  >=2
    */
  private def getSuitableDims(s1 : Schedule, s2 : Schedule) : List[Int] = {
    val deps : Set[Dependence] = s1.deps
    val suitableDims : List[Int] = List.empty
    val minNDims : Int = math.min(s1.numDims, s2.numDims)

    def isSuitableDim(dim : Int) : Boolean = {
      val v1 : List[Rat] = s1.getScheduleVector(dim)
      val v2 : List[Rat] = s2.getScheduleVector(dim)
      if (ScheduleVectorUtils.getMaxAbsoluteComponent(
        calcSegment(v1, v2)).intFloor > 1) {
        if (dim == 0) {
          return true
        } else {
          val uncarriedS1 : Set[Dependence] = deps
            .filterNot(s1.getDependencesCarriedUpToDim(dim - 1).contains)
          val uncarriedS2 : Set[Dependence] = deps
            .filterNot(s2.getDependencesCarriedUpToDim(dim - 1).contains)
          if (uncarriedS1.subsetOf(uncarriedS2) || uncarriedS2.subsetOf(uncarriedS1)) {
            return true
          }
        }
      }
      return false
    }
    return (0 until minNDims).filter(isSuitableDim).toList
  }

  /**
    * Calculate the new set of (Chernikova) generator - coefficient pairs
    * resulting from the geometric crossover in dimension i of schedules s1 and
    * s2.
    */
  private def calcScheduleSummands(s1Summands : Set[ScheduleSummand],
    s2Summands : Set[ScheduleSummand], alpha : Rat) : Set[ScheduleSummand] = {
    val schedSummands : HashSet[ScheduleSummand] = HashSet.empty
    val s1Vertices2Summands : HashMap[List[Rat], VertexSummand] = HashMap.empty
    val s1Rays2Summands : HashMap[List[Rat], RaySummand] = HashMap.empty
    val s1Lines2Summands : HashMap[List[Rat], LineSummand] = HashMap.empty
    val s1Coeff : Rat = Rat(1) - alpha

    // Part coming from s1
    for (s : ScheduleSummand <- s1Summands) {
      val c1 : Rat = s1Coeff * s.coeff
      s match {
        case VertexSummand(v, c) => {
          val vsNew : VertexSummand = new VertexSummand(v, c1)
          s1Vertices2Summands.put(vsNew.v, vsNew)
          schedSummands.add(vsNew)
        }
        case RaySummand(v, c) => {
          val rsNew : RaySummand = new RaySummand(v, c1)
          s1Rays2Summands.put(rsNew.v, rsNew)
          schedSummands.add(rsNew)
        }
        case LineSummand(v, c) => {
          val lsNew : LineSummand = new LineSummand(v, c1)
          s1Lines2Summands.put(lsNew.v, lsNew)
          schedSummands.add(lsNew)
        }
      }
    }

    // Part coming from s2. Keep in mind that s1 and s2 may share generators in
    // dimension i.
    for (s : ScheduleSummand <- s2Summands) {
      val c2 : Rat = s.coeff * alpha
      s match {
        case VertexSummand(v, c) => {
          s1Vertices2Summands.get(v) match {
            case None => schedSummands.add(new VertexSummand(v, c2))
            case Some(s1 @ VertexSummand(v1, c1)) => {
              schedSummands.remove(s1)
              val newCoeff : Rat = c1 + c2
              if (newCoeff != Rat(0))
                schedSummands.add(new VertexSummand(v, newCoeff))
            }
          }
        }
        case RaySummand(v, c) => {
          s1Rays2Summands.get(v) match {
            case None => schedSummands.add(new RaySummand(v, c2))
            case Some(s1 @ RaySummand(v1, c1)) => {
              schedSummands.remove(s1)
              val newCoeff : Rat = c1 + c2
              if (newCoeff != Rat(0))
                schedSummands.add(new RaySummand(v, newCoeff))
            }
          }
        }
        case LineSummand(v, c) => {
          s1Lines2Summands.get(v) match {
            case None => schedSummands.add(new LineSummand(v, c2))
            case Some(s1 @ LineSummand(v1, c1)) => {
              schedSummands.remove(s1)
              val newCoeff : Rat = c1 + c2
              if (newCoeff != Rat(0))
                schedSummands.add(new LineSummand(v, newCoeff))
            }
          }
        }
      }
    }
    return schedSummands.toSet
  }

  /**
    * Calculate the segment v1v2 between the points identified by the vectors v1
    * and v2.
    */
  private def calcSegment(v1 : List[Rat], v2 : List[Rat]) : List[Rat] = v1.zip(v2).map { (t : (Rat, Rat)) => t._2 - t._1 }
}