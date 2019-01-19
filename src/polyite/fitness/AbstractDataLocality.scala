package polyite.fitness

import java.util.logging.Logger

import isl.Isl
import polyite.ScopInfo
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.ScheduleUtils
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.util.SCoPMetrics
import isl.IslException
import isl.Isl.TypeAliases._
import isl.VoidCallback1
import isl.Conversions._

/**
  * Estimates data locality by weighting the depth at which a dependence is carried with a given metric.
  */
abstract class AbstractDataLocality(calcDepWeight : (Iterable[Dependence], ScopInfo, Config) => Map[Dependence, Long]) extends Feature {

  private val myLogger : Logger = Logger.getLogger("")
  private var dep2Volume : Map[Dependence, Long] = null
  private val dep2VolumeLck : Object = new Object()

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {
    val schedMap : isl.UnionMap = SchedTreeUtil.scheduleTree2IslScheduleTree(t).getMap
    val inputDeps : Set[Dependence] = ScheduleSpaceUtils.calcInputDeps(scop, domInfo, schedMap)
    myLogger.info("Number of input dependences: " + inputDeps.size)
    dep2VolumeLck.synchronized {
      if (dep2Volume == null)
        dep2Volume = calcDepWeight(deps, scop, conf)
    }
    val inputDep2Volume : Map[Dependence, Long] = calcDepWeight(inputDeps, scop, conf)

    val depsWithVolume : List[(Dependence, Long)] = dep2VolumeLck.synchronized {dep2Volume.toSet ++ inputDep2Volume.toSet } toList
    val maxVolume : Long = depsWithVolume.map(_._2).max
    assert(maxVolume > 0)

    val schedDims : List[(isl.UnionMap, Int)] = Isl.splitMultiDimUnionMap(schedMap).zipWithIndex.map(t => (t._1, t._2 + 1))
    val lastCarryingDims : List[Int] = depsWithVolume.map(_._1).map((dep : Dependence) => {
      val lastCarryingDim : Int = getLastCarryingDim(schedDims.map(_._1), dep.map, scop.getDomain)
      lastCarryingDim
    })
    val weights : List[Double] = depsWithVolume.map(_._2).map(_.toDouble / maxVolume)
    val carryingDimsWeighted : List[Double] = lastCarryingDims.zip(weights).map((t : (Int, Double)) => {
      t._1.toDouble / (if (conf.normalizeFeatures) schedDims.size else 1) * t._2
    })
    val result : Double = carryingDimsWeighted.sum / (if (conf.normalizeFeatures) weights.sum else 1)
    return result
  }

  private def getLastCarryingDim(schedDims : List[isl.UnionMap], dep : isl.Map, domain : isl.UnionSet) : Int = {
    val (lastCarryingDim : Int, _ : isl.Map) = schedDims.foldLeft((0, dep))((t : (Int, isl.Map), sched : isl.UnionMap) => {
      val lastDim : Int = t._1
      val remDeps : isl.Map = t._2
      if (remDeps.isEmpty())
        (lastDim, remDeps)
      else {
        val schedule : isl.UnionMap = sched.intersectDomain(domain)
        val stmtIn : String = dep.getTupleName(T_IN)
        val stmtOut : String = dep.getTupleName(T_OUT)
        val happensBeforeRel : isl.UnionMap = Isl.constructHappensBeforeMap(Isl.islUnionSetFilter(domain, Set(stmtIn, stmtOut)), sched)
        var happensBeforeRelFiltered : isl.UnionMap = isl.UnionMap.empty(happensBeforeRel.getSpace)
        happensBeforeRel.foreachMap((m : isl.Map) => {
          if (m.getTupleName(T_IN).equals(stmtIn) && m.getTupleName(T_OUT).equals(stmtOut))
            happensBeforeRelFiltered = happensBeforeRelFiltered.addMap(m)
        })
        val remDepsNew : isl.Map = if (happensBeforeRelFiltered.isEmpty())
          remDeps
        else
          remDeps.subtract(isl.Map.fromUnionMap(happensBeforeRelFiltered)).coalesce().detectEqualities()
        (lastDim + 1, remDepsNew)
      }
    })
    return lastCarryingDim
  }

  def isMultiStmt() : Boolean = false
}
