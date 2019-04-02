package polyite.util

import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.ScopInfo
import isl.Conversions._
import isl.Callback1
import isl.Isl.TypeAliases._

object SCoPMetrics {

  /**
    * Calculate metrics for the given SCoP.
    */
  def calc(scop : ScopInfo) : SCoPMetrics = {
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val numDeps : Int = deps.size
    val numStmts : Int = scop.getDomain.nSet

    val interfCount = ScheduleSpaceUtils.calcInterferenceOfDeps(deps, domInfo.universe).values.sum

    val interfShare : Double = interfCount.toDouble / (deps.size * (deps.size - 1))

    var maxLoopDepth : Int = 0
    scop.getSched.foreachMap((stmtSched : isl.Map) => {
      val nLoop : Int = stmtSched.dim(T_IN)
      maxLoopDepth = math.max(nLoop, maxLoopDepth)
    })
    val nStructurePar : Int = scop.getParams.params().dim(T_PAR)
    return SCoPMetrics(numDeps, numStmts, interfShare, nStructurePar, maxLoopDepth)
  }
}

/**
  * Some metrics of a SCoP:
  * <ul>
  * <li>number of data dependences</li>
  * <li>number of statements</li>
  * <li>share of dependences that interfere with other dependences (compare Pouchet PLDI'2008)</li>
  * <li>number of structure parameters</li>
  * <li>maximum loop depth</li>
  * </ul>
  */
case class SCoPMetrics(numDeps : Int, numStmts : Int, interfShare : Double, nStructurePar : Int,
  maxLoopDepth : Int)
