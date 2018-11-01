package polyite.fitness.scop_features

import polyite.fitness.Feature
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Dependence
import polyite.ScopInfo
import polyite.util.SCoPMetrics
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.config.Config

/**
  * Number of statements of the SCoP.
  */
object NumStmts extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = scopMetrics.numStmts

  def isMultiStmt() : Boolean = false

  override def toString() : String = getClass.getSimpleName
}