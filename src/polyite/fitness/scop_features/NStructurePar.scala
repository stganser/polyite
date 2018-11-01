package polyite.fitness.scop_features

import polyite.fitness.Feature
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Dependence
import polyite.ScopInfo
import polyite.util.SCoPMetrics
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.config.Config

/**
  * Number of structure parameters of the SCoP.
  */
object NStructurePar extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = scopMetrics.nStructurePar

  def isMultiStmt() : Boolean = false

  override def toString() : String = getClass.getSimpleName
}