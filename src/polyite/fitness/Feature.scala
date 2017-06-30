package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.ScopInfo
import polyite.util.SCoPMetrics

object Feature {

  // add new features here
  val features : List[Feature] = List(
    )
}

/**
  * Implementation of a structural feature of schedule trees.
  */
trait Feature extends Ordered[Feature] {

  def compare(that : Feature) : Int = this.toString() compare that.toString()

  /**
    * Calculate the feature's value for {@code t}.
    */
  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics,
    domInfo : DomainCoeffInfo, deps : Set[Dependence]) : Double;

  /**
    * Prints the name of the feature.
    */
  def toString() : String;

  /**
    * Returns a reference to the feature itself.
    */
  def getRef() : Feature;

  /**
    * Returns {@code true} iff this feature is only meaningful in case of SCoPs with multiple statements.
    */
  def isMultiStmt() : Boolean;
}