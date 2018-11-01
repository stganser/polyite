package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import polyite.config.Config
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.ScopInfo
import polyite.util.SCoPMetrics
import polyite.fitness.scop_features.InterfShare
import polyite.fitness.scop_features.NumDeps
import polyite.fitness.scop_features.NumStmts
import polyite.fitness.scop_features.MaxLoopDepth
import polyite.fitness.scop_features.NStructurePar

object Feature {

  // add new features here
  val features : List[Feature] = List(
    TreeDepth,
    NumSeqAndSetNodes,
    NumLeafs,
    ParallelLoops,
    TilableBands,
    SparsityIterCoeffs,
    DataLocality,
    MemAccessPattern //,
    //    InterfShare,
    //    MaxLoopDepth,
    //    NStructurePar,
    //    NumDeps,
    //    NumStmts
    )

  /**
    * Returns the feature with the given class name. Returns {@code None} if the features doesn't exist.
    */
  def getFeature(className : String) : Option[Feature] = features.find(_.getClass.getCanonicalName.equals(className))
}

/**
  * Implementation of a structural feature of schedule trees.
  */
trait Feature extends Ordered[Feature] {

  def compare(that : Feature) : Int = this.toString() compare that.toString()

  /**
    * Calculate the feature's value for {@code t}.
    * @return the feature value or {@code None} in case of an isl-computeout.
    */
  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics,
    domInfo : DomainCoeffInfo, deps : Set[Dependence]) : Double;

  /**
    * Prints the name of the feature.
    */
  def toString() : String;

  /**
    * Returns {@code true} iff this feature is only meaningful in case of SCoPs with multiple statements.
    */
  def isMultiStmt() : Boolean;
}
