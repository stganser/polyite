package polyite.schedule.hash

import polyite.schedule.Schedule
import polyite.config.Config
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.ScopInfo

/**
  * Two schedules are equal if they correspond to the same simplified schedule tree.
  */
case class SameScheduleTree(sched : Schedule, conf : Config, scop : ScopInfo) extends ScheduleHash {
  private val schedTree : ScheduleNode = ScheduleTreeConstruction.islUnionMap2ScheduleTree(
    sched.getSchedule,
    sched.domInfo, scop, sched.deps, conf)
  override def hashCode : Int = schedTree.hashCode()
  override def equals(o : Any) : Boolean = {
    if (!o.isInstanceOf[SameScheduleTree])
      return false
    return o.asInstanceOf[SameScheduleTree].schedTree.equals(schedTree)
  }
}

object SameScheduleTree {

  /**
    * Wraps {@code s} in an instance of {@code SameScheduleTree} based on {@code conf} and {@code scop}. {@code conf}
    * defines the steps carried out to simply the schedule tree constructed from {@code s}.
    */
  def fromSchedule(conf : Config, scop : ScopInfo)(s : Schedule) : ScheduleHash = SameScheduleTree(s, conf, scop)
}