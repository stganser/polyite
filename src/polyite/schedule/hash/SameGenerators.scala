package polyite.schedule.hash

import polyite.schedule.Schedule

/**
  * Considers two schedules to be equivalent, if their schedule matrices are equal down to the generators of their rows.
  */
case class SameGenerators(sched : Schedule) extends ScheduleHash {

  override def hashCode : Int = sched.hashCode()
  override def equals(o : Any) : Boolean = {
    if (!o.isInstanceOf[SameGenerators])
      return false
    return o.asInstanceOf[SameGenerators].sched.equals(sched)
  }
}

object SameGenerators {

  /**
    * Wraps {@code} s in an instance of {@code SameGenerators}.
    */
  def fromSchedule(s : Schedule) : ScheduleHash = SameGenerators(s)
}