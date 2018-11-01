package polyite.schedule.hash

import polyite.schedule.Schedule
import isl.Isl

/**
  * Considers two schedules to be equivalent if their integer schedule coefficients matrices are equal.
  */
case class SameIntScheduleMatrix(sched : Schedule) extends ScheduleHash {

  override def hashCode : Int = Isl.islUnionMapUniqueToString(sched.getSchedule).hashCode()
  override def equals(o : Any) : Boolean = {
    if (!o.isInstanceOf[SameIntScheduleMatrix])
      return false
    return o.asInstanceOf[SameIntScheduleMatrix].sched.getSchedule.isEqual(sched.getSchedule)
  }
}

object SameIntScheduleMatrix {

  /**
    * Wraps {@code} s in an instance of {@code SameIntScheduleMatrix}.
    */
  def fromSchedule(s : Schedule) : ScheduleHash = SameIntScheduleMatrix(s)
}