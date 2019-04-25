package polyite.schedule.hash

import polyite.schedule.Schedule
import isl.Isl
import polyite.schedule.ScheduleVectorUtils

/**
  * Considers two schedules to be equivalent if their integer schedule coefficients matrices are equal.
  */
case class SameIntScheduleMatrix(sched : Schedule) extends ScheduleHash {

  override def hashCode : Int = sched.getScheduleVectors().map(ScheduleVectorUtils.multiplyWithCommonDenominator).hashCode()
  override def equals(o : Any) : Boolean = {
    if (!o.isInstanceOf[SameIntScheduleMatrix])
      return false
    return sched.getScheduleVectors().map(ScheduleVectorUtils.multiplyWithCommonDenominator)
      .equals(o.asInstanceOf[SameIntScheduleMatrix].sched.getScheduleVectors().map(ScheduleVectorUtils.multiplyWithCommonDenominator))
  }
}

object SameIntScheduleMatrix {

  /**
    * Wraps {@code} s in an instance of {@code SameIntScheduleMatrix}.
    */
  def fromSchedule(s : Schedule) : ScheduleHash = SameIntScheduleMatrix(s)
}