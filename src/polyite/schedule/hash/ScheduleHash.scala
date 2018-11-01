package polyite.schedule.hash

/**
  * Sometimes, schedules must be checked for equality. Yet, the representation to use for structural comparison can vary.
  * This trait offers the ability to abstract from the concrete representation.
  */
trait ScheduleHash {
  def hashCode : Int
  def equals(o : Any) : Boolean
}