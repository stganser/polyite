package polyite.fitness

import polyite.schedule.ScheduleSpaceUtils

/**
  * Estimates data locality by weighting the depth at which a dependence is carried with the sum of the memory traffic
  * of source and target statement. This feature is cheaper to compute than {@code DataLocality}.
  */
object DataLocalityMemTraffic extends AbstractDataLocality(ScheduleSpaceUtils.calcMemTrafficSizesOfDepStmts) {
  override def toString() : String = getClass.getSimpleName
}