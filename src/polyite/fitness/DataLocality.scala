package polyite.fitness

import polyite.schedule.ScheduleSpaceUtils

/**
  * Estimates data locality by weighting the depth at which a dependence is carried by the volume of data communicated
  * between each pair of memory accesses of source and target statement. By volume we mean the number of memory cells
  * required to communicate the data.
  */
object DataLocality extends AbstractDataLocality(ScheduleSpaceUtils.calcTrafficSizesOfDeps) {
  override def toString() : String = getClass.getSimpleName
}