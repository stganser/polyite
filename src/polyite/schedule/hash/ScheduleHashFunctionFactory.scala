package polyite.schedule.hash

import polyite.config.Config
import polyite.config.MinimalConfig
import polyite.schedule.Schedule
import polyite.ScopInfo

object ScheduleHashFunctionFactory {

  def createHashFunction(conf : Config, scop : ScopInfo) : (Schedule => ScheduleHash) = {
    return conf.scheduleEquivalenceRelation match {
      case MinimalConfig.ScheduleEquivalenceRelation.INT_MATRIX                     => SameIntScheduleMatrix.fromSchedule
      case MinimalConfig.ScheduleEquivalenceRelation.RATIONAL_MATRIX_AND_GENERATORS => SameGenerators.fromSchedule
      case MinimalConfig.ScheduleEquivalenceRelation.SCHED_TREE                     => SameScheduleTree.fromSchedule(conf, scop)
    }
  }
}