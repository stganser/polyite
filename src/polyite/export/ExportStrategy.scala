package polyite.export

import polyite.schedule.Schedule
import polyite.sched_eval.Fitness
import polyite.fitness.Feature

/**
  * Trait for strategies that can be used to export sets of schedules together with their fitness to different file formats.
  */
trait ExportStrategy {

  def export(population : Iterable[(Schedule, Fitness)], activeScheduleFeatures : List[Feature], generation : Int) : Unit
}