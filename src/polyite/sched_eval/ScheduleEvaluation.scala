package polyite.sched_eval

import polyite.ScopInfo
import polyite.schedule.DomainCoeffInfo
import polyite.config.Config
import polyite.schedule.Dependence

/**
  * Concrete unaltered instance of {@link AbstractScheduleEvaluation}.
  */
object ScheduleEvaluation extends AbstractScheduleEvaluation {

  def init(
    config : Config,
    scopInf : ScopInfo,
    domainInfo : DomainCoeffInfo,
    dependences : Set[Dependence],
    makeTmpWorkingDirDistinct : String => String) = super.initialize(
    config,
    scopInf,
    domainInfo,
    dependences,
    makeTmpWorkingDirDistinct)
}