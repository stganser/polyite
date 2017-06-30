package polyite

import java.util.logging.Logger
import polyite.schedule.DomainCoeffInfo
import polyite.config.ConfigRand
import polyite.schedule.Schedule
import polyite.schedule.ScheduleUtils
import java.util.logging.Level
import polyite.schedule.Dependence
import polyite.config.Config
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.config.MinimalConfig.NumGeneratorsLimit

object MainRandScheds {

  val myLogger : Logger = Logger.getLogger("")

  def main(args : Array[String]) : Unit = {

    def buildRandSchedGen(s : ScopInfo) : ((DomainCoeffInfo, Set[Dependence],
        Int, Set[Schedule], NumGeneratorsLimit, NumGeneratorsLimit, ConfigRand) => Set[Schedule]) = {
      ScheduleUtils.genRandSchedules
    }

    try {
    MainUtil.runRandExpl[ConfigRand](args, ConfigRand.loadAndValidateConfig,
      buildRandSchedGen)
    } catch {
      case t : Throwable => {
        val logger : Logger = Logger.getLogger("")
        logger.log(Level.SEVERE, "Unhandled throwable", t)
      }
    }
  }
}