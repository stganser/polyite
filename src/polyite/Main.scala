package polyite

import polyite.evolution.SelectionStrategies
import java.util.logging.Logger
import java.util.logging.Level

object Main {
  def main(args : Array[String]) {
    try {
    MainUtil.runGA(args, SelectionStrategies.takeKBestSchedules)
    } catch {
      case t : Throwable => {
        val logger : Logger = Logger.getLogger("")
        logger.log(Level.SEVERE, "Unhandled throwable", t)
      }
    }
  }
}