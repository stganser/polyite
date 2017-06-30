package polyite.util

import java.io.File
import java.util.logging.Logger

import polyite.MainUtil
import polyite.ScopInfo
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils

import isl.Conversions.convertLambdaToVoidCallback1
import isl.Isl.TypeAliases.T_IN
import isl.Isl.TypeAliases.T_PAR

/**
  * Load a SCoP from JSCOP, print the folowing data and exit.
  *
  * <ul>
  * <li>Number of data dependences.</li>
  * <li>Number of statements</li>
  * <li>Maximum loop depth</li>
  * <li>Number of structure parameters</li>
  * </ul>
  *
  * Expects the path to the JSCOP file as a command line parameter.
  */
object PrintSCoPSize {

  val myLogger : Logger = Logger.getLogger("")

  def main(args : Array[String]) : Unit = {
    if (!checkNumArgs(args))
      return
    val (_ : File, scop : ScopInfo) = MainUtil.loadScop(args(0)) match {
      case None    => return
      case Some(s) => s
    }
    myLogger.info("Generating stats for " + args(0))

    val metrics : SCoPMetrics = SCoPMetrics.calc(scop)

    println("n deps: " + metrics.numDeps)
    println("n statements: " + metrics.numStmts)
    println("max loop depth: " + metrics.maxLoopDepth)
    println("n structure param: " + metrics.nStructurePar)
    println("interf share: " + metrics.interfShare)
  }

  def checkNumArgs(args : Array[String]) : Boolean = {
    if (args.length < 1) {
      myLogger.warning("Expected one argument: <JSCOP file path>")
      return false
    }
    return true
  }
}