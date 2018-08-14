package polyite.sched_eval

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.IOException
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.util.LinkedList
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.TimeUnit
import java.util.logging.Level
import java.util.logging.Logger

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

import polyite.ScopInfo
import polyite.config.Config
import polyite.export.JSCOPInterface
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.util.Util

object ScheduleEvaluation {
  private val myLogger : Logger = Logger.getLogger("")

  /**
    * Separates the given pairs of schedules into schedules that have already
    * been evaluated, schedules that still need evaluation and schedules that
    * cannot be evaluated. Schedules of the latter group are added to the group
    * of already evaluated schedules.
    * @return a tuple consisting of the following components:
    * <ol>
    * 	<li>already evaluated schedules together with the evaluation results.</li>
    *   <li>schedules needing evaluation.</li>
    * </ol>
    */
  def classifyForEvaluation(pairs : HashMap[Schedule, EvalResult]) : (HashMap[Schedule, EvalResult], HashSet[Schedule]) = {
    val alreadyEvaluated : HashMap[Schedule, EvalResult] = HashMap.empty
    val scheds2Eval : HashSet[Schedule] = HashSet.empty

    for ((sched, res) <- pairs) {
      if (res.completelyEvaluated)
        alreadyEvaluated.put(sched, res)
      else if (sched.fitsInt)
        scheds2Eval.add(sched)
      else {
        myLogger.warning(sched + " has coefficients that lie outside the "
          + "value range of Int.")
        alreadyEvaluated.put(sched, EvalResult.notEvaluated)
      }
    }
    return (alreadyEvaluated, scheds2Eval)
  }

  val evalResultCache : HashMap[String, EvalResult] = HashMap.empty

  def evaluateSchedules(scop : ScopInfo, domInfo : DomainCoeffInfo,
    deps : Set[Dependence],
    conf : Config)(scheds : HashSet[Schedule]) : (HashMap[Schedule, EvalResult], Boolean) = {
    if (scheds.isEmpty)
      return (HashMap.empty, true)

    var benchmarkingSuccessful : Boolean = true

    myLogger.info("Checking the evaluation results cache.")
    val scheds2Eval : HashSet[Schedule] = HashSet.empty
    for (s : Schedule <- scheds) {
      evalResultCache.get(s.toString()) match {
        case None      => scheds2Eval.add(s)
        case Some(res) => benchmarkingResult.offer((s, res))
      }
    }
    myLogger.info(benchmarkingResult.size() + " schedules have been evaluated before.")

    myLogger.info("Starting to benchmark " + scheds2Eval.size + " schedules.")
    try
      Util.mapInParallel(scheds2Eval, benchmarkSchedule(conf, domInfo, deps, scop),
        conf.numMeasurementThreads)
    catch {
      case t : Throwable => {
        myLogger.log(Level.SEVERE, "Failed to evaluate schedules.", t)
        benchmarkingSuccessful = false
      }
    }
    val result : HashMap[Schedule, EvalResult] = HashMap.empty
    val schedsTmp : HashSet[Schedule] = scheds2Eval.clone()
    while (!benchmarkingResult.isEmpty()) {
      val (s : Schedule, evalResult : EvalResult) = benchmarkingResult.poll()
      evalResultCache.put(s.toString(), evalResult)
      schedsTmp.remove(s)
      result.put(s, evalResult)
    }
    if (!schedsTmp.isEmpty) {
      myLogger.warning(schedsTmp.size + " schedules have not been evaluated.")
      for (s : Schedule <- schedsTmp) {
        result.put(s, EvalResult.notEvaluated)
      }
    }
    return (result, benchmarkingSuccessful)
  }

  var benchmarkingResult : ConcurrentLinkedQueue[(Schedule, EvalResult)] =
    new ConcurrentLinkedQueue

  private def getJSCOPString(s : Schedule, scop : ScopInfo, conf : Config) : String = {
    val schedTree : isl.Schedule = ScheduleTreeConstruction
      .islUnionMap2IslScheduleTree(s.getSchedule, s.domInfo,
        scop, s.deps, conf)
    return JSCOPInterface.genExtJSCOP(scop, s.getSchedule,
      schedTree, true)
  }

  private class BenchmarkingProcess(
      val logPrefix : String,
      val p : Process,
      val stdIn : BufferedWriter,
      val stdOut : BufferedReader,
      val stdErr : BufferedReader,
      val conf : Config) {

    private var timeoutThreads : ListBuffer[Thread] = ListBuffer.empty
    private var destroyed = false

    def isDestroyed : Boolean = this.synchronized { destroyed }

    def createTimeoutThread(timeout : Option[Long], errMsg : String) : Thread = {
      val t : Thread = new Thread() {
        override def run() {
          if (timeout.isDefined) {
            try {
              p.waitFor(timeout.get, TimeUnit.SECONDS)
            } catch {
              case e : InterruptedException => return
            }
            BenchmarkingProcess.this.synchronized {
              if (p.isAlive()) {
                myLogger.warning(logPrefix + errMsg)
                p.destroy()
                destroyed = true
              }
            }
          }
        }
      }
      this.synchronized {
        timeoutThreads.append(t)
      }
      return t
    }

    def stopTimeoutThreads() = this.synchronized {
      timeoutThreads.map { _.interrupt() }
    }
  }

  // The second component of the result is true, if the child process has timed
  // out. In that case the first component is None.
  private def getAndCheckAndParseInput[T](bp : BenchmarkingProcess, parser : String => T) : (Option[T], Boolean) = {
    val input : String = bp.stdOut.readLine()
    if (input == null) {
      bp.p.waitFor()
      if (bp.isDestroyed || bp.p.exitValue() == bp.conf.evaluationSigIntExitCode) {
        myLogger.warning(bp.logPrefix + " Time out.")
        return (None, true)
      }
      myLogger.warning(bp.logPrefix + " Response from child process is null: " + Util.readAll(bp.stdErr))
      bp.stopTimeoutThreads()
      bp.p.destroy()
      throw new BenchmarkingException(bp.logPrefix + " response from child process is null")
    }
    return (Some(parser(input)), false)
  }

  private def startBenchmarkingProcess(conf : Config, workerId : Int, logPrefix : String) : Option[BenchmarkingProcess] = {
    val commandTokens : Array[String] = conf.measurementCommand.split("\\s+")
    val commandList : java.util.List[String] = new LinkedList
    for (s : String <- commandTokens)
      commandList.add(s)
    commandList.add(workerId.toString)
    commandList.add(conf.measurementTmpDirBase.getAbsolutePath + "/")
    commandList.add(conf.measurementTmpDirNamePrefix)
    commandList.add(conf.benchmarkName)
    commandList.add(conf.functionName)
    commandList.add(conf.scopRegionStart)
    commandList.add(conf.scopRegionEnd)
    commandList.add(conf.referenceOutputFile.getAbsolutePath)
    commandList.add(conf.numCompilatonDurationMeasurements.toString())
    commandList.add(conf.validateOutput.toString())
    commandList.add(conf.numExecutionTimeMeasurements.toString())
    commandList.add(conf.irFilesLocation.getAbsolutePath)
    commandList.add(conf.evaluationSigIntExitCode.toString())
    commandList.add(conf.measureCacheHitRatePar.toString())
    commandList.add(conf.measureCacheHitRateSeq.toString())
    commandList.add(conf.measureParExecTime.toString())
    commandList.add(conf.measureSeqExecTime.toString())
    commandList.add(conf.seqPollyOptFlags)
    commandList.add(conf.parPollyOptFlags)
    commandList.add(conf.numactlConf.isDefined.toString())
    if (conf.numactlConf.isDefined)
      commandList.add(conf.numactlConf.get)

    var measurementPB : ProcessBuilder = new ProcessBuilder(commandList)
    measurementPB = measurementPB.directory(conf.measurementWorkingDir)
    val p : Process =
      try
        measurementPB.start()
      catch {
        case e : IOException => {
          val ex : BenchmarkingException = new BenchmarkingException(
            "couldn't start " + commandTokens.mkString(" "))
          ex.initCause(e)
          throw ex
        }
      }

    val stdIn : BufferedWriter = new BufferedWriter(new OutputStreamWriter(
      p.getOutputStream))
    val stdOut : BufferedReader = new BufferedReader(new InputStreamReader(
      p.getInputStream))
    val stdErr : BufferedReader = new BufferedReader(new InputStreamReader(
      p.getErrorStream))
    val bp : BenchmarkingProcess = new BenchmarkingProcess(logPrefix, p, stdIn, stdOut, stdErr, conf)
    val (initResponse : Option[String], timedOut : Boolean) = try {
      getAndCheckAndParseInput(bp, (x => x))
    } catch {
      case e : BenchmarkingException => return None
    }
    if (timedOut)
      throw new BenchmarkingException("Timeout during start of benchmarking process.")
    if (!initResponse.get.matches("Started")) {
      p.destroy()
      throw new BenchmarkingException(logPrefix + " Didn't receive the expected"
        + " initial response from the child process: " + initResponse.get)
    }
    return Some(bp)
  }

  private def awaitBooleanResponse(sched : Schedule, bp : BenchmarkingProcess,
    errResult : EvalResult, errMsg : String) : Boolean = {
    val (success : Option[Boolean], timedOut : Boolean) = getAndCheckAndParseInput(bp, s => s.toBoolean)
    if (timedOut) {
      benchmarkingResult.offer((sched, EvalResult.makeTimeoutResult(errResult)))
      return false
    }
    if (!success.get) {
      benchmarkingResult.offer((sched, errResult))
      myLogger.warning(bp.logPrefix + errMsg + ": " + Util.readAll(bp.stdErr))
      bp.p.waitFor()
      return false
    }
    return true
  }

  private def awaitDoubleResponse(sched : Schedule, bp : BenchmarkingProcess, errResult : EvalResult,
    errMsg : String) : Option[Double] = {
    val (result : Option[Double], timedOut : Boolean) = try {
      getAndCheckAndParseInput(bp, (s : String) => s.toDouble)
    } catch {
      case e : NumberFormatException => {
        bp.p.destroy()
        bp.stopTimeoutThreads()
        throw new BenchmarkingException("Failed to interpret the response of the child process as a double value..")
      }
    }
    if (timedOut) {
      benchmarkingResult.offer((sched, EvalResult.makeTimeoutResult(errResult)))
      return None
    }
    return result
  }

  private def attemptToStartBenchmarkingProcess(conf : Config, workerId : Int, logPrefix : String) : Option[BenchmarkingProcess] = {
    var bp : Option[BenchmarkingProcess] = None
    val timeout : Option[Double] = conf.benchmarkingSurrenderTimeout
    val start = System.currentTimeMillis()
    def timeElapsed : Double = (System.currentTimeMillis() - start) / 1000
    def retry : Boolean = !bp.isDefined && timeout.isDefined && timeElapsed <= timeout.get
    do {
      bp = startBenchmarkingProcess(conf, workerId, logPrefix)
      if (retry)
        Thread.sleep(15 * 60 + 1000) // wait for 15 minutes
    } while (retry)

    if (timeout.isDefined && !bp.isDefined)
      myLogger.warning(logPrefix + " Couldn't start the benchmarking process within the configured timeout.")

    return bp
  }

  private def awaitExecutionTimes(sched : Schedule, bp : BenchmarkingProcess, errResult : EvalResult, nValues : Int) : Option[List[Double]] = {
    val (executionTimes : Option[List[Double]],
      timedOut : Boolean) =
      if (nValues > 0)
        try {
          getAndCheckAndParseInput(bp, (s => s.split("\\s+").map(t => t.toDouble).toList))
        } catch {
          case e : NumberFormatException => {
            bp.p.destroy()
            bp.stopTimeoutThreads()
            throw new BenchmarkingException(bp.logPrefix + " Failed to parse the list of execution time values.")
          }
        }
      else {
        val (_ : Option[String], timedOut1 : Boolean) = getAndCheckAndParseInput(bp, (s => s))
        (if (timedOut1) None else Some(List.empty), timedOut1)
      }
    if (timedOut) {
      benchmarkingResult.offer((sched, EvalResult.makeTimeoutResult(errResult)))
      return None
    }
    if (!executionTimes.isDefined) {
      benchmarkingResult.offer((sched, errResult))
      return None
    }
    if (executionTimes.get.size != nValues) {
      bp.p.destroy()
      bp.stopTimeoutThreads()
      throw new BenchmarkingException("Did not receive the expected number of execution time values.")
    }
    return executionTimes
  }

  private def benchmarkSchedule(conf : Config, domInfo : DomainCoeffInfo,
    deps : Set[Dependence], scop : ScopInfo)(workerId : Int)(s : Schedule) : Unit = {
    val logPrefix : String = "(benchmarking worker #" + workerId + ")"
    myLogger.info(logPrefix + "benchmarking schedule: " + s)

    val jscopStr = getJSCOPString(s, scop, conf)
    System.gc()

    val bp : BenchmarkingProcess = attemptToStartBenchmarkingProcess(conf, workerId, logPrefix) match {
      case None    => throw new BenchmarkingException(logPrefix + " Failed to start the benchmarking process.")
      case Some(x) => x
    }

    val timeoutThread : Thread = bp.createTimeoutThread(Some(conf.measurementTimeout), "The evaluation of schedule " + s
      + " reached a timeout.")
    val codegenTimeoutThread : Thread = bp.createTimeoutThread(conf.compilationTimeout, "Code generation for schedule "
      + s + " reached a timeout.")

    timeoutThread.start()

    try {
      bp.stdIn.write(jscopStr)
      bp.stdIn.flush()
      bp.stdIn.close()
    } catch {
      case e : IOException => {
        myLogger.warning(logPrefix + "The benchmarking script failed:\n"
          + Util.readAll(bp.stdErr))
        val ex : BenchmarkingException = new BenchmarkingException(
          logPrefix + "The child process terminated unexpectedly.")
        ex.initCause(e)
        bp.p.destroy()
        bp.stopTimeoutThreads()
        throw ex
      }
    }

    var result : EvalResult = EvalResult.notEvaluated

    {
      val (isValidSchedule : Option[Boolean], timedOut : Boolean) = getAndCheckAndParseInput(bp, (s => s.toBoolean))
      if (timedOut) {
        benchmarkingResult.offer((s, EvalResult.makeTimeoutResult(result)))
        return
      }
      if (!isValidSchedule.get) {
        myLogger.warning(logPrefix + "Polly reports an invalid JSCOP file for "
          + "schedule " + s + ": " + Util.readAll(bp.stdErr))
        bp.stopTimeoutThreads()
        throw new BenchmarkingException(logPrefix + "Polly rejected the schedule " + s)
      }
    }

    codegenTimeoutThread.start()

    // code generation
    if (!awaitBooleanResponse(s, bp, result.setIsCodegenSuccessful(false), "Code generation failed for schedule " + s)) {
      bp.stopTimeoutThreads()
      return
    }

    codegenTimeoutThread.interrupt()
    result = result.setIsCodegenSuccessful(true)

    // duration of parallel code generation
    if (conf.measureParExecTime) {
      val parCompileDurations : Option[List[Double]] = awaitExecutionTimes(s, bp, result, conf.numCompilatonDurationMeasurements)
      if (!parCompileDurations.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setParallelCodegenDurations(parCompileDurations.get)
    }

    // duration of sequential code generation
    if (conf.measureSeqExecTime) {
      val seqCompileDurations : Option[List[Double]] = awaitExecutionTimes(s, bp, result, conf.numCompilatonDurationMeasurements)
      if (!seqCompileDurations.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setSequentialCodegenDurations(seqCompileDurations.get)
    }

    if (conf.measureParExecTime && conf.validateOutput) {
      // parallel: validate output execution
      if (!awaitBooleanResponse(s, bp, result.setExecutionValidateResultParSuccessful(false), "Output validation failed for schedule " + s + " with parallel execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionValidateResultParSuccessful(true)

      // parallel: is valid output?
      if (!awaitBooleanResponse(s, bp, result.setHasValidResultPar(false), "Schedule " + s + " does not produce valid output for parallel execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setHasValidResultPar(true)
    }

    if (conf.measureSeqExecTime && conf.validateOutput) {
      // sequential: validate output execution
      if (!awaitBooleanResponse(s, bp, result.setExecutionValidateResultSeqSuccessful(false),
        "Output validation failed for schedule " + s + " with sequential execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionValidateResultSeqSuccessful(true)

      // sequential: is valid output?
      if (!awaitBooleanResponse(s, bp, result.setHasValidResultSeq(false), "Schedule " + s + " does not produce valid output for sequential execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setHasValidResultSeq(true)
    }

    // parallel: measure execution time
    if (conf.measureParExecTime) {
      if (!awaitBooleanResponse(s, bp, result.setExecutionMeasureRuntimeParSuccessful(false), "Failed to measure the parallel execution time for schedule " + s)) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionMeasureRuntimeParSuccessful(true)
    }

    // parallel: execution times
    if (conf.measureParExecTime) {
      val execTimes : Option[List[Double]] = awaitExecutionTimes(s, bp, result, conf.numExecutionTimeMeasurements)
      if (!execTimes.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionTimesPar(execTimes.get)
    }

    if (conf.measureSeqExecTime) {
      // sequential: measure execution time
      if (!awaitBooleanResponse(s, bp, result.setExecutionMeasureRuntimeSeqSuccessful(false), "Failed to measure the sequential execution time for schedule " + s)) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionMeasureRuntimeSeqSuccessful(true)
    }

    // sequential: execution times
    if (conf.measureSeqExecTime) {
      val execTimes : Option[List[Double]] = awaitExecutionTimes(s, bp, result, conf.numExecutionTimeMeasurements)
      if (!execTimes.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionTimesSeq(execTimes.get)
    }

    // parallel: measure cache hit rate
    if (conf.measureCacheHitRatePar) {
      if (awaitBooleanResponse(s, bp, result.setExecutionMeasureCacheHitRateParSuccessful(false), "Failed to measure the cache hit rate for parallel execution")) {
        result = result.setExecutionMeasureCacheHitRateParSuccessful(true)
      } else {
        bp.stopTimeoutThreads()
        return
      }
    }

    // parallel: cache hit rate
    if (conf.measureCacheHitRatePar) {
      var cacheHitRatePar : Option[Double] = awaitDoubleResponse(s, bp, result, "Failed to receive the cache hit rate for parallel execution.")
      if (!cacheHitRatePar.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setCacheHitRatePar(cacheHitRatePar.get)
    }

    // seq: measure cache hit rate
    if (conf.measureCacheHitRateSeq) {
      if (awaitBooleanResponse(s, bp, result.setExecutionMeasureCacheHitRateSeqSuccessful(false), "Failed to measure the cache hit rate for sequential execution")) {
        result = result.setExecutionMeasureCacheHitRateSeqSuccessful(true)
      } else {
        bp.stopTimeoutThreads()
        return
      }
    }

    // seq: cache hit rate
    if (conf.measureCacheHitRateSeq) {
      var cacheHitRateSeq : Option[Double] = awaitDoubleResponse(s, bp, result, "Failed to receive the cache hit rate for sequential execution.")
      if (!cacheHitRateSeq.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setCacheHitRateSeq(cacheHitRateSeq.get)
    }

    result = result.setCompletelyEvaluated(true)

    benchmarkingResult.offer((s, result))
    bp.p.waitFor()
    bp.stopTimeoutThreads()
  }
}
