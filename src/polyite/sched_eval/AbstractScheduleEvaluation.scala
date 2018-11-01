package polyite.sched_eval

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.IOException
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.util.LinkedList
import java.util.concurrent.TimeUnit

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.Random

import polyite.ScopInfo
import polyite.config.Config
import polyite.export.JSCOPInterface
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.util.Util
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.util.SchedTreeUtil

/**
  * This abstract fitness evaluation strategy relies on benchmarking of schedules. It interacts with a utility binary
  * to invoke a compiler and run the transformed code. The protocol for the interaction with the script can be derived
  * from the included {@code measure_polybench.bash} script.
  *
  * To use this evaluation strategy, it must be sub-classed. despite providing an initialization method that invokes
  * {@code AbstractScheduleEvaluation#initialize(Config, ScopInfo, DomainCoeffInfo, Set[Dependence], String => String)}
  * the functionality for schedule-evaluation by benchmarking is fully implemented here.
  */
trait AbstractScheduleEvaluation extends AbstractFitnessEvaluation {

  /**
    * Must be invoked by sub-types' initialization method.
    */
  protected def initialize(
    config : Config,
    scopInf : ScopInfo,
    domainInfo : DomainCoeffInfo,
    dependences : Set[Dependence],
    makeTmpWorkingDirDistinct : String => String) {

    this.synchronized {

      if (initialized)
        throw new IllegalStateException("Already initialized.")

      initialized = true
      this.config = config
      this.scopInf = scopInf
      this.dependences = dependences
      this.domainInfo = domainInfo
      this.makeTmpWorkingDirDistinct = makeTmpWorkingDirDistinct
    }
  }

  private var initialized : Boolean = false
  private var config : Config = null
  private var scopInf : ScopInfo = null
  private var domainInfo : DomainCoeffInfo = null
  private var dependences : Set[Dependence] = null
  private var makeTmpWorkingDirDistinct : String => String = null

  protected def conf : Config = config

  protected def deps : Set[Dependence] = dependences

  protected def domInfo : DomainCoeffInfo = domainInfo

  protected def scop : ScopInfo = scopInf

  /**
    * Separates the given pairs of schedules into schedules that have already
    * been evaluated, schedules that still need evaluation and schedules that
    * cannot be evaluated. Schedules of the latter group are added to the group
    * of already evaluated schedules. Classifies schedules on the basis of their benchmarking results. Schedules without
    * {@code EvalResult} will be classified as requiring evaluation.
    * @return a tuple consisting of the following components:
    * <ol>
    * 	<li>already evaluated schedules together with the evaluation results.</li>
    *   <li>schedules needing evaluation.</li>
    * </ol>
    */
  override def classifyForEvaluation(pairs : HashMap[Schedule, Fitness]) : Option[(HashMap[Schedule, Fitness], HashSet[Schedule])] = {
    val alreadyEvaluated : HashMap[Schedule, Fitness] = HashMap.empty
    val scheds2Eval : HashSet[Schedule] = HashSet.empty

    for ((sched, f) <- pairs) {
      if (f.getEvalResult.map(_.completelyEvaluated).getOrElse(false))
        alreadyEvaluated.put(sched, f)
      else if (sched.fitsInt)
        scheds2Eval.add(sched)
      else {
        myLogger.warning(sched + " has coefficients that lie outside the "
          + "value range of Int.")
        alreadyEvaluated.put(sched, FitnessUnknown)
      }
    }
    return Some((alreadyEvaluated, scheds2Eval))
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
    commandList.add(makeTmpWorkingDirDistinct(conf.measurementTmpDirNamePrefix))
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

  private def awaitBooleanResponse(sched : Schedule, schedTree : ScheduleNode, bp : BenchmarkingProcess,
    errResult : EvalResult, errMsg : String) : Boolean = {
    val (success : Option[Boolean], timedOut : Boolean) = getAndCheckAndParseInput(bp, s => s.toBoolean)
    if (timedOut) {
      val res : Fitness = EvalResultOnly(EvalResult.makeTimeoutResult(errResult))
      benchmarkingResult.offer((sched, res))
      addToEvalResultCache(res, schedTree)
      return false
    }
    if (!success.get) {
      val res : Fitness = EvalResultOnly(errResult)
      benchmarkingResult.offer((sched, res))
      addToEvalResultCache(res, schedTree)
      myLogger.warning(bp.logPrefix + errMsg + ": " + Util.readAll(bp.stdErr))
      bp.p.waitFor()
      return false
    }
    return true
  }

  private def awaitDoubleResponse(sched : Schedule, schedTree : ScheduleNode, bp : BenchmarkingProcess, errResult : EvalResult,
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
      val res : Fitness = EvalResultOnly(EvalResult.makeTimeoutResult(errResult))
      benchmarkingResult.offer((sched, res))
      addToEvalResultCache(res, schedTree)
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

  private def awaitExecutionTimes(sched : Schedule, schedTree : ScheduleNode, bp : BenchmarkingProcess, errResult : EvalResult, nValues : Int) : Option[List[Double]] = {
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
      val res : Fitness = EvalResultOnly(EvalResult.makeTimeoutResult(errResult))
      benchmarkingResult.offer((sched, res))
      addToEvalResultCache(res, schedTree)
      return None
    }
    if (!executionTimes.isDefined) {
      val res : Fitness = EvalResultOnly(errResult)
      benchmarkingResult.offer((sched, res))
      addToEvalResultCache(res, schedTree)
      return None
    }
    if (executionTimes.get.size != nValues) {
      bp.p.destroy()
      bp.stopTimeoutThreads()
      throw new BenchmarkingException("Did not receive the expected number of execution time values.")
    }
    return executionTimes
  }

  override protected def benchmarkSchedule(workerId : Int)(s : Schedule) : Unit = {
    val logPrefix : String = "(benchmarking worker #" + workerId + ")"
    myLogger.info(logPrefix + "benchmarking schedule: " + s)

    val schedTree : ScheduleNode = ScheduleTreeConstruction
      .islUnionMap2ScheduleTree(s.getSchedule, s.domInfo,
        scop, s.deps, conf)

    System.gc()

    val fitnessCached : Option[Fitness] = checkEvalResultCache(schedTree)
    if (fitnessCached.isDefined) {
      benchmarkingResult.offer((s, fitnessCached.get))
      return
    }

    val jscopStr = JSCOPInterface.genExtJSCOP(scop, s.getSchedule,
      SchedTreeUtil.scheduleTree2IslScheduleTree(schedTree), true)

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
        val res : Fitness = EvalResultOnly(EvalResult.makeTimeoutResult(result))
        benchmarkingResult.offer((s, res))
        addToEvalResultCache(res, schedTree)
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
    if (!awaitBooleanResponse(s, schedTree, bp, result.setIsCodegenSuccessful(false), "Code generation failed for schedule " + s)) {
      bp.stopTimeoutThreads()
      return
    }

    codegenTimeoutThread.interrupt()
    result = result.setIsCodegenSuccessful(true)

    // duration of parallel code generation
    if (conf.measureParExecTime) {
      val parCompileDurations : Option[List[Double]] = awaitExecutionTimes(s, schedTree, bp, result, conf.numCompilatonDurationMeasurements)
      if (!parCompileDurations.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setParallelCodegenDurations(parCompileDurations.get)
    }

    // duration of sequential code generation
    if (conf.measureSeqExecTime) {
      val seqCompileDurations : Option[List[Double]] = awaitExecutionTimes(s, schedTree, bp, result, conf.numCompilatonDurationMeasurements)
      if (!seqCompileDurations.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setSequentialCodegenDurations(seqCompileDurations.get)
    }

    if (conf.measureParExecTime && conf.validateOutput) {
      // parallel: validate output execution
      if (!awaitBooleanResponse(s, schedTree, bp, result.setExecutionValidateResultParSuccessful(false), "Output validation failed for schedule " + s + " with parallel execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionValidateResultParSuccessful(true)

      // parallel: is valid output?
      if (!awaitBooleanResponse(s, schedTree, bp, result.setHasValidResultPar(false), "Schedule " + s + " does not produce valid output for parallel execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setHasValidResultPar(true)
    }

    if (conf.measureSeqExecTime && conf.validateOutput) {
      // sequential: validate output execution
      if (!awaitBooleanResponse(s, schedTree, bp, result.setExecutionValidateResultSeqSuccessful(false),
        "Output validation failed for schedule " + s + " with sequential execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionValidateResultSeqSuccessful(true)

      // sequential: is valid output?
      if (!awaitBooleanResponse(s, schedTree, bp, result.setHasValidResultSeq(false), "Schedule " + s + " does not produce valid output for sequential execution")) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setHasValidResultSeq(true)
    }

    // parallel: measure execution time
    if (conf.measureParExecTime) {
      if (!awaitBooleanResponse(s, schedTree, bp, result.setExecutionMeasureRuntimeParSuccessful(false), "Failed to measure the parallel execution time for schedule " + s)) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionMeasureRuntimeParSuccessful(true)
    }

    // parallel: execution times
    if (conf.measureParExecTime) {
      val execTimes : Option[List[Double]] = awaitExecutionTimes(s, schedTree, bp, result, conf.numExecutionTimeMeasurements)
      if (!execTimes.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionTimesPar(execTimes.get)
    }

    if (conf.measureSeqExecTime) {
      // sequential: measure execution time
      if (!awaitBooleanResponse(s, schedTree, bp, result.setExecutionMeasureRuntimeSeqSuccessful(false), "Failed to measure the sequential execution time for schedule " + s)) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionMeasureRuntimeSeqSuccessful(true)
    }

    // sequential: execution times
    if (conf.measureSeqExecTime) {
      val execTimes : Option[List[Double]] = awaitExecutionTimes(s, schedTree, bp, result, conf.numExecutionTimeMeasurements)
      if (!execTimes.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setExecutionTimesSeq(execTimes.get)
    }

    // parallel: measure cache hit rate
    if (conf.measureCacheHitRatePar) {
      if (awaitBooleanResponse(s, schedTree, bp, result.setExecutionMeasureCacheHitRateParSuccessful(false), "Failed to measure the cache hit rate for parallel execution")) {
        result = result.setExecutionMeasureCacheHitRateParSuccessful(true)
      } else {
        bp.stopTimeoutThreads()
        return
      }
    }

    // parallel: cache hit rate
    if (conf.measureCacheHitRatePar) {
      var cacheHitRatePar : Option[Double] = awaitDoubleResponse(s, schedTree, bp, result, "Failed to receive the cache hit rate for parallel execution.")
      if (!cacheHitRatePar.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setCacheHitRatePar(cacheHitRatePar.get)
    }

    // seq: measure cache hit rate
    if (conf.measureCacheHitRateSeq) {
      if (awaitBooleanResponse(s, schedTree, bp, result.setExecutionMeasureCacheHitRateSeqSuccessful(false), "Failed to measure the cache hit rate for sequential execution")) {
        result = result.setExecutionMeasureCacheHitRateSeqSuccessful(true)
      } else {
        bp.stopTimeoutThreads()
        return
      }
    }

    // seq: cache hit rate
    if (conf.measureCacheHitRateSeq) {
      var cacheHitRateSeq : Option[Double] = awaitDoubleResponse(s, schedTree, bp, result, "Failed to receive the cache hit rate for sequential execution.")
      if (!cacheHitRateSeq.isDefined) {
        bp.stopTimeoutThreads()
        return
      }
      result = result.setCacheHitRateSeq(cacheHitRateSeq.get)
    }

    result = result.setCompletelyEvaluated(true)
    
    val res : Fitness = EvalResultOnly(result)
    benchmarkingResult.offer((s, res))
    addToEvalResultCache(res, schedTree)
    bp.p.waitFor()
    bp.stopTimeoutThreads()
  }

  def selectForMutation(pairs : Array[(Schedule, Fitness)]) : Schedule = {
    if (pairs.isEmpty)
      throw new IllegalArgumentException("cannot choose from an empty set of schedules.")
    return pairs(Random.nextInt(pairs.size))._1
  }
}
