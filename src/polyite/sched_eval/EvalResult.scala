package polyite.sched_eval

object EvalResult {
  val notEvaluated : EvalResult = new EvalResult(false, None, false, None, None, None, None, None,
    None, None, None, None, None, None, None, None, None, false)

  def makeTimeoutResult(res : EvalResult) : EvalResult = {
    return new EvalResult(
      res.myCompletelyEvaluated,
      res.mySchedTreeSimplDurations,
      res.myIsCodegenSuccessful,
      res.myParallelCodegenDurations,
      res.mySequentialCodegenDurations,
      res.myExecutionValidateResultParSuccessful,
      res.myHasValidResultPar,
      res.myExecutionValidateResultSeqSuccessful,
      res.myHasValidResultSeq,
      res.myExecutionMeasureRuntimeParSuccessful,
      res.myExecutionTimesPar,
      res.myExecutionMeasureRuntimeSeqSuccessful,
      res.myExecutionTimesSeq,
      res.myExecutionMeasureCacheHitRateParSuccessful,
      res.myCacheHitRatePar,
      res.myExecutionMeasureCacheHitRateSeqSuccessful,
      res.myCacheHitRateSeq,
      true)
  }

  def create(completelyEvaluated : Boolean,
    schedTreeSimplDurations : Option[List[Double]],
    isCodegenSuccessful : Boolean,
    parallelCodegenDuration : Option[List[Double]],
    sequentialCodegenDuration : Option[List[Double]],
    executionValidateResultParSuccessful : Option[Boolean],
    hasValidResultPar : Option[Boolean],
    executionValidateResultSeqSuccessful : Option[Boolean],
    hasValidResultSeq : Option[Boolean],
    executionMeasureRuntimeParSuccessful : Option[Boolean],
    executionTimesPar : Option[List[Double]],
    executionMeasureRuntimeSeqSuccessful : Option[Boolean],
    executionTimesSeq : Option[List[Double]],
    executionMeasureCacheHitRateParSuccessful : Option[Boolean],
    cacheHitRatePar : Option[Double],
    executionMeasureCacheHitRateSeqSuccessful : Option[Boolean],
    cacheHitRateSeq : Option[Double],
    timedOut : Boolean) : EvalResult = {
    return new EvalResult(completelyEvaluated,
      schedTreeSimplDurations,
      isCodegenSuccessful,
      parallelCodegenDuration,
      sequentialCodegenDuration,
      executionValidateResultParSuccessful,
      hasValidResultPar,
      executionValidateResultSeqSuccessful,
      hasValidResultSeq,
      executionMeasureRuntimeParSuccessful,
      executionTimesPar,
      executionMeasureRuntimeSeqSuccessful,
      executionTimesSeq,
      executionMeasureCacheHitRateParSuccessful,
      cacheHitRatePar,
      executionMeasureCacheHitRateSeqSuccessful,
      cacheHitRateSeq,
      timedOut)
  }

  private def getNormalizedExecTime(ts : Iterable[Double]) : Double = ts.minBy { x => x }
}

/**
  * Result from the evaluation of a schedule.
  */
class EvalResult private (
    private var myCompletelyEvaluated : Boolean,
    private var mySchedTreeSimplDurations : Option[List[Double]],
    private var myIsCodegenSuccessful : Boolean,
    private var myParallelCodegenDurations : Option[List[Double]],
    private var mySequentialCodegenDurations : Option[List[Double]],
    private var myExecutionValidateResultParSuccessful : Option[Boolean],
    private var myHasValidResultPar : Option[Boolean],
    private var myExecutionValidateResultSeqSuccessful : Option[Boolean],
    private var myHasValidResultSeq : Option[Boolean],
    private var myExecutionMeasureRuntimeParSuccessful : Option[Boolean],
    private var myExecutionTimesPar : Option[List[Double]],
    private var myExecutionMeasureRuntimeSeqSuccessful : Option[Boolean],
    private var myExecutionTimesSeq : Option[List[Double]],
    private var myExecutionMeasureCacheHitRateParSuccessful : Option[Boolean],
    private var myCacheHitRatePar : Option[Double],
    private var myExecutionMeasureCacheHitRateSeqSuccessful : Option[Boolean],
    private var myCacheHitRateSeq : Option[Double],
    private var myTimedOut : Boolean) {

  def copy : EvalResult = {
    return new EvalResult(myCompletelyEvaluated,
      mySchedTreeSimplDurations,
      myIsCodegenSuccessful,
      myParallelCodegenDurations,
      mySequentialCodegenDurations,
      myExecutionValidateResultParSuccessful,
      myHasValidResultPar,
      myExecutionValidateResultSeqSuccessful,
      myHasValidResultSeq,
      myExecutionMeasureRuntimeParSuccessful,
      myExecutionTimesPar,
      myExecutionMeasureRuntimeSeqSuccessful,
      myExecutionTimesSeq,
      myExecutionMeasureCacheHitRateParSuccessful,
      myCacheHitRatePar,
      myExecutionMeasureCacheHitRateSeqSuccessful,
      myCacheHitRateSeq,
      myTimedOut)
  }

  /**
    * Aggregates the measurement results for parallel execution contained by
    * this EvalResult.
    * @throw IllegalStateException thrown if this EvalResult does not contain
    * any measurement results from parallel execution.
    */
  def getNormalizedExecTimePar = {
    if (myExecutionTimesPar.isDefined && myExecutionTimesPar.get.size > 0)
      EvalResult.getNormalizedExecTime(myExecutionTimesPar.get)
    else
      throw new IllegalStateException("This EvalResult does not contain any "
        + "parallel execution time results.")
  }

  /**
    * Aggregates the measurement results for sequential execution contained by
    * this EvalResult.
    * @throw IllegalStateException thrown if this EvalResult does not contain
    * any measurement results from sequential execution.
    */
  def getNormalizedExecTimeSeq = {
    if (myExecutionTimesSeq.isDefined && myExecutionTimesSeq.get.size > 0)
      EvalResult.getNormalizedExecTime(myExecutionTimesSeq.get)
    else
      throw new IllegalStateException("This EvalResult does not contain any "
        + "sequential execution time results.")
  }

  /**
    * Aggregates the measurement results for duration of generation of parallel code contained by
    * this EvalResult.
    * @throw IllegalStateException thrown if this EvalResult does not contain
    * any such measurement results.
    */
  def getNormalizedParallelCodegenDuration = {
    if (myParallelCodegenDurations.isDefined && myParallelCodegenDurations.get.size > 0)
      EvalResult.getNormalizedExecTime(myParallelCodegenDurations.get)
    else
      throw new IllegalStateException("This EvalResult does not contain time measurement results from parallel code generation.")
  }

  /**
    * Aggregates the measurement results for duration of generation of sequential code contained by
    * this EvalResult.
    * @throw IllegalStateException thrown if this EvalResult does not contain
    * any such measurement results.
    */
  def getNormalizedSequentialCodegenDuration = {
    if (mySequentialCodegenDurations.isDefined && mySequentialCodegenDurations.get.size > 0)
      EvalResult.getNormalizedExecTime(mySequentialCodegenDurations.get)
    else
      throw new IllegalStateException("This EvalResult does not contain time measurement results from sequential code generation.")
  }

  /**
    * Aggregates the measurement results for duration of schedule tree simplification contained by
    * this EvalResult.
    * @throw IllegalStateException thrown if this EvalResult does not contain
    * any such measurement results.
    */
  def getNormalizedSchedTreeSimplDurations : Double = {
    if (mySchedTreeSimplDurations.isDefined && mySchedTreeSimplDurations.size > 0)
      return EvalResult.getNormalizedExecTime(mySchedTreeSimplDurations.get)
    else
      throw new IllegalStateException("This EvalResult does not contain time measurement results from schedule tree simplification.")
  }

  def setCompletelyEvaluated(completelyEvaluated : Boolean) : EvalResult = {
    val cp : EvalResult = copy
    cp.myCompletelyEvaluated = completelyEvaluated
    return cp
  }

  def setSchedTreeSimplDurations(schedTreeSimplDurations : List[Double]) : EvalResult = {
    val cp : EvalResult = copy
    cp.mySchedTreeSimplDurations = Some(schedTreeSimplDurations)
    return cp
  }

  def setIsCodegenSuccessful(isCodegenSuccessful : Boolean) : EvalResult = {
    val cp : EvalResult = copy
    cp.myIsCodegenSuccessful = isCodegenSuccessful
    return cp
  }

  def setParallelCodegenDurations(parallelCodegenDurations : List[Double]) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myParallelCodegenDurations = Some(parallelCodegenDurations)
    return cp
  }

  def setSequentialCodegenDurations(sequentialCodegenDurations : List[Double]) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.mySequentialCodegenDurations = Some(sequentialCodegenDurations)
    return cp
  }

  def setExecutionValidateResultParSuccessful(executionValidateResultParSuccessful : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionValidateResultParSuccessful = Some(executionValidateResultParSuccessful)
    return cp
  }

  def setHasValidResultPar(hasValidResultPar : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myHasValidResultPar = Some(hasValidResultPar)
    return cp
  }

  def setExecutionValidateResultSeqSuccessful(executionValidateResultSeqSuccessful : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionValidateResultSeqSuccessful = Some(executionValidateResultSeqSuccessful)
    return cp
  }

  def setHasValidResultSeq(hasValidResultSeq : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myHasValidResultSeq = Some(hasValidResultSeq)
    return cp
  }

  def setExecutionMeasureRuntimeParSuccessful(executionMeasureRuntimeParSuccessful : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionMeasureRuntimeParSuccessful = Some(executionMeasureRuntimeParSuccessful)
    return cp
  }

  def setExecutionTimesPar(executionTimesPar : List[Double]) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionTimesPar = Some(executionTimesPar)
    return cp
  }

  def setExecutionMeasureRuntimeSeqSuccessful(executionMeasureRuntimeSeqSuccessful : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionMeasureRuntimeSeqSuccessful = Some(executionMeasureRuntimeSeqSuccessful)
    return cp
  }

  def setExecutionTimesSeq(executionTimesSeq : List[Double]) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionTimesSeq = Some(executionTimesSeq)
    return cp
  }

  def setExecutionMeasureCacheHitRateParSuccessful(executionMeasureCacheHitRateParSuccessful : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionMeasureCacheHitRateParSuccessful = Some(executionMeasureCacheHitRateParSuccessful)
    return cp
  }

  def setCacheHitRatePar(cacheHitRatePar : Double) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myCacheHitRatePar = Some(cacheHitRatePar)
    return cp
  }

  def setExecutionMeasureCacheHitRateSeqSuccessful(executionMeasureCacheHitRateSeqSuccessful : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myExecutionMeasureCacheHitRateSeqSuccessful = Some(executionMeasureCacheHitRateSeqSuccessful)
    return cp
  }

  def setCacheHitRateSeq(cacheHitRateSeq : Double) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myCacheHitRateSeq = Some(cacheHitRateSeq)
    return cp
  }

  def setTimedOut(timedOut : Boolean) : EvalResult = {
    val cp : EvalResult = this.copy
    cp.myTimedOut = timedOut
    return cp
  }

  def completelyEvaluated : Boolean = this.myCompletelyEvaluated
  def schedTreeSimplDurations : Option[List[Double]] = this.mySchedTreeSimplDurations
  def isCodegenSuccessful : Boolean = this.myIsCodegenSuccessful
  def parallelCodegenDurations : Option[List[Double]] = this.myParallelCodegenDurations
  def sequentialCodegenDurations : Option[List[Double]] = this.mySequentialCodegenDurations
  def executionValidateResultParSuccessful : Option[Boolean] = this.myExecutionValidateResultParSuccessful
  def hasValidResultPar : Option[Boolean] = this.myHasValidResultPar
  def executionValidateResultSeqSuccessful : Option[Boolean] = this.myExecutionValidateResultSeqSuccessful
  def hasValidResultSeq : Option[Boolean] = this.myHasValidResultSeq
  def executionMeasureRuntimeParSuccessful : Option[Boolean] = this.myExecutionMeasureRuntimeParSuccessful
  def executionTimesPar : Option[List[Double]] = this.myExecutionTimesPar
  def executionMeasureRuntimeSeqSuccessful : Option[Boolean] = this.myExecutionMeasureRuntimeSeqSuccessful
  def executionTimesSeq : Option[List[Double]] = this.myExecutionTimesSeq
  def executionMeasureCacheHitRateParSuccessful : Option[Boolean] = this.myExecutionMeasureCacheHitRateParSuccessful
  def cacheHitRatePar : Option[Double] = this.myCacheHitRatePar
  def executionMeasureCacheHitRateSeqSuccessful : Option[Boolean] = this.myExecutionMeasureCacheHitRateSeqSuccessful
  def cacheHitRateSeq : Option[Double] = this.myCacheHitRateSeq
  def timedOut : Boolean = this.myTimedOut

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[EvalResult]) {
      val other : EvalResult = o.asInstanceOf[EvalResult]
      return this.myCompletelyEvaluated.equals(other.myCompletelyEvaluated) &&
        this.mySchedTreeSimplDurations.equals(other.mySchedTreeSimplDurations) &&
        this.myIsCodegenSuccessful.equals(other.myIsCodegenSuccessful) &&
        this.myParallelCodegenDurations.equals(other.myParallelCodegenDurations) &&
        this.mySequentialCodegenDurations.equals(other.mySequentialCodegenDurations) &&
        this.myExecutionValidateResultParSuccessful.equals(other.myExecutionValidateResultParSuccessful) &&
        this.myHasValidResultPar.equals(other.myHasValidResultPar) &&
        this.myExecutionValidateResultSeqSuccessful.equals(other.myExecutionValidateResultSeqSuccessful) &&
        this.myHasValidResultSeq.equals(other.myHasValidResultSeq) &&
        this.myExecutionMeasureRuntimeParSuccessful.equals(other.myExecutionMeasureRuntimeParSuccessful) &&
        this.myExecutionTimesPar.equals(other.myExecutionTimesPar) &&
        this.myExecutionMeasureRuntimeSeqSuccessful.equals(other.myExecutionMeasureRuntimeSeqSuccessful) &&
        this.myExecutionTimesSeq.equals(other.myExecutionTimesSeq) &&
        this.myExecutionMeasureCacheHitRateParSuccessful.equals(other.myExecutionMeasureCacheHitRateParSuccessful) &&
        this.myCacheHitRatePar.equals(other.myCacheHitRatePar) &&
        this.myExecutionMeasureCacheHitRateSeqSuccessful.equals(other.myExecutionMeasureCacheHitRateSeqSuccessful) &&
        this.myCacheHitRateSeq.equals(other.myCacheHitRateSeq) &&
        this.myTimedOut.equals(other.myTimedOut)
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 41
    var hashCode : Int = myCompletelyEvaluated.hashCode()
    hashCode = hashCode * prime + myCompletelyEvaluated.hashCode()
    hashCode = hashCode * prime + mySchedTreeSimplDurations.hashCode()
    hashCode = hashCode * prime + myIsCodegenSuccessful.hashCode()
    hashCode = hashCode * prime + myParallelCodegenDurations.hashCode()
    hashCode = hashCode * prime + mySequentialCodegenDurations.hashCode()
    hashCode = hashCode * prime + myExecutionValidateResultParSuccessful.hashCode()
    hashCode = hashCode * prime + myHasValidResultPar.hashCode()
    hashCode = hashCode * prime + myExecutionValidateResultSeqSuccessful.hashCode()
    hashCode = hashCode * prime + myHasValidResultSeq.hashCode()
    hashCode = hashCode * prime + myExecutionMeasureRuntimeParSuccessful.hashCode()
    hashCode = hashCode * prime + myExecutionTimesPar.hashCode()
    hashCode = hashCode * prime + myExecutionMeasureRuntimeSeqSuccessful.hashCode()
    hashCode = hashCode * prime + myExecutionTimesSeq.hashCode()
    hashCode = hashCode * prime + myExecutionMeasureCacheHitRateParSuccessful.hashCode()
    hashCode = hashCode * prime + myCacheHitRatePar.hashCode()
    hashCode = hashCode * prime + myExecutionMeasureCacheHitRateSeqSuccessful.hashCode()
    hashCode = hashCode * prime + myCacheHitRateSeq.hashCode()
    hashCode = hashCode * prime + myTimedOut.hashCode()
    return hashCode
  }
}
