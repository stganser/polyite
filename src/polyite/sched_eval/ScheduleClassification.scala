package polyite.sched_eval

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.IndexedSeqView
import scala.collection.mutable.ListBuffer
import scala.util.Random

import polyite.ScopInfo
import polyite.config.Config
import polyite.fitness.Feature
import polyite.fitness.FeatureVect
import polyite.fitness.Prediction
import polyite.fitness.scikit_learn.Classifier
import polyite.fitness.scikit_learn.Classifier.LearningAlgorithms
import polyite.fitness.scikit_learn.Classifier.RandomForestConfig
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.util.SCoPMetrics
import polyite.util.fitness.FeatureCalculator
import isl.Isl
import polyite.schedule.schedule_tree.ScheduleNode

/**
  * Schedule evaluation strategy that is solely based on benchmarking. It labels schedules as either good or bad.
  */
object ScheduleClassification extends AbstractFitnessEvaluation {

  /**
    * Separates the given pairs of schedules into schedules that have already
    * been evaluated, schedules that still need evaluation and schedules that
    * cannot be evaluated. Schedules of the latter group are added to the group
    * of already evaluated schedules. Classifies schedules on the basis of their classification results. Schedules without
    * a predicted class will be classified as requiring evaluation.
    * @return a tuple consisting of the following components:
    * <ol>
    * 	<li>already evaluated schedules together with the evaluation results.</li>
    *   <li>schedules needing evaluation.</li>
    * </ol>
    */
  def classifyForEvaluation(pairs : HashMap[Schedule, Fitness]) : Option[(HashMap[Schedule, Fitness], HashSet[Schedule])] = {
    val alreadyEvaluated : HashMap[Schedule, Fitness] = HashMap.empty
    val scheds2Eval : HashSet[Schedule] = HashSet.empty

    for ((sched, f) <- pairs) {
      if (f.getPrediction.map(_.pClass.isDefined).getOrElse(false))
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

  def init(
    config : Config,
    features : List[Feature],
    domainInfo : DomainCoeffInfo,
    dependences : Set[Dependence],
    scopMetrics : SCoPMetrics,
    scopInf : ScopInfo) {

    this.synchronized {

      if (initialized)
        throw new IllegalStateException("Already initialized.")

      initialized = true
      this.scopMetrics = scopMetrics
      this.features = features
      this.config = config
      this.dependences = dependences
      this.domainInfo = domainInfo
      this.scopInf = scopInf

      this.synchronized {
        for (i <- 0 until config.numMeasurementThreads) {
          classifiers.append(new Classifier(
            features,
            config.learningSet.get,
            config.decTreeMinSamplesLeaf.get,
            config.learningAlgorithm.get,
            if (config.learningAlgorithm.get == LearningAlgorithms.RANDOM_FOREST)
              Some(new RandomForestConfig(config.randForestNTree.get, config.randForestMaxFeatures.get))
            else
              None,
            config.pythonVEnvLocation))
        }
      }
    }
  }

  private var initialized : Boolean = false
  private var scopMetrics : SCoPMetrics = null
  private var features : List[Feature] = null
  private var config : Config = null
  private var scopInf : ScopInfo = null
  private var domainInfo : DomainCoeffInfo = null
  private var dependences : Set[Dependence] = null
  private val classifiers : ListBuffer[Classifier] = ListBuffer.empty

  protected def conf : Config = config

  protected def deps : Set[Dependence] = dependences

  protected def domInfo : DomainCoeffInfo = domainInfo

  protected def scop : ScopInfo = scopInf

  protected override def benchmarkSchedule(workerId : Int)(s : Schedule) {
    val schedTree : ScheduleNode = FeatureCalculator.getSimplifiedSchedTree(s.getSchedule, domInfo, scop, deps, true)
    val schedTreeTiling : ScheduleNode = if (conf.splitLoopBodies)
      schedTree
    else
      FeatureCalculator.getSimplifiedSchedTree(s.getSchedule, domInfo, scop, deps, conf.splitLoopBodies)
    val cachedFitness : Option[Fitness] = checkEvalResultCache(schedTree, schedTreeTiling)
    if (cachedFitness.isDefined) {
      benchmarkingResult.offer((s, cachedFitness.get))
      return
    }

    val logPrefix : String = "(benchmarking worker #" + workerId + ")"
    myLogger.info(logPrefix + "benchmarking schedule: " + s)
    val fVect : FeatureVect = FeatureCalculator.calcFeatures(s.getSchedule, Some(schedTreeTiling), Some(schedTree), domInfo, scop, scopMetrics, deps, features, conf)
    val cls : Classifier = this.synchronized { classifiers(workerId) }
    val pred : Fitness = PredictionOnly(cls.predict(fVect))
    addToEvalResultCache(pred, schedTree, schedTreeTiling)
    benchmarkingResult.offer((s, pred))
  }

  def selectForMutation(pairs : Array[(Schedule, Fitness)]) : Schedule = {
    if (pairs.isEmpty)
      throw new IllegalArgumentException("cannot choose from an empty set of schedules.")
    def isGoodSched(t : (Schedule, Fitness)) : Boolean = t._2.getPrediction.map(_.pClass
      .map((c : Prediction.PerfClass.Value) => c == Prediction.PerfClass.GOOD).getOrElse(false)).getOrElse(false)
    val goodScheds : IndexedSeqView[(Schedule, Fitness), Array[(Schedule, Fitness)]] = pairs.view.filter(isGoodSched)
    val badScheds : IndexedSeqView[(Schedule, Fitness), Array[(Schedule, Fitness)]] = pairs.view.filterNot(isGoodSched)
    if (!goodScheds.isEmpty && Random.nextDouble() <= (2.toDouble / 3.toDouble) || badScheds.isEmpty)
      return goodScheds(Random.nextInt(goodScheds.size))._1
    else
      return badScheds(Random.nextInt(badScheds.size))._1
  }
}