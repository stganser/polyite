package polyite.sched_eval

import polyite.fitness.Prediction

object Fitness {

  def create(evalRes : Option[EvalResult], pred : Option[Prediction]) : Fitness = {
    return (evalRes, pred) match {
      case (None, None)       => FitnessUnknown
      case (Some(r), None)    => EvalResultOnly(r)
      case (None, Some(p))    => PredictionOnly(p)
      case (Some(r), Some(p)) => PredictionAndEvalResult(p, r)
    }
  }
}

trait Fitness {

  def getPrediction : Option[Prediction] = None
  def getEvalResult : Option[EvalResult] = None
  def isCompletelyEvaluated : Boolean = false
}

object FitnessUnknown extends Fitness

case class PredictionOnly(p : Prediction) extends Fitness {
  override def getPrediction = Some(p)
  override def isCompletelyEvaluated : Boolean = p.pClass.isDefined
}

case class EvalResultOnly(res : EvalResult) extends Fitness {
  override def getEvalResult = Some(res)
  override def isCompletelyEvaluated : Boolean = res.completelyEvaluated
}

case class PredictionAndEvalResult(p : Prediction, res : EvalResult) extends Fitness {
  override def getPrediction = Some(p)
  override def getEvalResult = Some(res)
  override def isCompletelyEvaluated : Boolean = res.completelyEvaluated && p.pClass.isDefined
}