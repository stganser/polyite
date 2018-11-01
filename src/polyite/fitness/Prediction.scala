package polyite.fitness

object Prediction {
  object PerfClass extends Enumeration {
    val GOOD, BAD = Value
  }
}

case class Prediction(fVect : FeatureVect, pClass : Option[Prediction.PerfClass.Value]) extends Ordered[Prediction] {

  def compare(that : polyite.fitness.Prediction) : Int = {
    if (pClass.isEmpty) {
      if (that.pClass.isEmpty)
        return 0
      return 1
    }
    if (that.pClass.isEmpty)
      return -1
    if (pClass.get == that.pClass.get)
      return 0
    if (pClass.get == Prediction.PerfClass.GOOD)
      return -1
    return 1
  }
}