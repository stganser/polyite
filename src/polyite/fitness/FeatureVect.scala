package polyite.fitness

/**
  * A feature vector consists of different quantities that describe structural characteristics of a schedule. Instances
  * of this class are immutable. Each modification yields a new instance.
  */
class FeatureVect(vect : Map[Feature, Double] = Map.empty) {

  private val features2Vals : Map[Feature, Double] = vect

  /**
    * Produces a copy of a {@code FeatureVect}, where feature {@code f} has value {@code v}.
    */
  def setFeature(f : Feature, v : Double) : FeatureVect = new FeatureVect(features2Vals + ((f, v)))

  /**
    * Returns the value for feature {@code f}, if it exists.
    */
  def getFeature(f : Feature) : Option[Double] = features2Vals.get(f)

  /**
    * Get the complete feature vector. The order of the components is defined by {@code Feature}.
    */
  def getVect() : List[(Feature, Double)] = features2Vals.toList.sortBy(_._1)

  override def toString() : String = getVect.map((t : (Feature, Double)) => t._1 + " : " + t._2).mkString("[", ",\n", "]")

  override def hashCode() : Int = features2Vals.hashCode()

  override def equals(o : Any) : Boolean = {
    return o.isInstanceOf[FeatureVect] && o.asInstanceOf[FeatureVect].features2Vals.equals(features2Vals)
  }
}