package polyite.schedule.sampling

import polyite.util.Rat

/**
 * Single monomial of the linear combination that forms a schedule vectors.
 */
abstract class ScheduleSummand {
  def v : List[Rat]
  def coeff : Rat

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[ScheduleSummand]) {
      val oSS : ScheduleSummand = o.asInstanceOf[ScheduleSummand]
      return oSS.coeff == coeff && oSS.v.length == v.length && (oSS.v.zip(v) forall { (t : (Rat, Rat)) => t._1 == t._2 })
    }
    false
  }

  override def hashCode : Int = {
    val prime : Int = 31
    prime * v.hashCode + coeff.hashCode()
  }

  def getValue : List[Rat] = v map { x => x * coeff }
}

case class VertexSummand(v : List[Rat], coeff : Rat) extends ScheduleSummand {

  override def equals(o : Any) : Boolean = {
    return o.isInstanceOf[VertexSummand] && super.equals(o)
  }
}
case class RaySummand(v : List[Rat], coeff : Rat) extends ScheduleSummand {
  override def equals(o : Any) : Boolean = {
    return o.isInstanceOf[RaySummand] && super.equals(o)
  }
}
case class LineSummand(v : List[Rat], coeff : Rat) extends ScheduleSummand {
  override def equals(o : Any) : Boolean = {
    return o.isInstanceOf[LineSummand] && super.equals(o)
  }
}