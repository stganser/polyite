package polyite.util

import scala.BigDecimal
import scala.BigInt
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt.int2bigInt

object Rat {

  val singleDigitInts : ArrayBuffer[Rat] = {
    val a : ArrayBuffer[Rat] = new ArrayBuffer(19)
    for (i <- -9 until 10) {
      a.append(Rat(i, 1))
    }
    a
  }

  def apply(numerator : BigInt, denominator : BigInt) : Rat = {
    if (denominator == 0)
      throw new IllegalArgumentException("the denominator must not be 0")
    val r = new Rat
    r.n = numerator
    r.d = denominator

    if (denominator != 1)
      r.reduce
    r.correctSign
    return r
  }

  def apply(n : BigInt) : Rat = {
    if (-9 <= n && n <= 9)
      return singleDigitInts((9 + n).intValue())
    return apply(n, 1)
  }

  def getRandomRat(min : Rat, max : Rat, maxDenom : BigInt) : Rat = {
    if (max < min)
      throw new IllegalArgumentException("max must not be greater than min.")
    if (min == max)
      return min

    // find the minimum required denominator using binary search
    val intervalLength : Rat = max - min
    var minDenom : BigInt = 1
    var base : BigInt = 1
    while (Rat(1, minDenom) > intervalLength) {
      base = minDenom
      minDenom *= 2
    }

    while (Rat(1, minDenom) < intervalLength && base < minDenom - 1) {
      val middle : BigInt = base + (minDenom - base) / 2
      if (Rat(1, middle) > intervalLength)
        base = middle
      else
        minDenom = middle
    }
    assert(Rat(1, minDenom) <= intervalLength, "minDenom = " + minDenom + "; base = " + base + "; intervalLength = " + intervalLength)

    val denom : BigInt = if (minDenom > maxDenom) minDenom else minDenom + Util.getNextRandomBigInt(maxDenom - minDenom + 1)
    val numeratorMin : BigInt = Rat(denom * min.numerator, min.denominator).intCeil
    val numeratorMax : BigInt = Rat(denom * max.numerator, max.denominator).intFloor
    val numerator : BigInt = numeratorMin + Util.getNextRandomBigInt(numeratorMax - numeratorMin + 1)
    val result : Rat = Rat(numerator, denom)
    assert(min <= result && result <= max, "min = " + min + "; result = " + result + "; max = " + max)
    return result
  }

  /**
    * Parses Strings of the format "\(-?[0-9]+( / [0-9]+)?\)" to Rat.
    * @throws NumberFormatException thrown if s does not match the expected format.
    */
  def fromString(s : String) : Rat = {
    val regexp : String = "\\(-?[0-9]+( / [0-9]+)?\\)"
    if (!s.matches(regexp))
      throw new NumberFormatException("s does not match the expected format " + regexp + ": " + s)
    val stripped : String = s.replaceFirst("\\(", "").replaceFirst("\\)", "")
    val tokens : Array[String] = stripped.split(" / ")
    val nums : Array[BigInt] = tokens.map { t => BigInt(t) }
    if (nums.length < 2)
      return Rat(nums(0))
    return Rat(nums(0), nums(1))
  }

  def main(args : Array[String]) : Unit = {
    val r1 : Rat = Rat(1, 3)
    val r2 : Rat = Rat(5, 6)
    var res = r1 + r2

    assert(Rat(3) <= Rat(3))
    assert(Rat(19, 5) <= Rat(4))

    assert(res.n == 7 && res.d == 6)
    res = r1 * r2
    assert(res.n == 5 && res.d == 18)
    res = r1 / r2
    assert(res.n == 2 && res.d == 5)
    res = r2.pow(2)
    assert(res.n == 25 && res.d == 36)
    res = r2.pow(-2)
    assert(res.n == 36 && res.d == 25)
    res = Rat(5, -4)
    assert(res.n == -5 && res.d == 4)

    val r3 = Rat(24, 36)
    res = r1 + r3
    assert(res.n == 1 && res.d == 1)
    res = Rat(0, 5)
    assert(res.n == 0 && res.d == 1)
    res = Rat(1, 4).ceil
    assert(res.n == 1 && res.d == 1)
    res = Rat(1, 4).floor
    assert(res.n == 0 && res.d == 1)
    res = Rat(-1, 4).ceil
    assert(res.n == 0 && res.d == 1)
    res = Rat(-1, 4).floor
    assert(res.n == -1 && res.d == 1)

    res = Rat(7, 4).ceil
    assert(res.n == 2 && res.d == 1)
    res = Rat(7, 4).floor
    assert(res.n == 1 && res.d == 1)
    res = Rat(-7, 4).ceil
    assert(res.n == -1 && res.d == 1)
    res = Rat(-7, 4).floor
    assert(res.n == -2 && res.d == 1)
    res = Rat(1)
    assert(res.n == 1 && res.d == 1)
    res = (Rat(4) + Rat(3)) * Rat(2)
    assert(res.n == 14 && res.d == 1)

    res = Rat(4) + (Rat(3) * Rat(2))
    assert(res.n == 10 && res.d == 1)
    res = getRandomRat(Rat(3), Rat(4), 10)
    assert(Rat(3) <= res && res <= Rat(4) && res.d <= 10)
    res = getRandomRat(Rat(3, 16), Rat(5, 16), 10)
    assert(Rat(3, 16) <= res && res <= Rat(5, 16) && res.d <= 10)
    res = getRandomRat(Rat(3, 123), Rat(5, 123), 10)
    assert(Rat(3, 123) <= res && res <= Rat(5, 123) && res.d <= 123)
    res = getRandomRat(Rat(0), Rat(0), 10)
    assert(res.getIntegerValue == 0)
    res = getRandomRat(Rat(-1), Rat(0), 10)
    assert(Rat(-1) <= res && res <= Rat(0) && res.d <= 10)
    assert(Rat(3) >= Rat(-3))
    assert(Rat(-3) <= Rat(3))
    assert(fromString("(0)") == Rat(0))
    assert(fromString("(-1)") == Rat(-1))
    assert(fromString("(008)") == Rat(8))
    assert(fromString("(1 / 3)") == Rat(1, 3))
    assert({
      var success = false
      try { fromString("(1 / -3)") } catch { case e : NumberFormatException => success = true }
      success
    })
    assert({
      var success = false
      try { fromString("(-001 / -3)") } catch { case e : NumberFormatException => success = true }
      success
    })
  }
}

class Rat {
  private var n : BigInt = 1
  private var d : BigInt = 1

  def +(o : Rat) : Rat = Rat(n * o.d + o.n * d, d * o.d)
  def -(o : Rat) : Rat = Rat(n * o.d - o.n * d, d * o.d)
  def unary_- : Rat = Rat(-n, d)
  def unary_+ : Rat = this
  def *(o : Rat) : Rat = Rat(n * o.n, d * o.d)
  def /(o : Rat) : Rat = Rat(n * o.d, d * o.n)

  def pow(exp : Int) : Rat = {
    if (exp == 0)
      return Rat(0)
    else if (exp > 0)
      return Rat(pow(n, exp), pow(d, exp))
    else
      return Rat(pow(d, -exp), pow(n, -exp))
  }

  private def pow(x : BigInt, exp : Int) : BigInt = (0 until exp).foldLeft(BigInt(1))((res, _) => res * x)

  override def toString : String = {
    val sb : StringBuilder = new StringBuilder
    sb.append('(')
      .append(n)
    if (d > 1) {
      sb.append(" / ")
        .append(d)
    }
    sb.append(')')
      .toString
  }

  /**
    * Checks whether this Rat is an integer.
    */
  def isInteger : Boolean = d == 1

  /**
    * Converts this Rat to an integer iff its denominator is 1.
    * @throws IllegalStateException thrown if the denominator does not equal 1.
    */
  def getIntegerValue : BigInt = {
    if (d == 1)
      return n
    else
      throw new IllegalStateException("Cannot convert " + this + " to Int.")
  }

  def numerator : BigInt = n

  def denominator : BigInt = d

  def getFloatingPointValue : BigDecimal = BigDecimal(n) / BigDecimal(d)

  def abs : Rat = Rat(n.abs, d.abs)

  def ==(o : Rat) : Boolean = n == o.n && d == o.d

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[Rat])
      return o.asInstanceOf[Rat] == this
    false
  }

  def !=(o : Rat) : Boolean = !(this == o)

  def <(o : Rat) : Boolean = n * o.d - o.n * d < 0

  def <=(o : Rat) : Boolean = this == o || this < o

  def >(o : Rat) : Boolean = n * o.d - o.n * d > 0

  def >=(o : Rat) : Boolean = this == o || this > o

  def ceil : Rat = Rat(intCeil, 1)

  def floor : Rat = Rat(intFloor, 1)

  def intCeil : BigInt = {
    if (d == 1) {
      return n
    } else if (n > 0) {
      return (n / d) + 1
    } else {
      return n / d
    }
  }

  def intFloor : BigInt = {
    if (d == 1) {
      return n
    } else if (n < 0) {
      return (n / d) - 1
    } else {
      return n / d
    }
  }

  override def hashCode : Int = {
    val prime : Int = 31
    var result : Int = 1
    result = prime * result + n.hashCode()
    result = prime * result + d.hashCode()
    result
  }

  private def reduce {
    val gcd = n.gcd(d)
    n = n / gcd
    d = d / gcd
  }

  private def correctSign {
    if (d < 0) {
      n = -n
      d = -d
    }
  }
}