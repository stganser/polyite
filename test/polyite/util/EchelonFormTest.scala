package polyite.util

import java.util.logging.Logger

import org.junit.Assert.assertTrue
import org.junit.Test

class EchelonFormTest {

  @Test
  def test {
    val matrix : Array[Array[Rat]] = Array(
      Array(Rat(1), Rat(1), Rat(1)),
      Array(Rat(1), Rat(0), Rat(1)),
      Array(Rat(0), Rat(1), Rat(1)),
      Array(Rat(0), Rat(2), Rat(2)))
    val echelonMatrix : Array[Array[Rat]] = Util.calcRowEchelonForm(matrix, 4, 3)
    println(echelonMatrix.map(_.mkString(", ")).mkString("\n"))
    assertTrue(Util.calcRowRank(echelonMatrix) == 3)
  }
}