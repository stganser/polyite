package polyite.util

import java.util.logging.Logger

import org.junit.Assert.assertTrue
import org.junit.Test

class TimerTest {

  @Test
  def test {
    val myLogger : Logger = Logger.getLogger("")

    val activeDurations : Array[Array[Long]] = Array(Array(500, 500, 1000), Array(1000, 4000, 2000), Array(2000, 2000, 1000), Array(1000, 1000, 2000), Array(3000, 1000, 2000))
    val pauseDurations : Array[Array[Long]] = Array(Array(1000, 3000, 1000), Array(3000, 1000, 1000), Array(1000, 1000, 3000), Array(1000, 3000, 1000), Array(1000, 1000, 3000))

    val measuredDurations : Array[Long] = new Array(activeDurations.size)

    (0 until activeDurations.size).toArray.par.map((myIndex : Int) => {
      val timerName : String = f"test${myIndex}"
      myLogger.info("Starting the timer")
      Timer.startTimer(timerName)
      val myActiveDurations : Array[Long] = activeDurations(myIndex)
      val myPauseDurations : Array[Long] = pauseDurations(myIndex)
      for (i <- 0 until myActiveDurations.size) {
        myLogger.info(f"Running for ${myActiveDurations(i)} milliseconds")
        Thread.sleep(myActiveDurations(i))
        Timer.stopTimer(timerName)
        myLogger.info(f"Sleeping for ${myPauseDurations(i)} milliseconds")
        Thread.sleep(myPauseDurations(i))
        Timer.restartTimer(timerName)
      }
      measuredDurations(myIndex) = Timer.removeTimer(timerName)
    })
    activeDurations.map(_.sum).zip(measuredDurations).map(t => (t._1 / 1000, t._2 / 1000)).map(t => assertTrue(t._1 == t._2))
    println(measuredDurations.mkString(", "))
  }
}