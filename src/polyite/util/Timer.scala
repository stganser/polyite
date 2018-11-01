package polyite.util

import scala.collection.mutable.HashMap

/**
  * Implements timers, that can be stopped and started. Accounting happens in milliseconds. A named timer can be
  * registered (started), stopped, restarted, queried and removed.
  *
  * This implementation is thread-safe.
  */
object Timer {
  private val timers : HashMap[String, SingleTimer] = HashMap.empty

  /**
    * Starts a new timer named {@code name}.
    * @throws IllegalStateException An {@code IllegalStateException} will be thrown, if a timer named {@code name}
    * already exists.
    */
  def startTimer(name : String) {
    timers.synchronized {
      if (timers.contains(name))
        throw new IllegalStateException("A timer named \"" + name + "\" already exists.")
      timers.put(name, SingleTimer(System.currentTimeMillis(), 0, true))
    }
  }

  /**
    * Stops the timer named {@code name}.
    * @return The current value of the timer in milliseconds.
    * @throws IllegalStateException An {@code IllegalStateException} will be thrown, if the timer does not exist or was
    * inactive already.
    */
  def stopTimer(name : String) : Long = {
    timers.synchronized {
      if (!timers.contains(name))
        throw new IllegalStateException("A timer named \"" + name + "\" does not exist.")
      val timer : SingleTimer = timers(name)
      if (!timer.active)
        throw new IllegalStateException("The timer named \"" + name + "\" was inactive already.")
      val newTimer : SingleTimer = SingleTimer(timer.startTime, timer.account + (System.currentTimeMillis() - timer.startTime), false)
      timers.put(name, newTimer)
      return newTimer.account
    }
  }

  /**
    * Restarts the timer with the given name.
    * @throws IllegalStateException An {@code IllegalStateException} will be thrown, if the timer does not exist or was
    * active already.
    */
  def restartTimer(name : String) {
    timers.synchronized {
      if (!timers.contains(name))
        throw new IllegalStateException("A timer named \"" + name + "\" does not exist.")
      val timer : SingleTimer = timers(name)
      if (timer.active)
        throw new IllegalStateException("The timer named \"" + name + "\" was active already.")
      val newTimer : SingleTimer = SingleTimer(System.currentTimeMillis(), timer.account, true)
      timers.put(name, newTimer)
    }
  }

  /**
    * Queries the timer with the given name.
    * @throws IllegalStateException An {@code IllegalStateException} will be thrown, if the timer does not exist.
    */
  def queryTimer(name : String) : Long = {
    var timer : SingleTimer = null
    timers.synchronized {
      if (!timers.contains(name))
        throw new IllegalStateException("A timer named \"" + name + "\" does not exist.")
      timer = timers(name)
    }
    return queryTimer(timer)
  }

  private def queryTimer(timer : SingleTimer) : Long = {
    if (timer.active)
      return timer.account + (System.currentTimeMillis() - timer.startTime)
    return timer.account
  }

  /**
    * Stops the timer with the given name and returns the duration in milliseconds for which it was active.
    * @throws IllegalStateException An {@code IllegalStateException} will be thrown, if the timer does not exist.
    */
  def removeTimer(name : String) : Long = {
    var timer : SingleTimer = null
    timers.synchronized {
      if (!timers.contains(name))
        throw new IllegalStateException("A timer named \"" + name + "\" does not exist.")
      timer = timers.remove(name).get
    }
    return queryTimer(timer)
  }

  /**
    * Checks whether a timer of the given name exists.
    */
  def exists(name : String) : Boolean = {
    timers.synchronized {
      return timers.contains(name)
    }
  }

  /**
    * Checks whether the timer named {@code name} is active.
    * @return Returns {@code true}, iff a timer named {@code name} exists and is active.
    */
  def isActive(name : String) : Boolean = {
    timers.synchronized {
      return timers.get(name).getOrElse(return false).active
    }
  }

  private case class SingleTimer(startTime : Long, account : Long = 0, active : Boolean)
}