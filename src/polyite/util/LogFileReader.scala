package polyite.util

import java.io.File
import scala.collection.Iterator
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.StringBuilder
import java.io.IOException

object LogFileReader {

  private val logLevelRegex : String = "^(SEVERE|WARNING|INFO|CONFIG|FINE|FINER|FINEST):"
  
  val defaultHeaderRegex : String = {
    lazy val months : String = "(Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|Jan)"
    val n : String = "[0-9]"
    val day : String = n + n
    val year : String = n + n + n + n
    val timePart : String = n + n + "?"
    val time : String = timePart + ":" + timePart + ":" + timePart + " (AM|PM)"
    months + " " + day + ", " + year + " " + time + ".*"
  }

  def main(args : Array[String]) : Unit = {
    val f : File = new File("/tmp/genetic_algorithm_correlation.log")
    val r : LogFileReader = new LogFileReader(f)
    
    while (r.hasNext) {
      println(r.next.get)
      println("----")
    }
  }
}

/**
  * This class allows to iterate forwardly over the messages of a log file produced by the {@code SimpleFormatter} of the Java logging
  * facilities (compare {@code java.util.logging}). Removes any prefix that matches {@code ^(SEVERE|WARNING|INFO|CONFIG|FINE|FINER|FINEST):}
  * from the first line of a log message.
  * @param headerRegex regular expression that identifies the first line of a log entry. This line is expected not
  * to contain any content.
  */
class LogFileReader(logFile : File, headerRegex : String) {

  /**
    * Constructs a {@code LogFileReader} that uses the default header regex {@code defaultHeaderRegex}.
    */
  def this(logFile : File) {
    this(logFile, LogFileReader.defaultHeaderRegex)
  }

  private val reader : BufferedReader = {
    val r = new BufferedReader(new FileReader(logFile))
    r.readLine()
    r
  }

  private var hasN : Boolean = true

  /**
    * Check whether another message exists.
    */
  def hasNext : Boolean = hasN

  /**
    * Get the next message.
    * @return {@code Some(m)} iff a next message {@code m} exists. Otherwise, {@code None} is returned.
    * @throws IOException
    */
  def next : Option[String] = {
    if (!hasN)
      return None

    try {
      var line : String = reader.readLine()
      val sb : StringBuilder = StringBuilder.newBuilder
      
      def continue = line != null && !line.matches(headerRegex)
      
      while (continue) {
        sb.append(line.replaceFirst(LogFileReader.logLevelRegex, ""))
        line = reader.readLine()
        
        if (continue)
          sb.append('\n')
      }
      hasN = line != null
      return Some(sb.toString)
    } catch {
      case e : IOException => {
        reader.close()
        hasN = false
        throw e
      }
    }
  }
}