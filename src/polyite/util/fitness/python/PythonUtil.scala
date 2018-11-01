package polyite.util.fitness.python

/**
 * Utility functions for using Jep in Scala.
 */
object PythonUtil {
  
  /**
   * Convert the given list of strings to the Python list notation.
   */
  def mkPythonStrList(l : List[Any]) : String = l.mkString("[\"", "\", \"", "\"]")

  /**
   * Convert the given Scala {@code Bool} into a Python boolean literal.
   */
  def toPythonBool(b : Boolean) = if (b) "True" else "False"
}