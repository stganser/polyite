package polyite.util

import java.util.logging.Logger

/**
  * Utility for parsing argument values.
  */
object ParseArgs {

  private val myLogger : Logger = Logger.getLogger("")

  /**
    * Parses a boolean value. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @param v String to parse.
    * @param k name of the property that {@code v} belongs to. Needed for error
    *          messages.
    *
    * @return Returns Some(x) if v could be interpreted as boolean x. Otherwise
    * returns None.
    */
  def parseBoolean(v : String, k : String) : Option[Boolean] = {
    try {
      return Some(v.toBoolean)
    } catch {
      case e : IllegalArgumentException => {
        complainAboutMalformedValue("boolean", k, v)
        return None
      }
    }
  }

  /**
    * Parses an integer value. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @param v String to parse.
    * @param k name of the property that {@code v} belongs to. Needed for error
    *          messages.
    *
    *
    * @return Returns Some(x) if v could be interpreted as integer x. Otherwise
    * returns None.
    */
  def parseInt(v : String, k : String) : Option[Int] = {
    return parseNumber(v, k, _.toInt, "integer")
  }

  /**
    * Parses a double value. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @param v String to parse.
    * @param k name of the property that {@code v} belongs to. Needed for error
    *          messages.
    *
    *
    * @return Returns Some(x) if v could be interpreted as double x. Otherwise
    * returns None.
    */
  def parseDouble(v : String, k : String) : Option[Double] = {
    return parseNumber(v, k, _.toDouble, "double")
  }

  /**
    * Parses a rational value. Logs a message if the property is missing or the
    * value is malformed.
    *
    * @param v String to parse.
    * @param k name of the property that {@code v} belongs to. Needed for error
    *          messages.
    *
    *
    * @return Returns Some(x) if v could be interpreted as rational x. Otherwise
    * returns None.
    */
  def parseRational(v : String, k : String) : Option[Rat] = {
    return parseNumber(v, k, Rat.fromString, "rational")
  }

  /**
    * Parses a long integer value. Logs a message if the property is missing or
    * the value is malformed.
    *
    * @param v String to parse.
    * @param k name of the property that {@code v} belongs to. Needed for error
    *          messages.
    *
    *
    * @return Returns Some(x) if v could be interpreted as long integer x.
    * Otherwise returns None.
    */
  def parseLong(v : String, k : String) : Option[Long] = {
    return parseNumber(v, k, _.toLong, "long integer")
  }

  private def parseNumber[T](v : String, k : String,
    convert : String => T, typeName : String) : Option[T] = {
    try {
      return Some(convert(v))
    } catch {
      case e : NumberFormatException => {
        complainAboutMalformedValue(typeName, k, v)
        return None
      }
    }
  }

  /**
    * Logs a message complaining that the value {@code s} of property
    * {@code key} with expected type {@code typeName} is malformed.
    */
  def complainAboutMalformedValue(typeName : String, key : String, s: String) {
    myLogger.warning("Failed to parse " + typeName + " property \""
      + key + "\": " + s)
  }
}