package polyite.util

import java.io.File
import java.io.IOException
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.HashMap
import java.util.logging.Logger

/**
  * Utility for loading data from CSV files.
  */
object CSVUtil {

  val myLogger : Logger = Logger.getLogger("")

  /**
    * Parses the content of CSV file {@code f} into a list of maps. The first line of {@code f} is interpreted as the
    * list of column names. The nth maps the column names to the values stored in row n.
    * @param colSeparator column separator
    * @throws IOException thrown if f cannot be read or is malformed.
    */
  def loadCSV(f : File, colSeparator : Char) : List[Map[String, String]] = {

    var dummy : Char = 'a'
    while (dummy == colSeparator && dummy < 'z')
      dummy = (dummy + 1).toChar

    if (dummy == colSeparator)
      throw new IllegalStateException("Cannot work with the given column separator: " + colSeparator)

    if (!(f.exists() && f.canRead()))
      throw new IOException("Cannot access " + f);
    val reader : BufferedReader = new BufferedReader(new FileReader(f));
    val header : String = reader.readLine()
    val columnNames : Array[String] = header.split(colSeparator).map(_.trim())
    var line = reader.readLine()

    var result : List[Map[String, String]] = List.empty

    var lineNum : Int = 2

    while (line != null) {
      line = line + colSeparator + dummy
      val tokens : Array[String] = line.split(colSeparator).map(_.trim()).dropRight(1)
      if (tokens.length > columnNames.length)
        throw new IOException("Line " + lineNum + " of " + f + " contains more values than the number of columns that "
          + " are defined in the header.")
      lineNum += 1
      line = reader.readLine()

      val lineMap : HashMap[String, String] = HashMap.empty

      for ((value, key) <- tokens.zip(columnNames)) {
        lineMap.put(key, value)
      }
      result ::= lineMap.toMap
    }
    return result.reverse
  }

  /**
    * Takes a map that corresponds to a line of a CSV file and checks whether it contains a value for the given column key.
    * If the value {@code v} exists {@code Some(v)} is returned. Otherwise a warning is logged and {@code None} is returned.
    */
  def getValueForColumnKey(line : Map[String, String], columnKey : String, lineNum : Int, csvFileName : String) : Option[String] = {
    line.get(columnKey) match {
      case None => {
        myLogger.warning("Value for column " + columnKey + " is missing in line " + lineNum + " of " + csvFileName + ".")
        return None
      }
      case x @ Some(_) => return x
    }
  }

  /**
    * Takes a map that corresponds to a line of a CSV file and checks whether it contains a value for the given column key.
    * If the value {@code v} exists and can be parsed by {@code convert} {@code Some(convert(v))} is returned. Otherwise
    * a warning is logged and {@code None} is returned. {@code convert} is expected to throw a {@code NumberFormatException}
    * if the parsing fails.
    */
  def getValueForColumnKey[T](line : Map[String, String], columnKey : String, lineNum : Int, csvFileName : String,
    convert : String => T, typeName : String) : Option[T] = {
    getValueForColumnKey(line, columnKey, lineNum, csvFileName) match {
      case None => return None
      case Some(s) => try {
        return Some(convert(s))
      } catch {
        case e : NumberFormatException => {
          myLogger.warning("Cannot interpret the value in line " + lineNum + ", column " + columnKey + " of " + csvFileName + " as an instance of " + typeName + ".")
          return None
        }
      }
    }
  }

  /**
    * Takes a map that corresponds to a line of a CSV file and checks whether it contains an integer value for the given
    * column key. If the value {@code v} exists and can is an integer @code Some(v)} is returned. Otherwise a warning is
    * logged and {@code None} is returned.
    */
  def getIntegerValueFromColumnKey(line : Map[String, String], columnKey : String, lineNum : Int, csvFileName : String) : Option[Int] =
    getValueForColumnKey(line, columnKey, lineNum, csvFileName, (s : String) => s.toInt, "integer")

  /**
    * Takes a map that corresponds to a line of a CSV file and checks whether it contains a double value for the given
    * column key. If the value {@code v} exists and can is an integer @code Some(v)} is returned. Otherwise a warning is
    * logged and {@code None} is returned.
    */
  def getDoubleValueFromColumnKey(line : Map[String, String], columnKey : String, lineNum : Int, csvFileName : String) : Option[Double] =
    getValueForColumnKey(line, columnKey, lineNum, csvFileName, (s : String) => s.toDouble, "floating point")

  /**
    * Takes a map that corresponds to a line of a CSV file and checks whether it contains a long integer value for the given
    * column key. If the value {@code v} exists and can is a long integer @code Some(v)} is returned. Otherwise a warning is
    * logged and {@code None} is returned.
    */
  def getLongValueFromColumnKey(line : Map[String, String], columnKey : String, lineNum : Int, csvFileName : String) : Option[Long] =
    getValueForColumnKey(line, columnKey, lineNum, csvFileName, (s : String) => s.toLong, "long integer")
    
  /**
    * Takes a map that corresponds to a line of a CSV file and checks whether it contains a boolean value for the given
    * column key. If the value {@code v} exists and can is a boolean @code Some(v)} is returned. Otherwise a warning is
    * logged and {@code None} is returned.
    */
  def getBooleanValueFromColumnKey(line : Map[String, String], columnKey : String, lineNum : Int, csvFileName : String) : Option[Boolean] =
    getValueForColumnKey(line, columnKey, lineNum, csvFileName, (s : String) => s.toBoolean, "boolean")
}