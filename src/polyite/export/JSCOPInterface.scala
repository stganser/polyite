package polyite.export

import scala.util.parsing.json.JSON
import scala.io.Source
import isl.Conversions._
import collection.mutable.HashMap
import java.io.PrintWriter
import java.io.File
import java.io.IOException
import isl.Isl.TypeAliases._
import java.util.logging.Logger
import java.util.logging.Level
import isl.Isl
import polyite.ScopInfo
import scala.BigDecimal
import scala.Range

object JSCOPInterface {

  private val jsonIndentWidth : Int = 4
  private val myLogger : Logger = Logger.getLogger("")

  def readJSCOP(fileName : String) : Option[ScopInfo] = {

    if (fileName == null) {
      throw new NullPointerException
    }

    val jscopFile = new java.io.File(fileName)

    if (!(jscopFile.exists() && jscopFile.canRead())) {
      return None
    }

    val input : Source = Source.fromFile(fileName)
    val jscopStr : String = try input.mkString finally input.close()
    return parseJSCOP(jscopStr)
  }

  def parseJSCOP(s : String) : Option[ScopInfo] = {
    parseJSON(s) match {
      case None => {
        myLogger.warning("Basic JSON parsing failed.")
        return None
      }
      case Some(parseTree) => {
        extractScop(parseTree) match {
          case None => return None
          case Some(scop) => {
            return Some(scop.setJSCOP(parseTree))
          }
        }
      }
    }
  }

  private def parseJSON(s : String) : Option[Any] = {
    // Globally parse numerics to BigDecimal instead of double
    val myConversionFunc = { input : String => BigDecimal(input) }
    JSON.globalNumberParser = myConversionFunc
    return JSON.parseFull(s)
  }

  private def extractScop(parseTree : Any) : Option[ScopInfo] = {

    if (!parseTree.isInstanceOf[Map[_, Any]])
      return None
    val jsonMap : Map[String, Any] = parseTree.asInstanceOf[Map[String, Any]]
    if (!jsonMap.contains("statements")) {
      myLogger.warning("Couldn't find property \"statements\".")
      return None
    }
    var jsonVal : Any = jsonMap("statements")
    if (!jsonVal.isInstanceOf[List[Any]]) {
      myLogger.warning("Value of \"statements\" is not a list.")
      return None
    }
    val jsonStatementsList : List[Any] = jsonVal.asInstanceOf[List[Any]]
    var scop : ScopInfo = new ScopInfo

    for (childNode <- jsonStatementsList) {

      if (!childNode.isInstanceOf[Map[_, Any]]) {
        myLogger.warning("An element of the \"statements\" list is not a map.")
        return None
      }

      val jsonStatementMap = childNode.asInstanceOf[Map[String, Any]]
      if (!List("accesses", "domain", "schedule", "name")
        .forall(jsonStatementMap.contains)) {
        myLogger.warning("Statement doesn't have the right properties.")
        return None
      }

      jsonVal = jsonStatementMap("domain")
      if (!jsonVal.isInstanceOf[String]) {
        myLogger.warning("Value of \"domain\" is not a string.")
        return None
      }
      scop = scop.addDomain(isl.Set.readFromStr(Isl.ctx, jsonVal.asInstanceOf[String]))

      jsonVal = jsonStatementMap("schedule")
      if (!jsonVal.isInstanceOf[String]) {
        myLogger.warning("Value of \"schedule\" is not a string.")
        return None
      }
      scop = scop.addSchedule(isl.Map.readFromStr(
        Isl.ctx, jsonVal.asInstanceOf[String]))

      jsonVal = jsonStatementMap("accesses")
      if (!jsonVal.isInstanceOf[List[Any]]) {
        myLogger.warning("Value of \"accesses\" is not a list.")
        return None
      }
      val jsonAccessesList = jsonVal.asInstanceOf[List[Any]]

      for (jsonVal <- jsonAccessesList) {
        if (!jsonVal.isInstanceOf[Map[_, Any]])
          return None
        var jsonAccessesMap = jsonVal.asInstanceOf[Map[String, Any]]
        if (!List("kind", "relation").forall(jsonAccessesMap.contains)) {
          myLogger.warning("access object does not have the right properties.")
          return None
        }
        val kind = jsonAccessesMap("kind")
        val relation = jsonAccessesMap("relation")
        kind.toString() match {
          case "read" =>
            scop = scop.addRds(isl.Map.readFromStr(Isl.ctx, relation.toString()))
          case "write" =>
            scop = scop.addWrs(isl.Map.readFromStr(Isl.ctx, relation.toString()))
        }
      }
    }

    if (!jsonMap.contains("context")) {
      myLogger.warning("Couldn't find property \"context\".")
      return None
    }
    jsonVal = jsonMap("context")
    if (!jsonVal.isInstanceOf[String]) {
      myLogger.warning("Value of \"context\" is not a string.")
      return None
    }
    scop = scop.setParams(isl.UnionSet.readFromStr(Isl.ctx, jsonVal.toString()))

    Some(scop)
  }

  def genExtJSCOP(scop : ScopInfo, newSched : isl.UnionMap,
    schedTree : isl.Schedule, doubleQuoteStringsInSchedTree : Boolean) : String = {
    return JSONExporter.generate(replaceSchedExt(scop, newSched, schedTree,
      doubleQuoteStringsInSchedTree), jsonIndentWidth) + '\n'
  }

  def replaceSchedExt(scop : ScopInfo, newSched : isl.UnionMap,
    schedTree : isl.Schedule, doubleQuoteStringsInSchedTree : Boolean) : Map[String, Any] = {
    var scopMap : Map[String, Any] = replaceSched(scop, newSched)
    var schedTreeStr : String = schedTree.toString()
    // make the serialized schedule tree valid JSON: we must surround the keys
    // with quotation marks.
    if (doubleQuoteStringsInSchedTree)
      schedTreeStr = schedTreeStr.replaceAll("\"", "\\\\\\\\\"")
    else
      schedTreeStr = schedTreeStr.replaceAll("\"", "\\\\\"")
    return scopMap + (("schedTree", schedTreeStr))
  }

  def replaceSched(scop : ScopInfo, newSched : isl.UnionMap) : Map[String, Any] = {
    val stmt2Sched : collection.mutable.Map[String, isl.Map] = new HashMap[String, isl.Map]();
    newSched.foreachMap(((m : isl.Map) => stmt2Sched.put(m.getTupleId(isl.DimType.In).toString(), m)))

    def rejectJSCOP() {
      throw new IllegalArgumentException("scop.getJscop does not have the expected format.")
    }

    // insert the new statement schedules into the JSCOP AST
    if (!scop.getJscop.isInstanceOf[Map[_, Any]])
      rejectJSCOP()
    var scopMap = scop.getJscop.asInstanceOf[Map[String, Any]]
    if (!scopMap.contains("statements"))
      rejectJSCOP()
    var jsonVal = scopMap("statements")
    if (!jsonVal.isInstanceOf[List[Any]])
      rejectJSCOP()
    var stmtList : List[Any] = jsonVal.asInstanceOf[List[Any]]
    stmtList = stmtList.map { (elem : Any) =>
      {
        if (!elem.isInstanceOf[Map[_, Any]])
          rejectJSCOP()
        var stmtMap : Map[String, Any] = elem.asInstanceOf[Map[String, Any]]
        if (!stmtMap.contains("name"))
          rejectJSCOP()
        val stmtName : String = stmtMap("name").toString
        val newStmtSched : String = stmt2Sched(stmtName).toString()
        stmtMap + (("schedule", newStmtSched))
      }
    }
    scopMap += (("statements", stmtList))
    return scopMap
  }

  def genJSCOP(scop : ScopInfo, newSched : isl.UnionMap) : String = {
    val scopMap : Any = replaceSched(scop, newSched)
    return JSONExporter.generate(scopMap, jsonIndentWidth) + '\n'
  }

  def exportSchedule(jscopFile : File, scop : ScopInfo, newSched : isl.UnionMap) {
    val json : String = genJSCOP(scop, newSched)
    writeFile(json, jscopFile)
  }

  private def writeFile(content : String, dest : File) {
    if (!dest.canWrite() && !dest.getAbsoluteFile.getParentFile.canWrite()) {
      throw new IllegalArgumentException("Cannot write to " + dest.getPath)
    }

    var writer : PrintWriter = null
    try {
      writer = new PrintWriter(dest)
      writer.write(content)
      writer.flush()
    } finally {
      if (writer != null)
        try {
          writer.close()
        } catch {
          case e : IOException => {
            myLogger.log(Level.SEVERE,
              "Failed to close writer.", e)
          }
        }
    }
  }

  def exportScheduleExt(jscopFile : File, scop : ScopInfo,
    newSched : isl.UnionMap, schedTree : isl.Schedule) {
    val json = genExtJSCOP(scop, newSched, schedTree, false)
    writeFile(json, jscopFile)
  }
}