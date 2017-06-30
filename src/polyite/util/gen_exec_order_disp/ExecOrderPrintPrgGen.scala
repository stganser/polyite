package polyite.util.gen_exec_order_disp

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.InputStreamReader
import java.util.LinkedList
import java.util.logging.Logger

import scala.collection.mutable.HashMap
import scala.collection.mutable.StringBuilder

import polyite.MainUtil
import polyite.ScopInfo

import isl.Conversions.convertLambdaToVoidCallback1
import isl.Isl.TypeAliases.T_PAR
import isl.Isl.TypeAliases.T_SET

object ExecOrderPrintPrgGen {

  val myLogger : Logger = Logger.getLogger("")

  def genPrg(optPb : ProcessBuilder, ctxVarValues : Map[String, Int], scop : ScopInfo) : Option[String] = {
    val opt : Process = optPb.start()
    val stderr : BufferedReader = new BufferedReader(new InputStreamReader(opt.getErrorStream))
    val stdout : BufferedReader = new BufferedReader(new InputStreamReader(opt.getInputStream))
    var astLines : List[String] = List.empty
    var line : String = stdout.readLine()
    while (line != null && !line.matches(".*isl ast.*"))
      line = stdout.readLine()

    while (line != null && !line.matches(".*original code.*")) {
      astLines ::= line
      line = stdout.readLine()
    }
    opt.waitFor()

    stdout.close()
    line = stderr.readLine()
    val sb : StringBuilder = StringBuilder.newBuilder
    while (line != null) {
      sb.append(line).append('\n')
      line = stderr.readLine()
    }
    stderr.close

    astLines = astLines.reverse.drop(5).dropRight(3)
    val code : String = astLines.mkString("\n")

    return Some(genPrgFromCode(code, scop, ctxVarValues))
  }

  def genPrgFromCode(code : String, scop : ScopInfo, ctxVarValues : Map[String, Int]) : String = {
    val params : isl.Set = scop.getDomain.params()
    val paramNames : Iterable[String] = for (pIdx <- 0 until params.dim(T_PAR))
      yield params.getDimId(T_PAR, pIdx).toString()

    val stmt2nIterators : HashMap[String, Int] = HashMap.empty
    scop.getDomain.foreachSet((s : isl.Set) => {
      stmt2nIterators.put(s.getTupleId.toString(), s.dim(T_SET))
    })

    // header
    val prog : StringBuilder = StringBuilder.newBuilder
    prog.append("#include <stdio.h>\n")
    prog.append("#include \"math.h\"\n\n")

    // macros
    prog.append("#define floord(n,d) (((n)<0) ? -((-(n)+(d)-1)/(d)) : (n)/(d))\n")
    prog.append("#define max(x,y)    ((x) > (y) ? (x) : (y))\n")
    prog.append("#define min(x,y)    ((x) < (y) ? (x) : (y))\n")

    // statement dummies
    for (stmtId : String <- stmt2nIterators.keys) {
      prog.append("void ").append(stmtId).append('(')
      val nIter : Int = stmt2nIterators(stmtId)
      for (i <- 0 until nIter - 1)
        prog.append("int ").append(idx2IterVarName(i)).append(", ")
      if (nIter > 0)
        prog.append("int ").append(idx2IterVarName(nIter - 1))
      prog.append(") {\n")
      prog.append("printf(\"").append(stmtId).append("(")
      for (i <- 0 until nIter - 1)
        prog.append("%d, ")
      if (nIter > 0)
        prog.append("%d")
      prog.append(")\\n\", ")

      for (i <- 0 until nIter - 1)
        prog.append(idx2IterVarName(i)).append(", ")
      if (nIter > 0)
        prog.append(idx2IterVarName(nIter - 1))

      prog.append(");\n")
      prog.append("}\n\n")
    }

    // main
    prog.append("int main(int argc, char **argv) {\n")
    // declare and init params
    for (p <- paramNames) {
      prog.append("int ").append(p).append(" = ").append(ctxVarValues(p).toString).append(";\n")
    }
    // append the SCoP
    prog.append('\n')
      .append(code)
      .append('\n')
      .append("}\n")

    return prog.toString()
  }

  def idx2IterVarName(i : Int) : String = f"i${i}"

  def main(args : Array[String]) : Unit = {
    if (!checkNumArgs(args))
      return

    val optCommandList : LinkedList[String] = new LinkedList
    args(0).split("\\s+").map(optCommandList.add)
    val jscopFilePath : String = args(1)
    val outputFilePath : String = args(2)

    val (_ : File, scop : ScopInfo) = MainUtil.loadScop(jscopFilePath) match {
      case None    => return
      case Some(t) => t
    }

    val pb : ProcessBuilder = new ProcessBuilder(optCommandList)

    var ctxVarValues : Map[String, Int] = Map.empty
    val context : isl.Set = scop.getDomain.params()
    for (i <- 0 until context.dim(T_PAR)) {
      ctxVarValues += ((context.getDimName(T_PAR, i), 10))
    }

    ExecOrderPrintPrgGen.genPrg(pb, ctxVarValues, scop) match {
      case None => {
        myLogger.warning("Failed to generate code.")
        return
      }
      case Some(s) => {
        val w : BufferedWriter = new BufferedWriter(new FileWriter(outputFilePath))
        try {
          w.write(s)
        } finally {
          w.close()
        }
      }
    }
  }

  def checkNumArgs(args : Array[String]) : Boolean = {
    if (args.length < 3) {
      myLogger.warning("Expected two arguments: <opt command> <JSCOP file> <output file>")
      return false
    }
    return true
  }
}