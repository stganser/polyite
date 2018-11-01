package polyite.util.fitness.python

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.InputStreamReader
import java.io.FileReader
import scala.collection.mutable.StringBuilder
import java.io.File
import java.io.FileWriter

class PythonWrapper(vEnvStartScript : Option[File]) {

  private val pb : ProcessBuilder = if (vEnvStartScript.isDefined) {
    val script : File = createVirtualEnvScript(vEnvStartScript.get)
    new ProcessBuilder(script.getAbsolutePath)
  } else {
    new ProcessBuilder("python3", "-i")
  }
  private val python : Process = pb.start()
  private val stdin : BufferedWriter = new BufferedWriter(new OutputStreamWriter(python.getOutputStream))
  private val stdout : BufferedReader = new BufferedReader(new InputStreamReader(python.getInputStream))
  private val stderr : BufferedReader = new BufferedReader(new InputStreamReader(python.getErrorStream))
  private def createVirtualEnvScript(startScript : File) : File = {
    val script : File = File.createTempFile("python_env_script", ".bash")
    script.deleteOnExit()
    val w : BufferedWriter = new BufferedWriter(new FileWriter(script))
    w.write("#!/bin/bash")
    w.newLine()
    w.write("source ")
    w.write(startScript.getAbsolutePath + "/bin/activate")
    w.newLine()
    w.write("python3 -i")
    w.newLine()
    w.write("deactivate")
    w.newLine()
    w.flush()
    w.close()
    script.setExecutable(true)
    return script
  }

  def destroy() {
    python.destroy()
    python.waitFor()
  }

  def readErrStream(nLines : Int) : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    var line : String = stderr.readLine()
    var n = nLines
    while (n > 0 && line != null) {
      sb.append(line)
      sb.append('\n')
      n -= 1
      if (n > 0)
        line = stderr.readLine()
    }
    return sb.toString()
  }

  def eval(stmt : String) {
    assert(python.isAlive())
    stdin.write(stmt)
    stdin.newLine()
    stdin.flush()
    synchronize()
  }

  def synchronize() {
    stdin.write("print(\"blah\")")
    stdin.newLine()
    stdin.flush()
    var line : String = null
    do {
      line = stdout.readLine()
    } while (line != "blah")
  }

  def getValue(variable : String) : String = {
    stdin.write(f"print(str(${variable}))")
    stdin.newLine()
    stdin.flush()
    return stdout.readLine()
  }

  def runScript(scriptPath : String) {
    val r : BufferedReader = new BufferedReader(new FileReader(scriptPath))
    var line : String = r.readLine()
    while (line != null) {
      assert(python.isAlive())
      stdin.write(line)
      stdin.newLine()
      stdin.flush()
      line = r.readLine()
    }
    r.close()
    synchronize()
  }
}