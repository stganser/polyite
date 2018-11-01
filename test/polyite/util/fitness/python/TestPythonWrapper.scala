package polyite.util.fitness.python

import org.junit.Test
import java.io.File

class TestPythonWrapper {

  @Test
  def test() {
    val pyw = new PythonWrapper(None)
    pyw.eval("a = 42")
    val a = pyw.getValue("a").toInt
    assert(a == 42)
  }

  @Test
  def testWithVEnv() {
    val vEnv : File = new File("/tmp/test_env")
    if (vEnv.exists())
      vEnv.delete()
    vEnv.mkdir()
    val virtualenvPb : ProcessBuilder = new ProcessBuilder("virtualenv", vEnv.getAbsolutePath)
    val virtualenv : Process = virtualenvPb.start()
    val pyw = new PythonWrapper(Some(vEnv))
    pyw.eval("a = 42")
    val a = pyw.getValue("a").toInt
    assert(a == 42)
  }
}