package polyite.export

import org.junit.Test
import polyite.AbstractTest
import java.util.logging.Logger
import polyite.schedule.AbstractScopTest
import polyite.config.Config
import scala.collection.mutable.HashMap
import polyite.schedule.Schedule
import polyite.sched_eval.EvalResult
import polyite.schedule.ScheduleUtils
import java.io.File

class JSONLogExporterTest extends AbstractScopTest {

  scopStr = """{ 
   "context" : "[n] -> {  : -2147483648 <= n <= 2147483647 }",
   "name" : "test",
   "statements" : [
      { 
         "accesses" : [
            {
              "kind" : "write",
              "relation" : "[n] -> { S0[i, j] -> A[i] }"
            },
            {
              "kind" : "read",
              "relation" : "[n] -> { S0[i, j] -> A[i - 1] }"
            }
         ], 
         "domain" : "[n] -> { S0[i, j] : 1 <= i <= n and 1 <= j <= n }",
         "name" : "S0",
         "schedule" : "[n] -> { S0[i, j] -> [i, j, 0] }"
      },
      {
         "accesses" : [
            {
              "kind" : "read",
              "relation" : "[n] -> { S1[i, j] -> A[i] }"
            }
         ],
         "domain" : "[n] -> { S1[i, j] : 1 <= i <= n and 1 <= j <= n }",
         "name" : "S1",
         "schedule" : "[n] -> { S1[i, j] -> [i, j, 1] }"
      },
      {
         "accesses" : [],
         "domain" : "[n] -> { S2[i, j] : 1 <= i <= n and 1 <= j <= n }",
         "name" : "S2",
         "schedule" : "[n] -> { S2[i, j] -> [i, j, 2] }"
      }
   ]
}"""

  def runFurtherPreparation() = ()

  @Test
  def test() {
    val conf : Config = super.createTestConfig() match {
      case None    => return
      case Some(c) => c
    }
    val population : HashMap[Schedule, EvalResult] = HashMap.empty
    val defaultEvalResult : EvalResult = EvalResult.create(true, Some(List(5, 6, 7, 8)), true, Some(List(1.3, 1.4)), None, Some(false), Some(true),
      None, Some(false), Some(true), Some(List(1, 2, 3, 4)), None, Some(List(5, 6, 7, 8, 98)), Some(false), Some(4.5), None, Some(8.9), false)
    ScheduleUtils.genRandSchedules(domInfo, deps, 5, conf.maxNumRays, conf.maxNumLines, conf).map { s =>
      {
        population.put(s, defaultEvalResult)
      }
    }

    val f : File = new File("./population42.json")
    f.deleteOnExit()
    val gen : Int = 42
    ScheduleExport.exportPopulationToFile(f, population, gen)
    val (loadedPopulation : HashMap[Schedule, EvalResult]) = ScheduleExport.loadPopulationFromFile(f, domInfo, deps, gen) match {
      case None    => throw new AssertionError("faield to import the population")
      case Some(p) => p
    }
    //println(population)
    for ((s, e) <- population) {
      assert(loadedPopulation.contains(s))
      assert(loadedPopulation(s).equals(e), loadedPopulation(s))
    }
    assert(population.equals(loadedPopulation))
  }
}