package polyite.schedule

import org.junit.Test
import isl.Isl
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import org.junit.Assert._
import polyite.schedule.schedule_tree.ScheduleNode

class ScheduleEqualityTest extends AbstractScopTest {

  scopStr = """{ 
   "context" : "[n] -> {  : -2147483648 <= n <= 2147483647 }",
   "name" : "test",
   "statements" : [
      { 
         "accesses" : [
         ], 
         "domain" : "[n] -> { S0[i] : 1 <= i <= n }",
         "name" : "S0",
         "schedule" : "[n] -> { S0[i] -> [i, 0] }"
      },
      {
         "accesses" : [
         ],
         "domain" : "[n] -> { S1[i] : 1 <= i <= n }",
         "name" : "S1",
         "schedule" : "[n] -> { S1[i] -> [i, 1] }"
      },
      {
         "accesses" : [
         ],
         "domain" : "[n] -> { S2[i] : 1 <= i <= n }",
         "name" : "S1",
         "schedule" : "[n] -> { S2[i] -> [i, 2] }"
      }
   ]
}"""

  def runFurtherPreparation() = ()

  @Test
  def test() {
    val ctx : isl.Ctx = Isl.ctx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i] -> [3*i + 0]; S1[i] -> [3*i + 1]; S2[i] -> [3*i + 2] }")
    val schedTree : ScheduleNode = ScheduleTreeConstruction.islUnionMap2ScheduleTree(sched, domInfo, scop, deps, conf)
    val sched1 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i] -> [3*i + 0]; S1[i] -> [3*i + 1]; S2[i] -> [3*i + 2] }")
    val schedTree1 : ScheduleNode = ScheduleTreeConstruction.islUnionMap2ScheduleTree(sched1, domInfo, scop, deps, conf)
    println(schedTree)
    assertTrue(schedTree.equals(schedTree1))
  }
}