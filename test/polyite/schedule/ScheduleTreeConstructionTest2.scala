package polyite.schedule

import org.junit.Test
import isl.Isl
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import org.junit.Assert._

class ScheduleTreeConstructionTest2 extends AbstractScopTest {

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

  val expectedResult : String = "{ domain: \"[n] -> { S1[i] : i >= 1 and i <= n; S0[i] : i >= 1 and i <= n; S2[i] : i >= 1 and i <= n }\", child: { schedule: \"[n] -> [{ S1[i0] -> [(i0)]; S2[i0] -> [(i0)]; S0[i0] -> [(i0)] }]\", coincident: [ 1 ], child: { sequence: [ { filter: \"[n] -> { S0[i] }\" }, { filter: \"[n] -> { S1[i] }\" }, { filter: \"[n] -> { S2[i] }\" } ] } } }"

  @Test
  def test() {
    val ctx : isl.Ctx = Isl.ctx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i] -> [3*i + 0]; S1[i] -> [3*i + 1]; S2[i] -> [3*i + 2] }")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(expectedResult, schedTree.toString())
  }

  @Test
  def test1() {
    val ctx : isl.Ctx = Isl.ctx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i] -> [3*i + 0, 0]; S1[i] -> [3*i + 1, 0]; S2[i] -> [3*i + 1, 1] }")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(expectedResult, schedTree.toString())
  }

  @Test
  def test2() {
    val ctx : isl.Ctx = Isl.ctx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i] -> [3*i + 0 - 4 + 5*n, 0]; S1[i] -> [3*i + 1 - 4 + 5*n, 0]; S2[i] -> [3*i + 1 - 4 + 5*n, 1] }")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(expectedResult, schedTree.toString())
  }
}