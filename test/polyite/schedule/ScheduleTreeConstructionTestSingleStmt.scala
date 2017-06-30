package polyite.schedule

import org.junit.Test
import isl.Isl
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import org.junit.Assert._

class ScheduleTreeConstructionTestSingleStmt extends AbstractScopTest {

  scopStr = """{ 
   "context" : "[n] -> {  : -2147483648 <= n <= 2147483647 }",
   "name" : "test",
   "statements" : [
      { 
         "accesses" : [
         ], 
         "domain" : "[n] -> { S0[i, j] : 1 <= i <= n and 1 <= j <= n }",
         "name" : "S0",
         "schedule" : "[n] -> { S0[i, j] -> [i, j] }"
      }
   ]
}"""

  def runFurtherPreparation() = ()

  @Test
  def testSkewing() {
    val ctx : isl.Ctx = Isl.ctx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [i + 2*j, i] }")
    val expectedResult = "{ domain: \"[n] -> { S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i0 + 2i1)] }, { S0[i0, i1] -> [(i0)] }]\", permutable: 1, coincident: [ 1, 1 ] } }"
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(expectedResult, schedTree.toString())
  }
  
  @Test
  def testSkewing1() {
    val ctx : isl.Ctx = Isl.ctx
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [i + j, i] }")
    val expectedResult = "{ domain: \"[n] -> { S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i0 + i1)] }, { S0[i0, i1] -> [(i0)] }]\", permutable: 1, coincident: [ 1, 1 ] } }"
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(expectedResult, schedTree.toString())
  }
}