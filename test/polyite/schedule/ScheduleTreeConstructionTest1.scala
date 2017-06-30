package polyite.schedule

import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import org.junit.Assert.assertEquals
import org.junit.Test

class ScheduleTreeConstructionTest1 extends AbstractScopTest {

  scopStr = """{
   "context" : "[n, m] -> {  : -2147483648 <= n <= 2147483647 and n = m }",
   "name" : "test",
   "statements" : [
      { 
         "domain" : "[n, m] -> { S0[i, j] : 1 <= i <= n and 1 <= j <= m }",
         "name" : "S0",
         "schedule" : "[n, m] -> { S0[i, j] -> [i, j, 0] }",
         "accesses" : []
      },
      { 
         "domain" : "[n, m] -> { S1[i, j] : 1 <= i <= n and 1 <= j <= m }",
         "name" : "S1",
         "schedule" : "[n, m] -> { S1[i, j] -> [i, j, 1] }",
         "accesses" : []
      }
   ]
}"""

  def runFurtherPreparation() = ()

  @Test
  def testParamElimination() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i, j] -> [i + n, j]; S1[i, j] -> [i + n + m, j] }")
    println("starting to construct schedule tree")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf)
    println(schedTree)
    val expectedResult : String = "{ domain: \"[n, m] -> { S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= m; S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= m }\", child: { sequence: [ { filter: \"[n, m] -> { S0[i, j] }\", child: { schedule: \"[n, m] -> [{ S0[i0, i1] -> [(i0)] }, { S0[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } }, { filter: \"[n, m] -> { S1[i, j] }\", child: { schedule: \"[n, m] -> [{ S1[i0, i1] -> [(i0)] }, { S1[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } } ] } }"
    assertEquals(schedTree.toString, expectedResult)
  }

  val skewingExpectedResult : String = "{ domain: \"[n, m] -> { S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= m; S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= m }\", child: { schedule: \"[n, m] -> [{ S1[i0, i1] -> [(i0)]; S0[i0, i1] -> [(i0)] }, { S1[i0, i1] -> [(2i0 + i1)]; S0[i0, i1] -> [(4i0 + 2i1)] }]\", permutable: 1, coincident: [ 1, 1 ], child: { sequence: [ { filter: \"[n, m] -> { S0[i, j] }\" }, { filter: \"[n, m] -> { S1[i, j] }\" } ] } } }"

  @Test
  def testSkewing() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i, j] -> [i, 2*j + 4*i, 0]; S1[i, j] -> [i, j + 2*i, 1] }")
    println("starting to construct schedule tree")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(schedTree.toString, skewingExpectedResult)
  }

  @Test
  def testSkewing1() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i, j] -> [i, 4*j + 8*i, 0]; S1[i, j] -> [i, 2*j + 4*i, 1] }")
    println("starting to construct schedule tree")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(schedTree.toString, skewingExpectedResult)
  }

  @Test
  def testSkewing2() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i, j] -> [i, i + j, 0]; S1[i, j] -> [i, i + j, 1] }")
    val expectedResult : String = "{ domain: \"[n, m] -> { S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= m; S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= m }\", child: { schedule: \"[n, m] -> [{ S1[i0, i1] -> [(i0)]; S0[i0, i1] -> [(i0)] }, { S1[i0, i1] -> [(i0 + i1)]; S0[i0, i1] -> [(i0 + i1)] }]\", permutable: 1, coincident: [ 1, 1 ], child: { sequence: [ { filter: \"[n, m] -> { S0[i, j] }\" }, { filter: \"[n, m] -> { S1[i, j] }\" } ] } } }"
    println("starting to construct schedule tree")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(schedTree.toString, expectedResult)
  }
}