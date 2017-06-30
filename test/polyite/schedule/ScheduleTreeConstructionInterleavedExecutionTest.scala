package polyite.schedule

import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import org.junit.Assert.assertEquals
import org.junit.Test

class ScheduleTreeConstructionInterleavedExecutionTest extends AbstractScopTest {

  scopStr = """{ 
   "context" : "[n, m] -> {  : -2147483648 <= n <= 2147483647 and -2147483648 <= m <= 2147483647 }",
   "name" : "test",
   "statements" : [
      { 
         "accesses" : [
         ], 
         "domain" : "[n, m] -> { S0[i] : 1 <= i <= n }",
         "name" : "S0",
         "schedule" : "[n, m] -> { S0[i] -> [i, 0] }"
      },
      {
         "accesses" : [
         ],
         "domain" : "[n, m] -> { S1[i] : 1 <= i <= n }",
         "name" : "S1",
         "schedule" : "[n, m] -> { S1[i] -> [i, 1] }"
      }
   ]
}"""

  def runFurtherPreparation() {

  }

  val expectedResult0 : String = "{ domain: \"[n, m] -> { S1[i] : i >= 1 and i <= n; S0[i] : i >= 1 and i <= n }\", child: { schedule: \"[n, m] -> [{ S1[i0] -> [(i0)]; S0[i0] -> [(-2 + i0)] }]\", coincident: [ 1 ], child: { sequence: [ { filter: \"[n, m] -> { S0[i] }\" }, { filter: \"[n, m] -> { S1[i] }\" } ] } } }"

  @Test
  def test00() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val sched0 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i] -> [1*i - 1, 0]; S1[i] -> [1*i + 1, 1] }")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched0, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(schedTree.toString(), expectedResult0)
  }

  @Test
  def test01() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val sched1 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i] -> [2*i - 3, 0]; S1[i] -> [2*i + 1	, 1] }")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched1, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(schedTree.toString(), expectedResult0)
  }

  @Test
  def test02() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val sched2 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i] -> [i + 2, 0]; S1[i] -> [i + 4	, 1] }")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched2, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(schedTree.toString(), expectedResult0)
  }

  @Test
  def test03() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val sched3 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i] -> [i - 2, 0]; S1[i] -> [i, 1] }")
    val schedTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched3, domInfo, scop, deps, conf)
    println(schedTree)
    assertEquals(schedTree.toString(), expectedResult0)
  }

  val expectedResult1 : String = "{ domain: \"[n, m] -> { S1[i] : i >= 1 and i <= n; S0[i] : i >= 1 and i <= n }\", child: { schedule: \"[n, m] -> [{ S1[i0] -> [(i0)]; S0[i0] -> [(i0)] }]\", coincident: [ 1 ], child: { sequence: [ { filter: \"[n, m] -> { S0[i] }\" }, { filter: \"[n, m] -> { S1[i] }\" } ] } } }"

  @Test
  def test10() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val sched0 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i] -> [2*i]; S1[i] -> [2*i + 1] }")

    val sTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched0, domInfo, scop, deps, conf)

    println(sTree)
    assertEquals(sTree.toString(), expectedResult1)
  }

  @Test
  def test11() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val sched1 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i] -> [3*i]; S1[i] -> [3*i + 1] }")

    val sTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched1, domInfo, scop, deps, conf)

    println(sTree)
    assertEquals(sTree.toString(), expectedResult1)
  }

  @Test
  def test12() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val sched2 : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S0[i] -> [3*i]; S1[i] -> [3*i + 2] }")

    val sTree : isl.Schedule = ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched2, domInfo, scop, deps, conf)

    println(sTree)
    assertEquals(sTree.toString(), expectedResult1)
  }
}