package polyite.schedule

import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import org.junit.Assert.assertEquals
import org.junit.Test
import polyite.ScopInfo
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.util.SchedTreeUtil

class ScheduleTreeConstructionTest extends AbstractScopTest {

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
  def testSeq0() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [0, i, j]; S1[i, j] -> [1, i, j]; S2[i, j] -> [2, i, j] }")
    val expectedResult : String = "{ domain: \"[n] -> { S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S2[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { sequence: [ { filter: \"[n] -> { S0[i, j] }\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i0)] }]\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i1)] }]\" } } }, { filter: \"[n] -> { S1[i, j] }\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i0)] }, { S1[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } }, { filter: \"[n] -> { S2[i, j] }\", child: { schedule: \"[n] -> [{ S2[i0, i1] -> [(i0)] }, { S2[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } } ] } }"
    assertEquals(ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf).toString, expectedResult)
  }

  @Test
  def testSeq1() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [0, i, j, 0]; S1[i, j] -> [0, i, j, 1]; S2[i, j] -> [1, i, j, 0] }")
    val expectedResult : String = "{ domain: \"[n] -> { S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S2[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { sequence: [ { filter: \"[n] -> { S1[i, j]; S0[i, j] }\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i0)]; S0[i0, i1] -> [(i0)] }]\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i1)]; S0[i0, i1] -> [(i1)] }]\", child: { sequence: [ { filter: \"[n] -> { S0[i, j] }\" }, { filter: \"[n] -> { S1[i, j] }\" } ] } } } }, { filter: \"[n] -> { S2[i, j] }\", child: { schedule: \"[n] -> [{ S2[i0, i1] -> [(i0)] }, { S2[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } } ] } }"
    assertEquals(ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf).toString, expectedResult)
  }

  @Test
  def testSeq2() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [0, i, j, i]; S1[i, j] -> [0, i, j, 2*i]; S2[i, j] -> [1, i, j, 0] }")
    val expectedResult : String = "{ domain: \"[n] -> { S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S2[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { sequence: [ { filter: \"[n] -> { S1[i, j]; S0[i, j] }\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i0)]; S0[i0, i1] -> [(i0)] }]\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i1)]; S0[i0, i1] -> [(i1)] }]\", child: { sequence: [ { filter: \"[n] -> { S0[i, j] }\" }, { filter: \"[n] -> { S1[i, j] }\" } ] } } } }, { filter: \"[n] -> { S2[i, j] }\", child: { schedule: \"[n] -> [{ S2[i0, i1] -> [(i0)] }, { S2[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } } ] } }"
    //    val astBuilder : isl.AstBuild = isl.AstBuild.fromContext(scop.getDomain.params())
    //    val p : isl.Printer = isl.Printer.toStr(ctx)
    //    val ast : isl.AstNode = astBuilder.astFromSchedule(schedMap.intersectDomain(scop.getDomain))
    //    println(ast.print(p, isl.AstPrintOptions.alloc(ctx)).getStr())
    assertEquals(ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf).toString, expectedResult)
  }

  @Test
  def testElimDims0() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [0, i, j, 0, i]; S1[i, j] -> [0, i, j, 1, 2*j]; S2[i, j] -> [1, i, j, 0, 0] }")
    val expectedResult : String = "{ domain: \"[n] -> { S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S2[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { sequence: [ { filter: \"[n] -> { S1[i, j]; S0[i, j] }\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i0)]; S0[i0, i1] -> [(i0)] }]\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i1)]; S0[i0, i1] -> [(i1)] }]\", child: { sequence: [ { filter: \"[n] -> { S0[i, j] }\" }, { filter: \"[n] -> { S1[i, j] }\" } ] } } } }, { filter: \"[n] -> { S2[i, j] }\", child: { schedule: \"[n] -> [{ S2[i0, i1] -> [(i0)] }, { S2[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } } ] } }"
    assertEquals(ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf).toString, expectedResult)
  }

  @Test
  def testElimDims1() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [0, i, j, i + j]; S1[i, j] -> [1, i, j, 2*i + 3 + i + j]; S2[i, j] -> [2, i, j, 0] }")
    val expectedResult : String = "{ domain: \"[n] -> { S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S2[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { sequence: [ { filter: \"[n] -> { S0[i, j] }\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i0)] }]\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i1)] }]\" } } }, { filter: \"[n] -> { S1[i, j] }\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i0)] }, { S1[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } }, { filter: \"[n] -> { S2[i, j] }\", child: { schedule: \"[n] -> [{ S2[i0, i1] -> [(i0)] }, { S2[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } } ] } }"
    assertEquals(ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf).toString, expectedResult)
  }

  @Test
  def testRemoveOffset0() {
    val ctx : isl.Ctx = isl.Isl.ctx
    val schedMap : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n] -> { S0[i, j] -> [0 + 3, i + n, j + 2*n + 5, 0]; S1[i, j] -> [0 + 3, i, j, 1]; S2[i, j] -> [1 + 3, i, j, 0] }")
    val expectedResult : String = "{ domain: \"[n] -> { S1[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S2[i, j] : i >= 1 and i <= n and j >= 1 and j <= n; S0[i, j] : i >= 1 and i <= n and j >= 1 and j <= n }\", child: { sequence: [ { filter: \"[n] -> { S1[i, j] }\", child: { schedule: \"[n] -> [{ S1[i0, i1] -> [(i0)] }, { S1[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } }, { filter: \"[n] -> { S0[i, j] }\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i0)] }]\", child: { schedule: \"[n] -> [{ S0[i0, i1] -> [(i1)] }]\" } } }, { filter: \"[n] -> { S2[i, j] }\", child: { schedule: \"[n] -> [{ S2[i0, i1] -> [(i0)] }, { S2[i0, i1] -> [(i1)] }]\", permutable: 1, coincident: [ 1, 1 ] } } ] } }"
    assertEquals(ScheduleTreeConstruction.islUnionMap2IslScheduleTree(schedMap, domInfo, scop, deps, conf).toString, expectedResult)
  }

  @Test
  def testEqParams() {
    val ctx : isl.Ctx = isl.Isl.ctx
    var scop1 : ScopInfo = new ScopInfo()
    scop1 = scop1.setParams(isl.UnionSet.readFromStr(ctx, "[n, m] -> { : n > 0 and n = m }"))
    scop1 = scop1.addDomain(isl.Set.readFromStr(ctx, "[n,m] -> { S[i] : 0 < i < n }"))
    scop1 = scop1.addDomain(isl.Set.readFromStr(ctx, "[n,m] -> { T[i] : 0 < i < n }"))
    scop1 = scop1.addSchedule(isl.Map.readFromStr(ctx, "[n, m] -> { S[i] -> [i] }"))
    scop1 = scop1.addSchedule(isl.Map.readFromStr(ctx, "[n, m] -> { T[i] -> [i] }"))
    val (deps1 : Set[Dependence], domInfo1 : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop1)
    val sched : isl.UnionMap = isl.UnionMap.readFromStr(ctx, "[n, m] -> { S[i] -> [i + n]; T[i] -> [i + m] }")
    val schedTree : ScheduleNode = ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(sched, domInfo1, scop1, deps1, false, true)
    val schedTreeSimpl : ScheduleNode = SchedTreeUtil.simplifySchedTree(schedTree, deps1)
    val islSched : isl.Schedule = SchedTreeUtil.scheduleTree2IslScheduleTree(schedTreeSimpl)
    var expectedResult : String = "{ domain: \"[n, m] -> { T[i] : i >= 1 and i <= -1 + n; S[i] : i >= 1 and i <= -1 + n }\", child: { schedule: \"[n, m] -> [{ T[i0] -> [(i0)]; S[i0] -> [(i0)] }]\", coincident: [ 1 ] } }"
    assertEquals(islSched.toString(), expectedResult)
  }
}