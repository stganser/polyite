package polyite.fitness.classifier

import polyite.AbstractTest
import org.junit.Test
import polyite.ScopInfo
import polyite.schedule.Dependence
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.util.SCoPMetrics
import polyite.fitness.ParallelLoops
import isl.Isl
import org.junit.Assert._
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.fitness.TilableBands

class TestTiling extends AbstractTest {
  
  @Test
  def testFullyTiableOneBand() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j - 1] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal :Double = TilableBands.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("fully tilable one band: " + fVal)
    assertEquals(1, fVal, 0)
  }
  
  @Test
  def notFullyTiable() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, j, k] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j,k] -> A[i, j, k] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j,k] -> A[i - 1, j + 1, k + 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 3)
    val fVal :Double = TilableBands.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("not fully tilable: " + fVal)
    assertEquals(2.0 / 3.0, fVal, 0)
  }
  
  @Test
  def notFullyTiableSeq() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, 0, j, k] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] -> [i, 1, j, k] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 3)
    val fVal :Double = TilableBands.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("not fully tilable seq: " + fVal)
    assertEquals(2.0 / 3.0, fVal, 0)
  }
  
  @Test
  def onlyOneStmtTilableSeq() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, 0, j, k] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] -> [i, 1, j, k] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i, j, k] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i, j + 1, i - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 3)
    val fVal :Double = TilableBands.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("only one statement tilable with seq: " + fVal)
    assertEquals(0.5, fVal, 0)
  }
}