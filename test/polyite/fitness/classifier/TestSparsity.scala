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
import polyite.fitness.SparsityIterCoeffs
import polyite.fitness.SparsityParamCoeffs

class TestSparsity extends AbstractTest {
  
  @Test
  def test1() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j - 1] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal :Double = SparsityIterCoeffs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("test 1: " + fVal)
    assertEquals(0.5, fVal, 0)
  }
  
  @Test
  def test2() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, j, k] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j,k] -> A[i, j, k] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j,k] -> A[i - 1, j + 1, k + 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 3)
    val fVal :Double = SparsityIterCoeffs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("test 2: " + fVal)
    assertEquals(2.0 / 3.0, fVal, 0)
  }
  
  @Test
  def withSeq1() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, 0, j, k] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] -> [i, 1, j, k] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 3)
    val fVal :Double = SparsityIterCoeffs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("with seq node 1: " + fVal)
    assertEquals(2.0 / 3.0, fVal, 0)
  }
  
  @Test
  def withSeqSkewed() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, 0, j, k] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] -> [i + k, 1, j + i + k, k + j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i, j, k] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i, j + 1, i - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 3)
    val fVal :Double = SparsityIterCoeffs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("with seq, skewed: " + fVal)
    assertEquals(8 / 18.0, fVal, 0)
  }
  
  @Test
  def testParams() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i + n, 0, j, k] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] -> [i + k + 1, 1, j + i + k, k + j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i, j, k] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i + n + 1, j + 1, i - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 3)
    val fVal :Double = SparsityParamCoeffs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("test params: " + fVal)
    assertEquals(10 / 12.0, fVal, 0)
  }
  
  @Test
  def testParamsOne() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, 0, j, k] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j, k] -> [i + k, 0, j + i + k, k + j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i, j, k] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j,k] -> A[i, j + 1, i - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 3)
    val fVal :Double = SparsityParamCoeffs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("params one: " + fVal)
    assertEquals(1.0, fVal, 0)
  }
}