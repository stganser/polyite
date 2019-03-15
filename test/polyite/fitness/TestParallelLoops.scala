package polyite.fitness

import polyite.ScopInfo
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.ScheduleSpaceUtils
import isl.Isl
import org.junit.Assert._
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import org.junit.Test
import polyite.util.SCoPMetrics
import polyite.AbstractTest

class TestParallelLoops extends AbstractTest {

  @Test
  def testSeq() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j - 1] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal : Double = ParallelLoops.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("no parallelism: " + fVal)
    assertEquals(0.0, fVal, 0)
  }

  @Test
  def testMiddleParMustSplit() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] : 0 <=i <= n and 0 <= j <= n and 0 <= k <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j, k] -> [i, j, k] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j, k] -> A[0, j, 0] : i = 0 and k = 0 }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j, k] -> A[0, j, 0] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal : Double = ParallelLoops.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("middle par must split: " + fVal)
    assertEquals(2.0 / 3.0, fVal, 0)
  }

  @Test
  def testInnerPar() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i + j, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j - 1] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal : Double = ParallelLoops.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("inner parallel: " + fVal)
    assertEquals(0.5, fVal, 0)
  }

  @Test
  def testInnerParSkewed() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal : Double = ParallelLoops.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("inner parallel skewed: " + fVal)
    assertEquals(0.5, fVal, 0)
  }

  @Test
  def testOuterPar() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i - j, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal : Double = ParallelLoops.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("inner parallel: " + fVal)
    assertEquals(1, fVal, 0)
  }

  @Test
  def testWithSeqOneInnerPar() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, 0, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [i, 1, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i, j - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 2)
    val fVal : Double = ParallelLoops.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("with seq, one inner par: " + fVal)
    assertEquals(0.25, fVal, 0)
  }

  @Test
  def testWithSeqTwoInnerPar() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, 0, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j] }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [i, 1, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 2, 0, 1, 2)
    val fVal : Double = ParallelLoops.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("with seq, two inner par: " + fVal)
    assertEquals(0.25, fVal, 0)
  }
}