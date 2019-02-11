package polyite.fitness

import polyite.ScopInfo
import isl.Isl
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Dependence
import polyite.schedule.ScheduleSpaceUtils
import org.junit.Assert._
import org.junit.Test
import polyite.schedule.schedule_tree.util.SchedTreeUtil

class TestMemAccPattern {

  @Test
  def testAll() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val fVal :Double = MemAccessPattern.calc(sched, null, scop, null, domInfo, deps)
    println("all: " + fVal)
    assertEquals(1.0, fVal, 0)
    val sched1 : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [j, i] }"), domInfo, scop, deps, false, true), deps))
    val fVal1 :Double = MemAccessPattern.calc(sched1, null, scop, null, domInfo, deps)
    println("all interchanged: " + fVal1)
    assertEquals(0.0, fVal1, 0)
  }
  
  @Test
  def testSome() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[j, i] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val fVal :Double = MemAccessPattern.calc(sched, null, scop, null, domInfo, deps)
    println("some: " + fVal)
    assertEquals(0.5, fVal, 0)
    val sched1 : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [j, i] }"), domInfo, scop, deps, false, true), deps))
    val fVal1 :Double = MemAccessPattern.calc(sched1, null, scop, null, domInfo, deps)
    println("some interchanged: " + fVal1)
    assertEquals(0.5, fVal1, 0)
  }
  
  @Test
  def testNone() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[j, i] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[j, i] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val fVal :Double = MemAccessPattern.calc(sched, null, scop, null, domInfo, deps)
    println("none: " + fVal)
    assertEquals(0.0, fVal, 0)
    val sched1 : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [j, i] }"), domInfo, scop, deps, false, true), deps))
    val fVal1 :Double = MemAccessPattern.calc(sched1, null, scop, null, domInfo, deps)
    println("none interchanged: " + fVal1)
    assertEquals(1.0, fVal1, 0)
  }
  
  @Test
  def testSkewedSome() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j + i] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[j, i] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j + n] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val fVal :Double = MemAccessPattern.calc(sched, null, scop, null, domInfo, deps)
    println("skewed some 1: " + fVal)
    assertEquals(0.5, fVal, 0)
  }
  
  @Test
  def testSkewedSome1() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[j, i + j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val fVal :Double = MemAccessPattern.calc(sched, null, scop, null, domInfo, deps)
    println("skewed some 2: " + fVal)
    assertEquals(0.5, fVal, 0)
  }
  
  @Test
  def testSkewedNone() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i <= n and 0 <= j <= n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[j, i + j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i + j, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val fVal :Double = MemAccessPattern.calc(sched, null, scop, null, domInfo, deps)
    println("skewed none: " + fVal)
    assertEquals(0.0, fVal, 0)
  }
}