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
import polyite.fitness.DataLocality
import polyite.fitness.TreeDepth
import polyite.fitness.NumSeqAndSetNodes
import polyite.fitness.NumLeafs

class TestNLeaf extends AbstractTest {
  
  @Test
  def testOneBandNode() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i < n*2 and 0 <= j < 2*n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i - 1, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i, j - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 3, 0, 1, 2)
    val fVal :Double = NumLeafs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("one band node: " + fVal)
    assertEquals(1.0 / 3.0, fVal, 0)
  }
  
  @Test
  def testOneSeq() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { R[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { R[i, j] -> [i, 0, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, 0, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [i, 1, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 3, 0, 1, 2)
    val fVal :Double = NumLeafs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("one seq: " + fVal)
    assertEquals(2.0 / 3.0, fVal, 0)
  }
  
  @Test
  def testOneSeq1() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { R[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { R[i, j] -> [i, 2, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, 0, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [i, 1, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 3, 0, 1, 2)
    val fVal :Double = NumLeafs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("one seq 1: " + fVal)
    assertEquals(1.0, fVal, 0)
  }
  
  @Test
  def testTwoSeq() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { R[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { R[i, j] -> [i, 1, j, 0] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, 0, j, 1] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [i, 0, j, 2] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 3, 0, 1, 2)
    val fVal :Double = NumLeafs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("two seq: " + fVal)
    assertEquals(1.0, fVal, 0)
  }
  
  @Test
  def testThreeLeave() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { R[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { R[i, j] -> [3*i + 1, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [3*i + 2, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [3*i + 3, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 3, 0, 1, 2)
    val fVal :Double = NumLeafs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("three leaves: " + fVal)
    assertEquals(1.0, fVal, 0)
  }
  
  @Test
  def testThreeLeave1() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { R[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { R[i, j] -> [3*i + 1 + 3*j, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [3*i + 2 + 3*j, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [3*i + 3 + 3*j, j] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    println(SchedTreeUtil.scheduleTree2IslScheduleTree(sched))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 3, 0, 1, 2)
    val fVal :Double = NumLeafs.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("three leaves 1: " + fVal)
    assertEquals(1.0, fVal, 0)
  }
}