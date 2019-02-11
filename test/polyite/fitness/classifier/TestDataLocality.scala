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

class TestDataLocality extends AbstractTest {
  
  @Test
  def test() {
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
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal :Double = DataLocality.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("feature value: " + fVal)
    assertEquals(3200.0 / 3520.0, fVal, 0)
  }
  
  @Test
  def testOne() {
    val scop : ScopInfo = new ScopInfo().setParams(isl.Set.readFromStr(Isl.ctx, "[n] -> { : 0 < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { S[i, j] : 0 <=i < n and 0 <= j < n }"))
      .addDomain(isl.Set.readFromStr(Isl.ctx, "[n] -> { T[i, j] : 0 <=i < n*2 and 0 <= j < 2*n }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i, j] -> [i, j] }"))
      .addSchedule(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i, j] -> [i, j] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { S[i,j] -> A[i, j - 1] }"))
      .addWrs(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i, j] }"))
      .addRds(isl.Map.readFromStr(Isl.ctx, "[n] -> { T[i,j] -> B[i, j - 1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val sched : ScheduleNode = SchedTreeUtil.markLoops(SchedTreeUtil.simplifySchedTree(ScheduleTreeConstruction.islUnionMap2BasicScheduleTree(scop.getSched, domInfo, scop, deps, false, true), deps))
    val scopMetrics : SCoPMetrics = SCoPMetrics.apply(deps.size, 1, 0, 1, 2)
    val fVal :Double = DataLocality.calc(sched, super.createTestConfig().get, scop, scopMetrics, domInfo, deps)
    println(sched)
    println("feature value (test one): " + fVal)
    assertEquals(1.0, fVal, 0)
  }
}