package polyite.schedule

import java.io.File
import polyite.ScopInfo
import polyite.MainUtil
import polyite.config.Config
import polyite.schedule.schedule_tree.ScheduleTreeConstruction
import isl.Isl

object Test3mm {

  def main(args : Array[String]) : Unit = {
    val ctx : isl.Ctx = Isl.ctx
    val scopFilePath : String = "/home/stg/workspace/schedule-optimization/polybench-c-4.1/3mm/kernel_3mm___%entry.split---%for.end112.jscop"
    //    val scopFilePath : String = "/home/stg/workspace/schedule-optimization/polybench-c-4.1/gemm/kernel_gemm___%entry.split---%for.end40.jscop"
    val confFile : File = new File("/home/stg/workspace/schedule-optimization/polybench-c-4.1/3mm/config_ga_3mm_kernel_3mm_%entry.split---%for.end112.properties")
    //    val confFile : File = new File("/home/stg/workspace/schedule-optimization/polybench-c-4.1/gemm/config_ga_gemm_kernel_gemm_%entry.split---%for.end40.properties")
    val (scopFile : File, scop : ScopInfo) = MainUtil.loadScop(scopFilePath) match {
      case None    => return
      case Some(s) => s
    }
    val conf : Config = MainUtil.loadConfig(confFile.getAbsolutePath, Config.loadAndValidateConfig) match {
      case None    => return
      case Some(c) => c
    }
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    //    val scheds : Set[Schedule] = ScheduleUtils.genRandSchedules(domInfo, deps, 5, conf.maxNumRays, conf.maxNumLines, conf)

    val scheds : List[isl.UnionMap] = List("[ni, nj, nk, nl, nm] -> { Stmt_for_body89[i0, i1, i2] -> [3 + 2nm, 1 + 3nj - nl + i2, 2nj, 0, i1, nk + i0]; Stmt_for_body16[i0, i1] -> [3 + 2nm, -1 + 3nj - nl + i1, 0, 0, -3i0, i0]; Stmt_for_body21[i0, i1, i2] -> [3 + 2nm, 3nj - nl + i1, 0, 4i2, -4i0, i0]; Stmt_for_body53[i0, i1, i2] -> [3 + 2nm, 3nj - nl + i0, 0, -nm + 3i2, -i1, i0]; Stmt_for_body46[i0, i1] -> [2 + 2nm - i1, -1 + 3nj - nl + i0, 0, 0, i0, i0]; Stmt_for_body82[i0, i1] -> [3 + 2nm, 3 - nl, 0, 0, i0, i1] }",
      "[ni, nj, nk, nl, nm] -> { Stmt_for_body46[i0, i1] -> [-1 + nk - 3i0, 0, 0, 3ni, 0, nm + 2i0, 0, 0, 0, 0, 0, -i1]; Stmt_for_body89[i0, i1, i2] -> [nk, 0, i0, 1 + i2, -3nm, 0, 0, nk + 4nm, 1 - nm, -2, nk - 3i0 + 3i2, -i1]; Stmt_for_body16[i0, i1] -> [2 - 2nj + nk + 2i1, 4 - 4nj + 4i1, i0, 1 + i1, -3nm - i0, -3i0, -2i0, nk + 4nm, -nm, 2ni, 0, i0]; Stmt_for_body53[i0, i1, i2] -> [nk, 0, 1 - nm + i2, 2 - nm + i0 + i2, -3 - 3i2, 0, -3i2, nk + 3nm - i2, -4i2, 2 - 4nl + 4i1, 7 - 3ni - 4nj + nk + 7i0, i0]; Stmt_for_body21[i0, i1, i2] -> [nk, 0, i0, 1 + i1, -3nm, -3i0, 0, nk + 4nm, 1 - nm, -2nk, -3i0 + 3i1 + i2, i0]; Stmt_for_body82[i0, i1] -> [nk, 0, i0, 0, -3, 0, 0, 0, 0, -3nm, 0, -3nk + i1] }",
      "[ni, nj, nk, nl, nm] -> { Stmt_for_body89[i0, i1, i2] -> [-3 - 3ni, 0, 2ni - i0, nj, nk, 4nk, -1 + 4i1, ni - nk, -3nj, 1 + i2, i0]; Stmt_for_body46[i0, i1] -> [-3 - 3ni - 2i1, 0, 2, -2 + nj, 0, 0, 0, 0, 0, 0, -i0]; Stmt_for_body21[i0, i1, i2] -> [-6ni + 3i0, 0, 2ni - i0, nj, 1 + i2, 4 + 4i2, -nk - 4i0 + i2, i0 - 2i1 - i2, -3nj - 4i0, -i0 - 2i1, i0]; Stmt_for_body16[i0, i1] -> [-6ni + 3i0, 0, 2ni - i0, -1 + nj - 4i0, 0, 0, 0, 0, 0, 0, -i1]; Stmt_for_body82[i0, i1] -> [-3 - 3ni, -1, 3nl + 2i0, -3nl, nl, 0, 0, 0, -3ni, -2ni + 3nk, -i1]; Stmt_for_body53[i0, i1, i2] -> [-3 - 3ni - 2i1, 0, 2, -1 + nj, 0, 0, 0, 0, 0, i2, -i0] }",
      "[ni, nj, nk, nl, nm] -> { Stmt_for_body21[i0, i1, i2] -> [-1 - 2nj - i1, 1 - nk + i1 + i2, 0, -5i0, i0]; Stmt_for_body53[i0, i1, i2] -> [-2nj, i0, 4i2, i1, i0]; Stmt_for_body89[i0, i1, i2] -> [-2nj, 1 + i2, 0, -3i0, -3i1]; Stmt_for_body82[i0, i1] -> [-2nj, 0, 0, i0, 4i1]; Stmt_for_body16[i0, i1] -> [-1 - 2nj - i1, -nk + i1, 0, i0, i0]; Stmt_for_body46[i0, i1] -> [-2nj - 4i1, -1 + i0, 0, i0, i0] }",
      "[ni, nj, nk, nl, nm] -> { Stmt_for_body53[i0, i1, i2] -> [nk - 2nl - 4i1, 2nm + i0, 0, -2nk, 0, 0, 0, i2, i0]; Stmt_for_body21[i0, i1, i2] -> [nk - 2nl - 3i0, 2nm + i1, 2i2, i2, -1 - i1, -2 + 2i1, 3 + i1, -3 + 4ni - 4i0, i0]; Stmt_for_body16[i0, i1] -> [nk - 2nl - 3i0, 2nm + i1, 0, 2 - 2ni + 2i0, -nj, -2nj, 0, 0, i0]; Stmt_for_body89[i0, i1, i2] -> [nk - 2nl, 1 + 2nm + i2, 0, 0, 0, 2i1, 0, 0, -5i0]; Stmt_for_body82[i0, i1] -> [nk - 2nl, 2nm, 3i1, 0, 0, 0, 0, nm, -nj - 3nm + 4i0]; Stmt_for_body46[i0, i1] -> [nk - 2nl - 4i1, -1 + 2nm + i0, -3nj, 0, 0, 3ni, 0, 0, i0] }").map(isl.UnionMap.readFromStr(ctx, _))

    for (sched <- scheds) {
      println("Generating schedule tree for " + sched)
      val start : Long = System.currentTimeMillis()
      println(ScheduleTreeConstruction.islUnionMap2IslScheduleTree(sched, domInfo, scop, deps, conf))
      println("duration: " + (System.currentTimeMillis() - start).toDouble / 1000)
    }
  }
}