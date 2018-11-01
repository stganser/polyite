package polyite.schedule

import polyite.ScopInfo
import isl.Isl

object ThesisSearchSpaceRegion {
  
  def main(args: Array[String]): Unit = {
    val ctx : isl.Ctx = Isl.ctx
    val scop : ScopInfo = (new ScopInfo)
      .setParams(isl.Set.readFromStr(ctx, "[n, m] -> { : n > 0 and m > 0 }"))
      .addDomain(isl.Set.readFromStr(ctx, "[n, m] -> { R[i, j] : 0 <= i < n and 0 <= i <= j }"))
      .addDomain(isl.Set.readFromStr(ctx, "[n, m] -> { S[i, j, k] : 0 <= i < n and 0 <= j < m and 0 <= k <= i }"))
      .addRds(isl.Map.readFromStr(ctx, "[n, m] -> { R[i, j] -> C[i, j] }"))
      .addRds(isl.Map.readFromStr(ctx, "[n, m] -> { R[i, j] -> beta[] }"))
      .addWrs(isl.Map.readFromStr(ctx, "[n, m] -> { R[i, j] -> C[i, j] }"))
      .addRds(isl.Map.readFromStr(ctx, "[n, m] -> { S[i, j, k] -> C[i, k] }"))
      .addWrs(isl.Map.readFromStr(ctx, "[n, m] -> { S[i, j, k] -> C[i, k] }"))
      .addRds(isl.Map.readFromStr(ctx, "[n, m] -> { S[i, j, k] -> alpha[] }"))
      .addRds(isl.Map.readFromStr(ctx, "[n, m] -> { S[i, j, k] -> A[i, j] }"))
      .addRds(isl.Map.readFromStr(ctx, "[n, m] -> { S[i, j, k] -> A[i, k] }"))
      .addSchedule(isl.Map.readFromStr(ctx, "[n, m] -> { R[i, j] -> [i, 0, j, 0] }"))
      .addSchedule(isl.Map.readFromStr(ctx, "[n, m] -> { S[i, j, k] -> [i, 1, j, k] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    val depsList : List[Dependence] = deps.toList.sortBy(_.toString())
    println(depsList.mkString(", "))
    
    println(f"R: iteration coeffs: ${domInfo.stmtInfo("R").itStart} to ${domInfo.stmtInfo("R").itStart + domInfo.stmtInfo("R").nrIt - 1}")
    println(f"R: par coeffs: ${domInfo.stmtInfo("R").parStart} to ${domInfo.stmtInfo("R").parStart + domInfo.nrParPS - 1}")
    println(f"R: constant: ${domInfo.stmtInfo("R").cstIdx}")
    
    println(f"S: iteration coeffs: ${domInfo.stmtInfo("S").itStart} to ${domInfo.stmtInfo("S").itStart + domInfo.stmtInfo("S").nrIt - 1}")
    println(f"S: par coeffs: ${domInfo.stmtInfo("S").parStart} to ${domInfo.stmtInfo("S").parStart + domInfo.nrParPS - 1}")
    println(f"S: constant: ${domInfo.stmtInfo("S").cstIdx}")
    
    println(depsList(0).weakConstr)
    println(depsList(0).strongConstr)
    println(depsList(1).weakConstr)
    println(depsList(1).strongConstr)
    
    println(Isl.simplify(depsList(0).weakConstr.intersect(depsList(1).weakConstr)))
    println(Isl.simplify(depsList(0).weakConstr.intersect(depsList(1).weakConstr)))
    println(Isl.simplify(depsList(0).strongConstr.intersect(depsList(1).weakConstr)))
    println(Isl.simplify(depsList(1).strongConstr))
    
    println(Isl.simplify(depsList(1).strongConstr.intersect(depsList(0).weakConstr)))
  }
}