package polyite.schedule

import isl.Isl
import polyite.ScopInfo

object ThesisSearchSpaceRegionMini {
  def main(args: Array[String]): Unit = {
    val ctx : isl.Ctx = Isl.ctx
    val scop : ScopInfo = (new ScopInfo()).setParams(isl.Set.readFromStr(ctx, "[] -> {:}"))
        .addDomain(isl.Set.readFromStr(ctx, "[] -> { S[i,j] : 0 <= i < 4 and 0 <= j < 4 }"))
        .addSchedule(isl.Map.readFromStr(ctx, "[] -> { S[i, j] -> [i,j] }"))
        .addRds(isl.Map.readFromStr(ctx, "[] -> { S[i,j] -> A[] }"))
        .addWrs(isl.Map.readFromStr(ctx, "[] -> { S[i,j] -> A[] }"))
        .addWrs(isl.Map.readFromStr(ctx, "[] -> { S[i,j] -> B[i,j] }"))
        .addWrs(isl.Map.readFromStr(ctx, "[] -> { S[i,j] -> B[i-1,j-1] }"))
    val (deps : Set[Dependence], domInfo : DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    println(deps.mkString("\n"))
    println(deps.foldLeft(domInfo.universe)(((s : isl.Set, dep : Dependence) => s.intersect(dep.strongConstr))))
  }
}