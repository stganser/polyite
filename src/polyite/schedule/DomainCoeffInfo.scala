package polyite.schedule

import java.util.Comparator
import isl.Isl.TypeAliases._
import isl.Conversions._
import isl.Isl

object DomainCoeffInfo {
  def apply(domain : isl.UnionSet) : DomainCoeffInfo = {

    val ctx : isl.Ctx = domain.getCtx()
    var count : Int = 0
    var i : Int = 0
    val nrStmts : Int = domain.nSet()
    val domainSets = new Array[isl.Set](nrStmts)
    domain.foreachSet { set : isl.Set =>
      domainSets(i) = set
      count += set.dim(T_SET)
      i += 1
    }
    java.util.Arrays.sort(domainSets, new Comparator[isl.Set] {
      def compare(x : isl.Set, y : isl.Set) : Int = x.getTupleName().compareTo(y.getTupleName())
    })

    var stmtInfo : Map[String, StmtCoeffInfo] = Map.empty

    val nrIt : Int = count
    count = 0
    val domainParDim : Int = domain.params().dim(T_PAR)
    for ((set, i) <- domainSets.view.zipWithIndex) {
      val stmtNrIt : Int = set.dim(T_SET)
      stmtInfo += (set.getTupleName() ->
        new StmtCoeffInfo(count, stmtNrIt,
          nrIt + domainParDim * i,
          nrIt + domainParDim * nrStmts + i))
      count += stmtNrIt
    }

    val dim : Int = nrIt + nrStmts * (domainParDim + 1)
    var universe = isl.Set.universe(isl.Space.setAlloc(ctx, 0, dim))
    return new DomainCoeffInfo(nrIt, domainParDim, stmtInfo, universe, domain)
  }
}

class DomainCoeffInfo private (val nrIt : Int, val nrParPS : Int, val stmtInfo : Map[String, StmtCoeffInfo],
    val universe : isl.Set, val domain : isl.UnionSet) {

  val ctx : isl.Ctx = universe.getCtx()
  val nrStmts : Int = stmtInfo.size
  val dim : Int = nrIt + nrParPS * nrStmts + nrStmts

  def scheduleParamSpace : isl.Space = domain.getSpace().params()

  def transferToCtx(ctx1 : isl.Ctx) : DomainCoeffInfo = {
    return new DomainCoeffInfo(
      nrIt,
      nrParPS,
      stmtInfo,
      isl.Set.readFromStr(ctx1, universe.toString()),
      isl.UnionSet.readFromStr(ctx1, domain.toString()))
  }
}

case class StmtCoeffInfo(val itStart : Int, val nrIt : Int, val parStart : Int, val cstIdx : Int)
