package polyite.schedule.schedule_tree.normalization

import scala.collection.mutable.HashSet
import scala.math.BigInt.int2bigInt

import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.ScheduleUtils
import polyite.schedule.ScheduleVectorUtils
import polyite.util.Rat
import polyite.util.Util

import isl.Conversions.convertLambdaToVoidCallback1
import isl.Isl
import isl.Isl.TypeAliases.T_SET

class RebuildDimSchedsVisitor extends PartialSchedMapNormalizationVisitor {

  override def toString() : String = "rebuild partial 1-d schedules."

  override def simplifySchedMap(s : isl.UnionMap, domain : isl.UnionSet) : Option[isl.UnionMap] = {
    val limitedDomInfo : DomainCoeffInfo = DomainCoeffInfo(domain)
    //    val artDeps : Iterable[Dependence] = constructArtificialDeps(s, domainLimited, limitedDomInfo)
    val happensBeforeRelations : Iterable[isl.BasicMap] = constructHappensBeforeRelations(domain, s)
    val sameExecTimeRelations : Iterable[isl.BasicMap] = constructSameExecTimeRelations(domain, s)

    var coeffSpace : isl.Set = calculateCoeffSpace(happensBeforeRelations, sameExecTimeRelations, limitedDomInfo)
    assert(!coeffSpace.isEmpty())
    val (coeffSpaceTmp, nBoundDims : Int) = addCoeffMinConstraintsFast(coeffSpace)
    coeffSpace = coeffSpaceTmp
    //    val (coeffSpaceWithObjF, nObjF) = addMinObjFunctions(coeffSpace, limitedDomInfo)
    //    coeffSpace = addMinObjFunction(coeffSpace)
    val lexMin : isl.Set = Isl.simplify(coeffSpace).lexmin()
    //    assert(lexMin.isSingleton())
    val normCoeffVectPoint : isl.Point = lexMin.samplePoint()
    val normCoeffVect : List[Rat] = Util.islPoint2RatList(normCoeffVectPoint)

    if (ScheduleVectorUtils.checkAllZero(normCoeffVect))
      return None
    val newSched : isl.UnionMap = ScheduleUtils.coeffMatrix2IslUnionMap(limitedDomInfo, normCoeffVect.drop(nBoundDims).map(_.intFloor))
    return Some(newSched)
  }

  private def calculateCoeffSpace(depsOrder : Iterable[isl.BasicMap], depsPar : Iterable[isl.BasicMap], domInfo : DomainCoeffInfo) : isl.Set = {
    var coeffSpace : isl.Set = domInfo.universe
    for (dep <- depsOrder)
      coeffSpace = coeffSpace.intersect(ScheduleSpaceUtils.compSchedConstrForDep(dep, domInfo, true))
    for (dep <- depsPar)
      coeffSpace = coeffSpace.intersect(ScheduleSpaceUtils.compParSchedConstrForDep(dep, domInfo))
    return coeffSpace
  }

  //  private def constructArtificialDeps(sched : isl.UnionMap, domain : isl.UnionSet, domInfo : DomainCoeffInfo) : Iterable[Dependence] = {
  //    val ctx : isl.Ctx = sched.getCtx
  //    val artificialArray : isl.BasicSet = isl.BasicSet.readFromStr(ctx, "{ [0] }")
  //    val empty : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)
  //    var artificialAccesses : isl.UnionMap = empty
  //    domain.foreachSet((s0 : isl.Set) => {
  //      val access : isl.Map = isl.Map.fromDomainAndRange(s0, artificialArray)
  //      artificialAccesses = artificialAccesses.addMap(access)
  //    })
  //    artificialAccesses = artificialAccesses.intersectDomain(domain)
  //    val depsA : Array[isl.UnionMap] = new Array(1)
  //    val depsA1 : Array[isl.UnionMap] = new Array(1)
  //    artificialAccesses = Isl.simplify(artificialAccesses)
  //    artificialAccesses.computeFlow(artificialAccesses, artificialAccesses, sched.intersectDomain(domain), depsA, depsA1, null, null)
  //    val deps : isl.UnionMap = depsA(0).union(depsA1(0))
  //    val depsSeparated : Iterable[isl.BasicMap] = sepDepsFast(deps)
  //
  //    val depsSep : Iterable[Dependence] = depsSeparated map {
  //      (m : isl.BasicMap) =>
  //        {
  //          val wrs : isl.BasicSet = ScheduleSpaceUtils.compSchedConstrForDep(m, domInfo, false)
  //          val srs : isl.BasicSet = ScheduleSpaceUtils.compSchedConstrForDep(m, domInfo, true)
  //          new Dependence(m, wrs, srs)
  //        }
  //    }
  //    return depsSep
  //  }

  private def sepDepsFast(deps : isl.UnionMap) : Set[isl.BasicMap] = {
    var result : HashSet[isl.BasicMap] = HashSet.empty
    deps.foreachMap((m : isl.Map) => {
      m.foreachBasicMap((bm : isl.BasicMap) => result.add(bm))
    })
    return result.toSet
  }

  private def constructSameExecTimeRelations(domain : isl.UnionSet, sched : isl.UnionMap) : Iterable[isl.BasicMap] = {
    val uMap : isl.UnionMap = constructSameExecTimeMap(domain, sched)
    return sepDepsFast(uMap)
  }

  private def constructSameExecTimeMap(domain : isl.UnionSet, sched : isl.UnionMap) : isl.UnionMap = {
    val stmts : Set[String] = Isl.islUnionSetGetTupleNames(domain)
    var result : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)
    for (s1 : String <- stmts) {
      for (s2 : String <- stmts) {
        if (s1 <= s2) {
          val sched1 : isl.Map = isl.Map.fromUnionMap(Isl.islUnionMapFilter(sched, Set(s1)))
          val sched2 : isl.Map = isl.Map.fromUnionMap(Isl.islUnionMapFilter(sched, Set(s2)))
          val dom1 : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(domain, Set(s1)))
          val dom2 : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(domain, Set(s2)))
          val m : isl.Map = constructSameExecTimeMap(dom1, dom2, sched1, sched2)
          result = result.addMap(m)
        }
      }
    }
    return result
  }

  private def constructSameExecTimeMap(dom1 : isl.Set, dom2 : isl.Set, sched1 : isl.Map, sched2 : isl.Map) : isl.Map = {
    val result : isl.Map = isl.Map.fromDomainAndRange(dom1, dom2)
    return result.addConstraint(Isl.buildEqConstrFromMaps(dom1, dom2, sched1, sched2))
  }

  private def constructHappensBeforeRelations(domain : isl.UnionSet, sched : isl.UnionMap) : Iterable[isl.BasicMap] = {
    val uMap : isl.UnionMap = constructHappensBeforeMap(domain, sched)
    return sepDepsFast(uMap)
  }

  private def constructHappensBeforeMap(domain : isl.UnionSet, sched : isl.UnionMap) : isl.UnionMap = {
    val stmts : Set[String] = Isl.islUnionSetGetTupleNames(domain)
    var result : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)
    for (s1 : String <- stmts) {
      for (s2 : String <- stmts) {
        val sched1 : isl.Map = isl.Map.fromUnionMap(Isl.islUnionMapFilter(sched, Set(s1)))
        val sched2 : isl.Map = isl.Map.fromUnionMap(Isl.islUnionMapFilter(sched, Set(s2)))
        val dom1 : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(domain, Set(s1)))
        val dom2 : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(domain, Set(s2)))
        val m : isl.Map = constructHappensBeforeMap(dom1, dom2, sched1, sched2)
        result = result.addMap(m)
      }
    }
    return result
  }

  private def constructHappensBeforeMap(dom1 : isl.Set, dom2 : isl.Set, sched1 : isl.Map, sched2 : isl.Map) : isl.Map = {
    val result : isl.Map = isl.Map.fromDomainAndRange(dom1, dom2)
    return result.addConstraint(Isl.buildHappensBeforeConstrFromMaps(dom1, dom2, sched1, sched2))
  }

  // this is derived from Pluto+ -> Bondhugula2016
  // we do not group by statement but arbitrarily according to a fixed small group size. 
  private def addCoeffMinConstraintsFast(s : isl.Set) : (isl.Set, Int) = {
    val groupSize : Int = 5
    val nGroups : Int = Rat(s.dim(T_SET), groupSize).intCeil.toInt
    val nBoundDims : Int = nGroups + 1

    var res : isl.Set = s.insertDims(T_SET, 0, nBoundDims)
    val lsp : isl.LocalSpace = isl.LocalSpace.fromSpace(res.getSpace)
    val baseIneq : isl.Constraint = isl.Constraint.allocInequality(lsp)

    for (group : Int <- 0 until nGroups) {
      val start : Int = group * groupSize
      val end : Int = math.min(start + groupSize - 1, s.dim(T_SET) - 1)
      val currGroupSize : Int = end - start + 1
      val signCombs : Iterator[Array[Int]] = constructCombinations(Array.fill(currGroupSize)(2))
      for (signComb : Array[Int] <- signCombs) {
        var constr : isl.Constraint = baseIneq.setCoefficientSi(T_SET, group + 1, 1)
        for ((sign, pos) <- signComb.map(produceSign).zipWithIndex) {
          constr = constr.setCoefficientSi(T_SET, nBoundDims + start + pos, sign)
        }
        res = res.addConstraint(constr)
      }
    }
    System.gc()
    var constr : isl.Constraint = baseIneq.setCoefficientSi(T_SET, 0, 1)
    for (i <- 1 until nBoundDims)
      constr.setCoefficientSi(T_SET, i, -1)
    res = res.addConstraint(constr)

    constr = baseIneq.setCoefficientSi(T_SET, 0, 1)
    res = res.addConstraint(constr)

    return (res, nBoundDims)
  }

  //  // this is derived from Pluto+ -> Bondhugula2016
  //  private def addCoeffMinCostraints(s : isl.Set, domInfo : DomainCoeffInfo) : (isl.Set, Int) = {
  //    val nBoundDims : Int = domInfo.stmtInfo.size + 1
  //    var res : isl.Set = s.insertDims(T_SET, 0, nBoundDims)
  //
  //    val lsp : isl.LocalSpace = isl.LocalSpace.fromSpace(res.getSpace)
  //    val baseIneq : isl.Constraint = isl.Constraint.allocInequality(lsp)
  //
  //    for ((sInfo : StmtCoeffInfo, stmtIdx : Int) <- domInfo.stmtInfo.map(_._2).toList.zipWithIndex) {
  //      val nStmtCoeffs : Int = 1 + sInfo.nrIt + domInfo.nrParPS
  //      val signCombinations : Iterator[Array[Int]] = constructCombinations(Array.fill(nStmtCoeffs)(2))
  //      for (signComb : Array[Int] <- signCombinations) {
  //        var signIdx = 0
  //        var constr : isl.Constraint = baseIneq.setCoefficientSi(T_SET, stmtIdx + 1, 1)
  //        constr = constr.setCoefficientSi(T_SET, sInfo.cstIdx + nBoundDims, produceSign(signComb(signIdx)))
  //        signIdx += 1
  //        for (i <- sInfo.itStart until sInfo.itStart + sInfo.nrIt) {
  //          constr = constr.setCoefficientSi(T_SET, i + nBoundDims, produceSign(signComb(signIdx)))
  //          signIdx += 1
  //        }
  //        for (i <- sInfo.parStart until sInfo.parStart + domInfo.nrParPS) {
  //          constr = constr.setCoefficientSi(T_SET, i + nBoundDims, produceSign(signComb(signIdx)))
  //          signIdx += 1
  //        }
  //        res = res.addConstraint(constr)
  //      }
  //    }
  //    System.gc()
  //
  //    var constr : isl.Constraint = baseIneq.setCoefficientSi(T_SET, 0, 1)
  //    for (i <- 1 until nBoundDims) {
  //      constr.setCoefficientSi(T_SET, i, -1)
  //    }
  //    res = res.addConstraint(constr)
  //
  //    constr = baseIneq.setCoefficientSi(T_SET, 0, 1)
  //    res = res.addConstraint(constr)
  //
  //    return (res, nBoundDims)
  //  }

  private def produceSign(in : Int) : Int = if (in == 1) 1 else -1

  private def constructCombinations(iterDomain : Array[Int]) : Iterator[Array[Int]] = {
    val counters : Array[Int] = Array.fill(iterDomain.size)(0)

    val iter : Iterator[Array[Int]] = new Iterator[Array[Int]] {

      var started : Boolean = false

      def next() : Array[Int] = {

        if (!hasNext())
          throw new IllegalStateException("Cannot produce next element.")

        started = true

        val res : Array[Int] = new Array(counters.length)
        Array.copy(counters, 0, res, 0, counters.length)
        for (i <- (0 until counters.length).reverse) {
          counters(i) = (counters(i) + 1) % iterDomain(i)
          if (counters(i) > 0)
            return res
        }
        return res
      }

      def hasNext() : Boolean = !started || !counters.forall(_ == 0)
    }
    return iter
  }
}