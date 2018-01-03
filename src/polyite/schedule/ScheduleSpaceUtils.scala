package polyite.schedule

import java.util.logging.Logger

import scala.Ordering
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.Random

import isl.Conversions.convertLambdaToVoidCallback1
import isl.Isl
import isl.Isl.TypeAliases.T_IN
import isl.Isl.TypeAliases.T_OUT
import isl.Isl.TypeAliases.T_PAR
import isl.Isl.TypeAliases.T_SET
import isl.IslException
import polyite.ScopInfo
import polyite.config.Config
import polyite.util.Util
import polyite.util.Util.GeneratorsRat
object ScheduleSpaceUtils {
  val myLogger : Logger = Logger.getLogger("")
  /**
    * Check whether dependence {@code d} is carried by all schedules resulting
    * from the schedule coefficient vectors in {@code coeffSpace}.
    */
  def checkCarried(domInfo : DomainCoeffInfo,
    coeffSpace : isl.Set)(d : Dependence) : Boolean = {
    return coeffSpace.isSubset(d.strongConstr)
  }

  /**
    * Constructs a schedule space as a list of Chernikova generator sets. The
    * schedules in the constructed schedule space will carry all dependences
    * contained by {@code deps} in the order given in {@code depsPartition}. The
    * function stops generating new dimensions as soon as all dependences have
    * been carried.
    *
    * @param deps dependences that will be carried strongly by the schedules
    * contained in the generated schedule space.
    * @param depsPartition a list of subsets of {@code deps}. Each subset
    * specifies the dependences that will be carried strongly by the schedule
    * dimension with the same index. Each element of {@code deps} must occur in
    * exactly one set in {@code depsPartition}.
    *
    * @return Returns {@code None} if it is not possible to construct a schedule
    * space according to {@code depsPartition}.
    *
    * @throws IllegalArgumentException thrown if {@code depsPartition} doesn't
    * contain all elements of {@code deps} or a dependence occurs in more than
    * one set of {@code depsPartition}.
    */
  def constructScheduleSpaceFromDepsPartitionDual(deps : Set[Dependence],
    depsPartition : List[Set[Dependence]],
    domInfo : DomainCoeffInfo) : Option[Iterable[GeneratorsRat]] = {
    checkIsValidPartitioning(deps, depsPartition)
    constructScheduleSpaceFromDepsPartition(deps, depsPartition, domInfo) match {
      case None => return None
      case Some(sp) => {
        return Some(sp.map(Util.constraints2GeneratorsRat))
      }
    }
  }

  /**
    * Calculate data dependences domain and domain coefficient info for the given
    * SCoP.
    */
  def calcDepsAndDomInfo(scop : ScopInfo) : (Set[Dependence], DomainCoeffInfo) = {
    myLogger.info("Computing dependencies and domain coefficient info.")
    val empty = isl.UnionMap.empty(scop.getDomain.getSpace().params)
    val depArr = new Array[isl.UnionMap](1)
    val depArr2 = new Array[isl.UnionMap](1)

    val schedule = Isl.simplify(scop.getSched.intersectDomain(scop.getDomain))

    val writes = if (scop.getWrs == null)
      empty
    else
      Isl.simplify(scop.getWrs.intersectDomain(scop.getDomain).coalesce())
    //val writes = Isl.simplify(scop.getWrs.gistDomain(scop.getDomain))
    val reads = if (scop.getRds == null)
      empty
    else
      Isl.simplify(scop.getRds.intersectDomain(scop.getDomain).coalesce())
    //else Isl.simplify(scop.getRds.gistDomain(scop.getDomain))

    // anti & output
    myLogger.info("Computing anti and output dependencies.")
    writes.computeFlow(writes, reads, schedule,
      depArr, depArr2, null, null) // output params
    var antiOut = depArr(0).union(depArr2(0))

    //    if (Util.islUnionMapRangeIsWrapping(antiOut))
    //      antiOut = antiOut.uncurry().domain().unwrap()

    // flow
    myLogger.info("Computing flow dependencies.")
    reads.computeFlow(writes, empty, schedule,
      depArr, null, null, null) // output params (C-style)
    var flow = depArr(0)

    //    if (Util.islUnionMapRangeIsWrapping(flow))
    //      flow = flow.uncurry().domain().unwrap()

    myLogger.info("Unifiying the anti, output and flow dependencies.")
    //val deps : isl.UnionMap = Isl.simplify(antiOut.union(flow))
    val deps : isl.UnionMap = Isl.simplify(antiOut.union(flow))

    val domInfo = DomainCoeffInfo(scop.getDomain)

    myLogger.info("Separating distinct dependencies.")
    val depList : Iterable[isl.BasicMap] = preprocess(deps, scop)
    val dependences : Set[Dependence] = bMaps2Deps(depList, domInfo)
    (dependences, domInfo)
  }
  private def bMaps2Deps(bMaps : Iterable[isl.BasicMap], domInfo : DomainCoeffInfo) : Set[Dependence] = {
    return bMaps.toSet.map { d : isl.BasicMap =>
      val weakConstr : isl.BasicSet = compSchedConstrForDep(d, domInfo, false)
      val strongConstr : isl.BasicSet = compSchedConstrForDep(d, domInfo, true)
      new Dependence(d, weakConstr, strongConstr)
    }
  }
  /**
    * Calculate the input dependences of the given SCoP according to {@code sched}. These are interesting for schedule analysis.
    */
  def calcInputDeps(scop : ScopInfo, domInfo : DomainCoeffInfo, sched : isl.UnionMap) : Set[Dependence] = {
    //    val depArr : Array[isl.UnionMap] = new Array[isl.UnionMap](1)
    val empty = isl.UnionMap.empty(scop.getDomain.getSpace().params)
    val reads = if (scop.getRds == null)
      empty
    else
      scop.getRds.intersectDomain(scop.getDomain).removeRedundancies().coalesce()
    val schedule = sched.intersectDomain(scop.getDomain).removeRedundancies().coalesce()
    //    reads.computeFlow(reads, empty, schedule, depArr, null, null, null) // output params (C-style)
    //    val input : isl.UnionMap = depArr(0)
    val order : isl.UnionMap = schedule.lexLtUnionMap(schedule)
    val input : isl.UnionMap = reads.applyRange(reads.reverse()).intersect(order)

    val depList : Iterable[isl.BasicMap] = sepDepsFast(input, scop)
    return bMaps2Deps(depList, domInfo)
  }

  private def sepDepsFast(deps : isl.UnionMap, scop : ScopInfo) : Set[isl.BasicMap] = {
    var result : HashSet[isl.BasicMap] = HashSet.empty
    deps.foreachMap((m : isl.Map) => {
      m.foreachBasicMap((bm : isl.BasicMap) => result.add(bm.removeDivs()))
    })
    val resFiltered : Set[isl.BasicMap] = result.toSet.filterNot { (d : isl.BasicMap) =>
      d.intersectDomain(scop.getDomain).intersectRange(scop.getDomain).isEmpty()
    }
    return resFiltered
  }

  /**
    * Splits the data dependence union map {@code deps} into separate basic maps.
    */
  def preprocess(deps : isl.UnionMap, scop : ScopInfo) : ArrayBuffer[isl.BasicMap] = {
    val res = new ArrayBuffer[isl.BasicMap]()
    val maxSplit : Int = 100

    deps.foreachMap { map : isl.Map =>
      val interm = new ArrayBuffer[isl.BasicMap]()
      var i : Int = 0
      val mapUnwrapped : isl.Map = if (Isl.islMapRangeIsWrapping(map))
        map.uncurry().domain().unwrap()
      else
        map
      var depsMap : isl.Map = mapUnwrapped
      do {
        var dep : isl.Map = Isl.simplify(depsMap.lexmin())
        try {
          dep.foreachBasicMap { bmap : isl.BasicMap =>
            interm += bmap.removeDivs()
            i += 1
            if (i > maxSplit)
              throw new PreprocessTimeoutException
          }
        } catch {
          case e : PreprocessTimeoutException => ()
        }
        depsMap = depsMap.subtract(dep)
      } while (!depsMap.isEmpty() && i <= maxSplit)
      if (i <= maxSplit)
        res ++= interm
      else
        mapUnwrapped.foreachBasicMap { bmap : isl.BasicMap =>
          res += bmap.removeDivs()
        }
    }

    val resFiltered : ArrayBuffer[isl.BasicMap] = res.filterNot { (d : isl.BasicMap) =>
      d.intersectDomain(scop.getDomain).intersectRange(scop.getDomain).isEmpty()
    }

    return resFiltered.sorted(new Ordering[isl.BasicMap]() {
      def compare(x : isl.BasicMap, y : isl.BasicMap) : Int = {
        val xMax : String = Ordering[String].max(x.getTupleName(isl.DimType.In), x.getTupleName(isl.DimType.Out))
        val yMax : String = Ordering[String].max(y.getTupleName(isl.DimType.In), y.getTupleName(isl.DimType.Out))
        return xMax.compareTo(yMax)
      }
    })
  }

  private class PreprocessTimeoutException extends RuntimeException

  /**
    * Constructs a schedule space as a list of Isl sets. The
    * schedules in the constructed schedule space will carry all dependences
    * contained by {@code deps} in the order given in {@code depsPartition}. The
    * function stops generating new dimensions as soon as all dependences have
    * been carried.
    *
    * @param deps dependences that will be carried strongly by the schedules
    * contained in the generated schedule space.
    * @param depsPartition a list of subsets of {@code deps}. Each subset
    * specifies the dependences that will be carried strongly by the schedule
    * dimension with the same index. Each element of {@code deps} must occur in
    * exactly one set in {@code depsPartition}.
    *
    * @return Returns {@code None} if it is not possible to construct a schedule
    * space according to {@code depsPartition}.
    *
    * @throws IllegalArgumentException thrown if {@code depsPartition} doesn't
    * contain all elements of {@code deps} or a dependence occurs in more than
    * one set of {@code depsPartition}.
    */
  def constructScheduleSpaceFromDepsPartition(deps : Set[Dependence],
    depsPartition : List[Set[Dependence]],
    domInfo : DomainCoeffInfo) : Option[Iterable[isl.Set]] = {
    checkIsValidPartitioning(deps, depsPartition)

    var schedSpace : List[isl.Set] = List.empty
    var uncarriedDeps : HashSet[Dependence] = HashSet.empty
    var depsPartitionRest : List[Set[Dependence]] = depsPartition
    deps.map(uncarriedDeps.add)

    while (!uncarriedDeps.isEmpty) {
      val depsToCarry : Set[Dependence] = depsPartitionRest.head
      depsPartitionRest = depsPartitionRest.tail
      val (coeffSpace : isl.Set, uncarryableDeps : Set[Dependence]) = calculateCoeffSpace(uncarriedDeps, depsToCarry, domInfo.universe)
      if (!depsToCarry.isEmpty && !uncarryableDeps.isEmpty)
        return None
      schedSpace ::= coeffSpace
      uncarriedDeps = uncarriedDeps -- depsToCarry
    }
    return Some(schedSpace.reverse)
  }

  private def checkIsValidPartitioning(deps : Set[Dependence],
    depsPartition : List[Set[Dependence]]) {
    val foundDeps : HashSet[Dependence] = HashSet.empty
    for (p : Set[Dependence] <- depsPartition) {
      for (d : Dependence <- p)
        if (foundDeps.contains(d))
          throw new IllegalArgumentException("A dependence occurred more than "
            + "once in the given dependences partitioning.")
        else
          foundDeps.add(d)
    }
    if (foundDeps.size < deps.size)
      throw new IllegalArgumentException("At least one dependence is not "
        + "mapped to a dimension by the given dependences partitioning.")
  }

  /**
    * Constructs a random schedule space as a list of Chernikova generator sets.
    * The generated dimensions will carry strongly all dependences from
    * {@code deps2CarryStrongly}.
    *
    * @throws InterruptedException thrown if the number of required Isl-operations exceeds the
    * configuration option {@code islComputeout}.
    */
  def constructScheduleSpace(domInfo : DomainCoeffInfo,
    deps2CarryStrongly : Set[Dependence], conf : Config) : Iterable[GeneratorsRat] = {
    return constructScheduleSpace(new Schedule(domInfo, deps2CarryStrongly),
      deps2CarryStrongly, conf)
  }

  /**
    * Constructs a random schedule space as a list of Chernikova generator sets.
    * The generated dimensions will carry strongly all dependences from
    * {@code deps2CarryStrongly}.
    *
    * @return the schedule space and total time spent inside Chernikova in milliseconds.
    *
    * @throws InterruptedException thrown if the number of required Isl-operations exceeds the
    * configuration option {@code islComputeout}.
    */
  def constructScheduleSpaceWithProfile(domInfo : DomainCoeffInfo,
    deps2CarryStrongly : Set[Dependence], conf : Config) : (Iterable[GeneratorsRat], Long) = {
    return constructScheduleSpaceWithProfile(new Schedule(domInfo, deps2CarryStrongly),
      deps2CarryStrongly, conf)
  }

  /**
    * Constructs a random schedule space as a list of Chernikova generator sets
    * for the suffix of the given partial schedule. The generated dimensions will
    * carry strongly all dependences from {@code deps2CarryStrongly}.
    *
    * @return a Tuple consisting of the schedule space representation and the total duration spent inside the Chernikova
    * algorithm in milliseconds.
    */
  def constructScheduleSpaceWithProfile(partialSched : Schedule,
    deps2CarryStrongly : Set[Dependence], conf : Config) : (Iterable[GeneratorsRat], Long) = {

    /*
     * Use an own ISL context in here in order to be able to work with
     * isl_ctx_set_max_operations on multiple threads. This requires the
     * transfer of any existing ISL that is used to the new context.
     */
    val tmpCtx : isl.Ctx = Isl.initCtx()
    try {
      return constructScheduleSpaceHelp(tmpCtx, partialSched, deps2CarryStrongly, conf)
    } catch {
      case e : IslException => {
        //        tmpCtx.resetError()
        if (tmpCtx.getRemainingOperations <= 0) {
          val msg : String = "The number of Isl operations for schedule space " +
            "construction has reached islComputeout: " + conf.islComputeout
          myLogger.warning(msg)
          throw new InterruptedException(msg)
        }
        throw e
      }
    }
  }

  /**
    * Constructs a random schedule space as a list of Chernikova generator sets
    * for the suffix of the given partial schedule. The generated dimensions will
    * carry strongly all dependences from {@code deps2CarryStrongly}.
    */
  def constructScheduleSpace(partialSched : Schedule,
    deps2CarryStrongly : Set[Dependence], conf : Config) : Iterable[GeneratorsRat] = {
    return constructScheduleSpaceWithProfile(partialSched, deps2CarryStrongly, conf)._1
  }

  private def constructScheduleSpaceHelp(tmpCtx : isl.Ctx, partialSched : Schedule,
    deps2CarryStrongly : Set[Dependence], conf : Config) : (Iterable[GeneratorsRat], Long) = {
    val schedTmpCtx : Schedule = partialSched.transferToCtx(tmpCtx)

    val deps2CarryStronglyTmpCtx : Set[Dependence] = {
      val deps2CarryStronglyStr : Set[String] = deps2CarryStrongly.map(_.toString())
      for (d <- schedTmpCtx.deps; if (deps2CarryStronglyStr.contains(d.toString())))
        yield d
    }
    val domInfo : DomainCoeffInfo = schedTmpCtx.domInfo

    tmpCtx.setMaxOperations(conf.islComputeout)

    // Construct the schedule space
    val searchSpaceGenerators : ListBuffer[GeneratorsRat] = ListBuffer.empty
    val initCarriedDeps : Set[Dependence] = schedTmpCtx.getCarriedDeps
    var uncarriedDeps : Set[Dependence] = schedTmpCtx.deps -- initCarriedDeps
    var deps2CarryStronglyRand : List[Dependence] = Random
      .shuffle((deps2CarryStronglyTmpCtx -- initCarriedDeps).toList)

    // time spent in Chernikova
    var chernikovaDuration : Long = 0

    /* 
     * As long as the set of dependences that must be carried strongly is not
     * empty, add an additional schedule dimension.
     */
    while (!deps2CarryStronglyRand.isEmpty) {
      tmpCtx.resetOperations()

      if (Thread.interrupted())
        throw new InterruptedException
      var coeffSpace : isl.Set = null
      if (Random.nextDouble() < conf.probabilityToCarryDep) {
        val numDepsToCarry : Int = Random.nextInt(deps2CarryStronglyRand.size) + 1

        /*
         *  Choose dependences that should be carried strongly by the current
         *  dimension.
         */
        var carriedByCurrDim = deps2CarryStronglyRand.take(numDepsToCarry).toSet
        val (coeffSpaceTmp, uncarryableDeps) = calculateCoeffSpace(uncarriedDeps,
          carriedByCurrDim, domInfo.universe)
        coeffSpace = coeffSpaceTmp
        carriedByCurrDim --= uncarryableDeps

        uncarriedDeps = uncarriedDeps.filterNot(carriedByCurrDim.contains)
        deps2CarryStronglyRand = deps2CarryStronglyRand.filterNot(carriedByCurrDim.contains)
      } else {
        coeffSpace = calculateCoeffSpace(domInfo, uncarriedDeps, false)
      }
      uncarriedDeps = uncarriedDeps.filterNot(checkCarried(domInfo, coeffSpace))
      deps2CarryStronglyRand = deps2CarryStronglyRand.filterNot(checkCarried(domInfo, coeffSpace))
      val start : Long = System.currentTimeMillis()
      val generators : GeneratorsRat = Util.constraints2GeneratorsRat(coeffSpace, conf.moveVertices, conf.rayPruningThreshold)
      chernikovaDuration += System.currentTimeMillis() - start
      searchSpaceGenerators.append(generators)
      System.gc()
      System.gc()
      System.gc()
    }
    return (searchSpaceGenerators, chernikovaDuration)
  }

  /**
    * Calculates the schedule coefficient space such that all of the given
    * dependences are carried at least weakly. Use {@code makePar} to the require
    * that all of the given dependences are carried just weakly.
    */
  def calculateCoeffSpace(domInfo : DomainCoeffInfo,
    deps : Iterable[Dependence], makePar : Boolean) : isl.Set = {
    var coeffSpace : isl.Set = domInfo.universe

    // all dependences must be satisfied at least weakly
    for (dep <- deps) {
      val constr : isl.BasicSet =
        if (makePar)
          compParSchedConstrForDep(dep.map, domInfo)
        else
          dep.weakConstr
      coeffSpace = coeffSpace.intersect(constr)
    }
    return coeffSpace
  }

  /**
    * Calculates the schedule coefficient space such that all of the given
    * dependences are carried at least weakly. The function tries to strongly
    * carry as many dependences from {@code depsToCarry} as possible.
    *
    * @param universe This is the set that we start from. Use the full-space
    * polyhedron if only the dependences should bound the set of legal schedules
    * or an initial set of constraint. These could for instance limit the
    * schedule coefficients to values from some interval.
    *
    * @return Returns the resulting schedule coefficient set and the set of
    * dependences from {@code depsToCarry} that could not be guaranteed to be
    * carried strongly.
    */
  def calculateCoeffSpace(deps : Iterable[Dependence],
    depsToCarry : Iterable[Dependence], universe : isl.Set) : (isl.Set, Set[Dependence]) = {
    var coeffSpace : isl.Set = universe

    // weakly satisfy all dependences
    for (dep <- deps) {
      coeffSpace = coeffSpace.intersect(dep.weakConstr)
    }

    // try to satisfy strongly as many dependences from depsToCarry as possible.
    val unsatisfiableDeps : HashSet[Dependence] = HashSet.empty
    for (dep <- depsToCarry) {
      val coeffSpaceOld : isl.Set = coeffSpace
      coeffSpace = coeffSpace.intersect(dep.strongConstr)
      if (coeffSpace.isEmpty()) {
        coeffSpace = coeffSpaceOld
        unsatisfiableDeps.add(dep)
      }
    }
    return (coeffSpace, unsatisfiableDeps.toSet)
  }

  def compParSchedConstrForDep(dep : isl.BasicMap, domInfo : DomainCoeffInfo) : isl.BasicSet = {
    val solvesDepW : isl.BasicSet = compSchedConstrForDep(dep, domInfo, false)
    val revDep : isl.BasicMap = dep.reverse()
    val solvesRevDepW : isl.BasicSet = compSchedConstrForDep(revDep, domInfo, false)
    return solvesDepW.intersect(solvesRevDepW)
  }

  def compSchedConstrForDep(dep : isl.BasicMap, domInfo : DomainCoeffInfo,
    strongSatisfy : Boolean) : isl.BasicSet = {
    val ctx : isl.Ctx = dep.getCtx()
    val depSrcName = dep.getTupleName(T_IN)
    val depDstName = dep.getTupleName(T_OUT)
    val inEqOut : Boolean = depSrcName == depDstName

    val srcDim = dep.dim(T_IN)
    val dstDim = dep.dim(T_OUT)
    val parDim = dep.dim(T_PAR)
    val depPoly : isl.BasicSet = dep.moveDims(T_IN, srcDim, T_OUT, 0, dstDim).domain()
    var schedCoeffs : isl.BasicMap = depPoly.coefficients().unwrap()

    if (schedCoeffs.hasTupleId(T_OUT))
      schedCoeffs = Isl.islBasicMapFromMap(schedCoeffs.resetTupleId(T_OUT))

    // if in and out are for the same statement:
    //   coefficients of source and dest must have the same value but a different sign
    //     "[ci0,ci1, di0,di1] -> [-ci0,-ci1] : ci0 = -di0 and ci1 = -di1"
    // else
    //   invert coefficients for source statement
    //     "[ci0,ci1, di0] -> [-ci0,-ci1, di0]"
    val njuDim : Int = if (inEqOut) srcDim else srcDim + dstDim
    var postprocIt = isl.Map.universe(isl.Space.alloc(ctx, 0, srcDim + dstDim, njuDim))
    for (i <- 0 until srcDim)
      postprocIt = postprocIt.oppose(T_IN, i, T_OUT, i)
    val njuOff : Int = if (inEqOut) 0 else srcDim
    for (i <- 0 until dstDim)
      postprocIt = postprocIt.equate(T_IN, srcDim + i, T_OUT, njuOff + i)
    schedCoeffs = schedCoeffs.applyRange(postprocIt.affineHull())

    // if in and out are for the same statement:
    //   coefficient for parameters must be 0, constant must be  if (strong) -1 else 0
    //     "[c_cst, c_p0,c_p1] -> [] : c_cst = {-1/0} and c_p0 = 0 and c_p1 = 0"
    // else
    //   split single coefficients for parameter and constants in two (one for each schedule)
    //     "[c_cst, c_p0,c_p1] -> [cs,cd, cp0s,cp1s,cp0d,cp1d] : c_cst = cd-cs{-1} and c_p0 = cp0d-cp0s and c_p1 = cp1d-cp1s"
    //       ==
    //     "[c_cst, c_p0,c_p1] -> [cs, c_cst+cs{+1}, cp0s, cp1s, c_p0+cp0s, c_p1+cp1s]"
    var postprocPar : isl.BasicMap = null
    if (inEqOut) {
      postprocPar = isl.BasicMap.universe(isl.Space.alloc(ctx, 0, parDim + 1, 0))

      val zeroVal = isl.Val.zero(ctx)
      val valC = if (strongSatisfy) isl.Val.negone(ctx) else zeroVal
      postprocPar = postprocPar.fixVal(T_IN, 0, valC)
      for (i <- 0 until parDim)
        postprocPar = postprocPar.fixVal(T_IN, i + 1, zeroVal)

    } else {
      // parameter dimensions are  [cs, cp0s,cp1s]
      var mAff = isl.MultiAff.zero(isl.Space.alloc(ctx, parDim + 1, parDim + 1, 2 * parDim + 2))
      val lSp = isl.LocalSpace.fromSpace(isl.Space.setAlloc(ctx, parDim + 1, parDim + 1))
      val oneVal = isl.Val.one(ctx)

      val affC1 = isl.Aff.varOnDomain(lSp, T_PAR, 0)
      var affC2 = isl.Aff.varOnDomain(lSp, T_SET, 0).add(affC1)
      if (strongSatisfy)
        affC2 = isl.Aff.valOnDomain(lSp, oneVal).add(affC2)

      mAff = mAff.setAff(0, affC1)
      mAff = mAff.setAff(1, affC2)
      for (i <- 0 until parDim) {
        val affP1 = isl.Aff.varOnDomain(lSp, T_PAR, i + 1)
        val affP2 = isl.Aff.varOnDomain(lSp, T_SET, i + 1).add(affP1)
        mAff = mAff.setAff(i + 2, affP1)
        mAff = mAff.setAff(i + 2 + parDim, affP2)
      }
      postprocPar = isl.BasicMap.fromMultiAff(mAff).projectOut(T_PAR, 0, parDim + 1)
    }
    schedCoeffs = schedCoeffs.applyDomain(postprocPar)

    // expand dimensionality to global schedule constraints
    val StmtCoeffInfo(srcItOff, srcNrIt, srcParOff, srcCstIdx) = domInfo.stmtInfo(depSrcName)
    val StmtCoeffInfo(dstItOff, dstNrIt, dstParOff, dstCstIdx) = domInfo.stmtInfo(depDstName)

    val nrPar : Int = domInfo.nrParPS
    var solSpMap = isl.BasicMap.fromDomainAndRange(
      schedCoeffs.reverse().wrap().flatten(),
      isl.BasicSet.universe(isl.Space.setAlloc(ctx, 0, domInfo.dim)))

    var off : Int = 0

    for (i <- 0 until srcNrIt)
      solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, srcItOff + i)
    if (!inEqOut) {
      off += srcNrIt
      for (i <- 0 until dstNrIt)
        solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, dstItOff + i)
      off += dstNrIt

      solSpMap = solSpMap.equate(T_IN, off, T_OUT, srcCstIdx)
      off += 1
      solSpMap = solSpMap.equate(T_IN, off, T_OUT, dstCstIdx)
      off += 1

      for (i <- 0 until nrPar)
        solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, srcParOff + i)
      off += nrPar
      for (i <- 0 until nrPar)
        solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, dstParOff + i)
      // off += nrPar // not needed anymore (comment is just for symmetry)
    }

    return solSpMap.range()
  }

  /**
    * Filters a given set of dependences for dependences that only affect
    * statements that are part of the given domain.
    */
  def filterDepSetForDomain(deps : Set[Dependence], domain : isl.UnionSet) : Set[Dependence] = {
    val domainStmtNames : Set[String] = Isl.islUnionSetGetTupleNames(domain)
    return deps.filter { d =>
      {
        val s1 : String = d.getTupleNameIn()
        val s2 : String = d.getTupleNameOut()
        domainStmtNames.contains(s1) && domainStmtNames.contains(s2)
      }
    }
  }

  /**
    * Iterate over the given list of generator sets and determine the maximum number of lines of all dimensions.
    */
  def getMaxNumLines(schedSpaceGenerators : Iterable[GeneratorsRat]) : Int = {
    if (schedSpaceGenerators.isEmpty)
      return 0
    schedSpaceGenerators.map(_.lines.size).max
  }

  /**
    * Iterate over the given list of generator sets and determine the maximum number of rays of all dimensions.
    */
  def getMaxNumRays(schedSpaceGenerators : Iterable[GeneratorsRat]) : Int = {
    if (schedSpaceGenerators.isEmpty)
      return 0
    return schedSpaceGenerators.map(_.rays.size).max
  }

  /**
    * Counts for each of the dependences in {@code deps} the number of dependences that it interferes with.
    */
  def calcInterferenceOfDeps(deps : Iterable[Dependence],
    initBoundingBox : isl.Set) : Map[Dependence, Int] = {
    val depA : Array[Dependence] = deps.toArray
    val result : HashMap[Dependence, Int] = HashMap.empty
    for (i <- 0 until depA.length) {
      var nInterf : Int = 0
      for (j <- 0 until depA.length)
        if (i != j && !checkInterference(depA(i), depA(j), initBoundingBox))
          nInterf += 1
      result.put(depA(i), nInterf)
      System.gc()
    }
    return result.toMap
  }

  /**
    * Check whether {@code d1} interferes with {@code d2}, that is, they cannot be carried together by a one-dimensional
    * schedule.
    * @param initBoundingBox A bounding box for the schedule coefficient space. Typically, this is a hyper-cube.
    */
  def checkInterference(d1 : Dependence, d2 : Dependence,
    initBoundingBox : isl.Set) : Boolean = {
    myLogger.info("Checking interference of dependences " + d1 + " and " + d2)
    val depSet : Set[Dependence] = Set(d1, d2)
    val (_, uncarried : Set[Dependence]) = ScheduleSpaceUtils
      .calculateCoeffSpace(depSet, depSet, initBoundingBox)
    val interfDetected = !uncarried.isEmpty
    myLogger.info("interference status of " + d1 + " and " + d2 + ": " + interfDetected)
    return interfDetected
  }

  private var memAccesses : HashMap[String, StmtMemAccesses] = null

  /**
    * Calculates the amount of data communicated by each of the dependences in {@code deps}.
    */
  def calcTrafficSizesOfDeps(deps : Iterable[Dependence],
    scop : ScopInfo, conf : Config) : Map[Dependence, Long] = {
    if (memAccesses == null)
      memAccesses = buildMemAccessMap(scop)

    val result : HashMap[Dependence, Long] = HashMap.empty
    for (d <- deps) {
      val sourceStmt : String = d.getTupleNameIn()
      val destStmt : String = d.getTupleNameOut()

      val srcReadLocs : Set[String] = getAccessedMemLocs(scop.getRds, sourceStmt)
      val destReadLocs : Set[String] = getAccessedMemLocs(scop.getRds, destStmt)

      val srcWriteLocs : Set[String] = getAccessedMemLocs(scop.getWrs, sourceStmt)
      val destWriteLocs : Set[String] = getAccessedMemLocs(scop.getWrs, destStmt)

      val rrTrafficSize = calcTrafficSizeOfDep(d.map, srcReadLocs, destReadLocs,
        memAccesses(sourceStmt).rds, memAccesses(destStmt).rds, conf)
      val rwTrafficSize = calcTrafficSizeOfDep(d.map, srcReadLocs, destWriteLocs,
        memAccesses(sourceStmt).rds, memAccesses(destStmt).wrs, conf)
      val wrTrafficSize = calcTrafficSizeOfDep(d.map, srcWriteLocs, destReadLocs,
        memAccesses(sourceStmt).wrs, memAccesses(destStmt).rds, conf)
      val wwTrafficSize = calcTrafficSizeOfDep(d.map, srcWriteLocs, destWriteLocs,
        memAccesses(sourceStmt).wrs, memAccesses(destStmt).wrs, conf)
      result.put(d, rrTrafficSize + rwTrafficSize + wrTrafficSize + wwTrafficSize)
    }
    result.toMap
  }

  var stmtMemTraffic : Map[String, Long] = null

  def calcMemTrafficSizesOfDepStmts(deps : Iterable[Dependence],
    scop : ScopInfo, conf : Config) : Map[Dependence, Long] = {
    if (memAccesses == null)
      memAccesses = buildMemAccessMap(scop)
    val result : HashMap[Dependence, Long] = HashMap.empty

    if (stmtMemTraffic == null)
      stmtMemTraffic = Isl.islUnionSetGetTupleNames(scop.getDomain).map((stmt : String) => {
        (stmt, calcMemTrafficSizeOfStmt(stmt, scop, conf, memAccesses(stmt)))
      }).toMap

    for (d <- deps) {
      val sourceStmt : String = d.getTupleNameIn()
      val destStmt : String = d.getTupleNameOut()
      result.put(d, stmtMemTraffic(sourceStmt) + stmtMemTraffic(destStmt))
    }
    return result.toMap
  }

  private def calcMemTrafficSizeOfStmt(stmt : String, scop : ScopInfo, conf : Config, memAccesses : StmtMemAccesses) : Long = {
    val paramVals : Map[String, Int] = conf.paramValMappings
    val ctx : isl.Ctx = scop.getDomain.getCtx
    val uniqueAccesses : HashMap[String, HashSet[isl.Map]] = HashMap.empty
    val stmtDomain : isl.Set = isl.Set.fromUnionSet(Isl.islUnionSetFilter(scop.getDomain, Set(stmt)))
    for (memLoc : String <- memAccesses.rds.keySet) {
      if (!uniqueAccesses.contains(memLoc))
        uniqueAccesses.put(memLoc, HashSet.empty)
      memAccesses.rds(memLoc).filterNot((access : isl.Map) => uniqueAccesses(memLoc).exists(_.toString().equals(access.toString()))).foreach(uniqueAccesses(memLoc).add(_))
    }
    for (memLoc : String <- memAccesses.wrs.keySet) {
      if (!uniqueAccesses.contains(memLoc))
        uniqueAccesses.put(memLoc, HashSet.empty)
      memAccesses.wrs(memLoc).filterNot((access : isl.Map) => uniqueAccesses(memLoc).exists(_.toString().equals(access.toString()))).foreach(uniqueAccesses(memLoc).add(_))
    }
    var result : Long = 0
    for (memLoc <- uniqueAccesses.keySet) {
      for (access <- uniqueAccesses(memLoc)) {
        var accessed : isl.Set = access.intersectDomain(stmtDomain).wrap()
        if (!accessed.isEmpty()) {
          val nParam = accessed.getSpace.dim(T_PAR)
          for (i <- 0 until nParam) {
            val dimName : String = accessed.getDimName(T_PAR, i)
            if (paramVals.contains(dimName))
              accessed = accessed.fixVal(T_PAR, i, isl.Val
                .fromInt(ctx, paramVals(dimName)))
            else
              // param value is unknown. make it zero allow counting of inner points
              accessed = accessed.fixVal(T_PAR, i, isl.Val.fromInt(ctx, 0))
          }
        }
        accessed = Isl.simplify(accessed)
        result += countNPoints(accessed, conf)
      }
    }
    return result
  }

  private def calcTrafficSizeOfDep(dep : isl.Map, srcAccessLocs : Set[String],
    destAccessLocs : Set[String],
    srcMemLocs2MemAccesses : HashMap[String, HashSet[isl.Map]],
    destMemLocs2MemAccesses : HashMap[String, HashSet[isl.Map]],
    conf : Config) : Long = {
    myLogger.fine("Calculating traffic size for dependence: " + dep)
    val ctx : isl.Ctx = dep.getCtx
    val paramVals : Map[String, Int] = conf.paramValMappings
    val srcStmt : String = dep.getTupleName(T_IN)
    val destStmt : String = dep.getTupleName(T_OUT)
    var trafficSize : Long = 0
    for (lSrc <- srcAccessLocs)
      for (lDest <- destAccessLocs)
        if (lSrc.equals(lDest)) {
          val srcAccessMaps = srcMemLocs2MemAccesses(lSrc)
          val destAccessMaps = destMemLocs2MemAccesses(lDest)
          val srcIterations : isl.Set = dep.domain()
          val destIterations : isl.Set = dep.range()
          val srcAccessed : List[isl.Set] = srcAccessMaps.toList.map(srcIterations.apply)
          val destAccessed : List[isl.Set] = destAccessMaps.toList.map(destIterations.apply)

          for (s0 : isl.Set <- srcAccessed)
            for (s1 : isl.Set <- destAccessed) {
              var accessedByBoth : isl.Set = Isl.simplify(s0.intersect(s1))
              if (!accessedByBoth.isEmpty()) {
                val nParam = accessedByBoth.getSpace.dim(T_PAR)
                for (i <- 0 until nParam) {
                  val dimName : String = accessedByBoth.getDimName(T_PAR, i)
                  if (paramVals.contains(dimName))
                    accessedByBoth = accessedByBoth.fixVal(T_PAR, i, isl.Val
                      .fromInt(ctx, paramVals(dimName)))
                  else
                    // param value is unknown. make it zero allow counting of inner points
                    accessedByBoth = accessedByBoth.fixVal(T_PAR, i, isl.Val.fromInt(ctx, 0))
                }
              }
              accessedByBoth = Isl.simplify(accessedByBoth)
              trafficSize += countNPoints(accessedByBoth, conf)
            }
        }
    myLogger.fine("traffic size for " + dep + ": " + trafficSize)
    return trafficSize
  }

  private def countNPoints(s : isl.Set, conf : Config) : Long = {

    if (s.isEmpty())
      return 0

    Isl.islSetCountNPoints(conf.barvinokBinary, conf.barvinokLibraryPath, s).max().getNumSi
    val poly : isl.PwQpolynomial = Isl.islSetCountNPoints(conf.barvinokBinary, conf.barvinokLibraryPath, s)
    val max : isl.Val = poly.max()
    //    if (max.isNeginfty() || max.isInfty()) {
    //      println(poly)
    //      return 0
    //    } else
    return max.getNumSi
  }

  private def getAccessedMemLocs(memAccesses : isl.UnionMap, stmt : String) : Set[String] = {
    val memLocs : HashSet[String] = HashSet.empty
    memAccesses.foreachMap((a : isl.Map) => {
      if (a.getTupleName(T_IN).equals(stmt))
        memLocs.add(a.getTupleName(T_OUT))
    })
    return memLocs.toSet
  }

  private case class StmtMemAccesses(
    // MemLoc -> access maps
    rds : HashMap[String, HashSet[isl.Map]],
    wrs : HashMap[String, HashSet[isl.Map]])

  private def buildMemAccessMap(scop : ScopInfo) : HashMap[String, StmtMemAccesses] = {
    val result : HashMap[String, StmtMemAccesses] = HashMap.empty
    scop.getRds.foreachMap((a : isl.Map) => {
      val stmt : String = a.getTupleName(T_IN)
      val memLoc : String = a.getTupleName(T_OUT)
      if (!result.contains(stmt))
        result.put(stmt, StmtMemAccesses(HashMap.empty, HashMap.empty))
      if (!result(stmt).rds.contains(memLoc))
        result(stmt).rds.put(memLoc, HashSet.empty)
      result(stmt).rds(memLoc).add(a)
    })
    scop.getWrs.foreachMap((a : isl.Map) => {
      val stmt : String = a.getTupleName(T_IN)
      val memLoc : String = a.getTupleName(T_OUT)
      if (!result.contains(stmt))
        result.put(stmt, StmtMemAccesses(HashMap.empty, HashMap.empty))
      if (!result(stmt).wrs.contains(memLoc))
        result(stmt).wrs.put(memLoc, HashSet.empty)
      result(stmt).wrs(memLoc).add(a)
    })
    return result
  }
}
