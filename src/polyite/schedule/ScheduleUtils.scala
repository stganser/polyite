package polyite.schedule

import java.util.logging.Logger

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.Random

import isl.Conversions.convertLambdaToVoidCallback1
import isl.Conversions.convertLambdaToVoidCallback2
import isl.Conversions.convertValToBigInt
import isl.Isl
import isl.Isl.TypeAliases.T_IN
import isl.Isl.TypeAliases.T_OUT
import isl.Isl.TypeAliases.T_PAR
import isl.Isl.TypeAliases.T_SET
import isl.IslException
import polyite.config.Config
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.schedule.hash.ScheduleHash
import polyite.util.Rat
import polyite.util.Util
import polyite.schedule.sampling.SamplingStrategy
import polyite.schedule.sampling.ScheduleSummand
import polyite.schedule.sampling.SamplingStrategyParams
import polyite.schedule.sampling.Polyhedron

/**
  * Utilities for handling schedules.
  */
object ScheduleUtils {

  var myLogger : Logger = Logger.getLogger("")

  /**
    * Simplifies a given schedule by eliminating constant dimensions that apply the same offset to all
    * statements.
    */
  def simplify(s : Schedule) : Schedule = {
    elimFruitlessConstDims(s)
  }

  /**
    * Determines the direction that schedule {@code sched} imposes on
    * dependence {@code dep}.
    */
  def getDirectionOfDep(sched : isl.UnionMap, dep : Dependence) : CmpMultiResult = {
    val newDep : isl.UnionMap = dep.map.applyDomain(sched).applyRange(sched)
    var sp = sched.getSpace() // param space (since sched is a UnionMap)
    sp = sp.addDims(T_IN, 1).addDims(T_OUT, 1)
    val delta : isl.Set = newDep.extractMap(sp).deltas()

    val ctx = dep.map.getCtx()
    val univ = isl.Set.universe(delta.getSpace())
    val zer = univ.fixVal(T_SET, 0, isl.Val.zero(ctx))
    val pos = univ.lowerBoundVal(T_SET, 0, isl.Val.one(ctx))
    val neg = univ.upperBoundVal(T_SET, 0, isl.Val.negone(ctx))

    val isNeg = !delta.intersect(neg).isEmpty()
    val isZer = !delta.intersect(zer).isEmpty()
    val isPos = !delta.intersect(pos).isEmpty()

    return CmpMultiResult(isNeg, isZer, isPos)
  }

  case class CmpMultiResult(val isNegative : Boolean, val isZero : Boolean, val isPositive : Boolean) {
    val isNegativeOnly : Boolean = isNegative && !isZero && !isPositive
    val isZeroOnly : Boolean = !isNegative && isZero && !isPositive
    val isPositiveOnly : Boolean = !isNegative && !isZero && isPositive
  }

  /**
    * Filters {@code deps} for dependencies that are carried strongly by the
    * schedule given through the union map {@code s}.
    */
  def getDepsCarriedBySchedule(
    s : isl.UnionMap,
    deps : Set[Dependence]) : Set[Dependence] = {
    return deps.filter { d => getDirectionOfDep(s, d).isPositiveOnly }
  }

  /**
    * Filters {@code deps} for dependencies that are carried at least weakly by
    * the schedule given through the union map {@code s}.
    */
  def getDepsCarriedWeaklyBySchedule(
    s : isl.UnionMap,
    deps : Set[Dependence]) : Set[Dependence] = {
    return deps.filter { d =>
      {
        val dir = getDirectionOfDep(s, d)
        !dir.isNegative
      }
    }
  }

  private def getConstCoeffsForSttmnt(v : Array[Rat], domInfo : DomainCoeffInfo,
    sttmntInfo : StmtCoeffInfo) : List[Rat] = {
    var result : List[Rat] = List.empty
    for (i <- sttmntInfo.parStart until sttmntInfo.parStart + domInfo.nrParPS)
      result = v(i) :: result
    result = v(sttmntInfo.cstIdx) :: result
    return result.reverse
  }

  /**
    * Eliminates constant dimensions that apply the same offset to all statements.
    * Call this before elimSubseqConstDims.
    */
  private def elimFruitlessConstDims(sched : Schedule) : Schedule = {
    val domInfo : DomainCoeffInfo = sched.domInfo
    val result : Schedule = new Schedule(sched.domInfo, sched.deps)
    for (dim <- 0 until sched.numDims) {
      if (sched.isConstant(dim)) {
        val v : Array[Rat] = sched.getScheduleVector(dim).toArray
        var sttmnts : List[String] = domInfo.stmtInfo.keys.toList
        if (!sttmnts.isEmpty) {
          val fstSttmntConstCoeffs : List[Rat] = getConstCoeffsForSttmnt(
            v,
            domInfo, domInfo.stmtInfo(sttmnts.head))
          if (!sttmnts.forall { s =>
            getConstCoeffsForSttmnt(v, domInfo,
              domInfo.stmtInfo(s)).equals(fstSttmntConstCoeffs)
          })
            result.addForeignDim(sched, dim)
        }
      } else
        result.addForeignDim(sched, dim)
    }
    return result
  }

  /**
    * Construct {@code numCompletions} many schedules from the schedule prefix
    * {@code sched}. The resulting schedules will strongly carry all dependences
    * that are contained by {@code deps2CarryStrongly} Any other dependences that
    * are not being carried by the given schedule prefix will be carried weakly.
    *
    * In a post-processing step the schedule is expanded to a full schedule with
    * the necessary number of linearly independent dimensions.
    *
    * @param maxNumRays limit type for the maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumRays limit type for the maximum number of lines per linear combination that forms a schedule coefficient vector
    *
    * @throws InterruptedException thrown if either the current thread has been
    * interrupted or if number of required Isl-operations exceeds the
    * configuration option {@code islComputeout}.
    */
  def completeSchedule(sched : Schedule, numRaysLimit : NumGeneratorsLimit, numLinesLimit : NumGeneratorsLimit,
    conf : Config, numCompletions : Int, deps2CarryStrongly : Set[Dependence], sampler : SamplingStrategy) : HashSet[Schedule] = {

    // Sample a schedule space region
    val searchSpaceRegion : Iterable[Polyhedron] = ScheduleSpaceUtils.constructScheduleSpace(sched, deps2CarryStrongly,
      sampler.preparePolyhedron(_, _), conf)

    // Extract as many schedules from the above constructed schedule space region as required.
    var result : HashSet[Schedule] = HashSet.empty
    for (i <- 0 until numCompletions) {

      val samplerParams : SamplingStrategyParams = sampler.prepareSamplingStrategyParams(searchSpaceRegion, conf, numRaysLimit, numLinesLimit)

      val newSched : Schedule = sched.clone
      val gIter : Iterator[Polyhedron] = searchSpaceRegion.iterator
      while (gIter.hasNext && newSched.getCarriedDeps.size < newSched.deps.size) {
        if (Thread.interrupted())
          throw new InterruptedException()
        val (coeffs : List[Rat], schedSummands : Set[ScheduleSummand]) = sampler.sampleCoeffVect(
          gIter.next(),
          newSched.domInfo, conf, samplerParams)
        newSched.addScheduleVector(coeffs, schedSummands)
      }
      val schedCleaned : Schedule = simplify(newSched)

      /*
       * complete the schedule to a full schedule if it carries all dependences.
       * Otherwise we were asked to generate a schedule prefix.
       */
      if (schedCleaned.deps.size == (schedCleaned.getCarriedDeps.size)) {
        val fullSched : Schedule = expandToFullSchedule(conf, sampler, samplerParams, schedCleaned,
          generateLinIndepScheduleVector)
        result.add(fullSched)
      } else
        result.add(schedCleaned)
      System.gc()
    }
    return result
  }

  /**
    * Checks whether {@code s} is a valid schedule.
    */
  def isValidSchedule(s : Schedule) : Boolean = {
    var uncarriedDeps : Set[Dependence] = s.deps
    for (i : Int <- 0 until s.numDims) {
      val sched : isl.UnionMap = s.getSchedule(i)
      for (d : Dependence <- uncarriedDeps)
        if (getDirectionOfDep(sched, d).isNegative)
          return false
      uncarriedDeps = uncarriedDeps.filterNot { d =>
        getDirectionOfDep(sched, d).isPositiveOnly
      }
    }
    return uncarriedDeps.isEmpty
  }

  /**
    * Throws an {@code AssertionError} if {@code s} is not a valid schedule.
    */
  def assertValid(s : Schedule) = assert(isValidSchedule(s), s)

  private def getLastDimCarryingNewDep(s : Schedule) : Int = (0 until s.numDims)
    .foldLeft(0)((lastWithNewDep : Int, currDim : Int) =>
      if (!s.getDepsNewlyCarriedBy(currDim).isEmpty) currDim else lastWithNewDep)

  /**
    * Make sure that no linearly dependent dimension comes after the last
    * dimension of the given schedule that carries a dependence. Linearly dependent
    * means linearly dependent in the coefficients of the iteration variables.
    */
  def assertNoUnneccessaryDimsInSuffix(s : Schedule) {
    val lastDimCarryingNewDep = getLastDimCarryingNewDep(s)

    for (dim : Int <- lastDimCarryingNewDep + 1 until s.numDims)
      assert(s.isLinIndepToPrefix(dim), "Found a dimension in the suffix of a " +
        "schedule that is not linearly independent to the prefix.")
  }

  /**
    * Generates random schedules for the iteration domain described by {@code domInfo}
    * that carry the dependences in {@code deps}. The function attempts to generate
    * {@code maxNumScheds} many schedules.
    *
    * @param maxNumRays maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumLines maximum number of lines per linear combination that forms a schedule coefficient vector
    * @param domInfo models the schedule coefficient vector space
    * @param maxNumScheds the number of schedules to generate
    * @param sampler strategy for sampling schedules.
    * @param hashSched this function determines the criterion according to which two schedules are considered to be equivalent.
    * @param conf configuration properties.
    */
  def genRandSchedules(domInfo : DomainCoeffInfo, deps : Set[Dependence], maxNumScheds : Int,
    maxNumRays : NumGeneratorsLimit, maxNumLines : NumGeneratorsLimit, conf : Config, sampler : SamplingStrategy,
    hashSched : Schedule => ScheduleHash) : Set[Schedule] =
    genRandSchedules(domInfo, deps, maxNumScheds, Set.empty, maxNumRays, maxNumLines, conf, sampler, hashSched)

  /**
    * Generates a set of random schedules. Any schedules contained by {@code basis}
    * are also part of the result. The result contains at most
    * max(|basis|, maxNumScheds) schedules.
    *
    * @param maxNumRays maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumLines maximum number of lines per linear combination that forms a schedule coefficient vector
    * @param domInfo models the schedule coefficient vector space
    * @param maxNumScheds the number of schedules to generate
    * @param basis Set of schedules to add to until its size is at least {@code maxNumScheds}.
    * @param sampler strategy for sampling schedules.
    * @param hashSched this function determines the criterion according to which two schedules are considered to be equivalent.
    * @param conf configuration properties.
    */
  def genRandSchedules(
    domInfo : DomainCoeffInfo,
    deps : Set[Dependence], maxNumScheds : Int, basis : Set[Schedule], maxNumRays : NumGeneratorsLimit,
    maxNumLines : NumGeneratorsLimit, conf : Config, sampler : SamplingStrategy, hashSched : Schedule => ScheduleHash) : Set[Schedule] = {
    val scheds : HashMap[ScheduleHash, Schedule] = HashMap.empty
    basis.map((s : Schedule) => scheds.put(hashSched(s), s))

    def genScheds(idx : Int)(x : Unit) {
      var numFailures = 0

      def continue : Boolean = {
        scheds.synchronized {
          var cancel : Boolean = false
          if (numFailures >= conf.genSchedsMaxAllowedConseqFailures)
            cancel = true
          return !cancel && scheds.size < maxNumScheds
        }
      }

      while (continue) {
        val numSchedsAtOnce : Int = scheds.synchronized {
          math.min(
            Random.nextInt(conf.maxNumSchedsAtOnce) + 1,
            maxNumScheds - scheds.size)
        }
        val args : Array[Any] = Array(new Schedule(domInfo, deps), maxNumRays, maxNumLines, conf, numSchedsAtOnce, deps, sampler)
        val schedsMaybe : Option[Iterable[Schedule]] = Util.runWithTimeout(
          args,
          ((args : Array[Any]) => ScheduleUtils.completeSchedule(
            args(0).asInstanceOf[Schedule], args(1).asInstanceOf[NumGeneratorsLimit],
            args(2).asInstanceOf[NumGeneratorsLimit], args(3).asInstanceOf[Config], args(4).asInstanceOf[Int],
            args(5).asInstanceOf[Set[Dependence]], args(6).asInstanceOf[SamplingStrategy])), conf.randSchedsTimeout * 1000)
        var addedAny : Boolean = false
        schedsMaybe match {
          case None => myLogger.warning("(schedule gen worker #" + idx
            + ") Timeout of schedule completion.")
          case Some(newScheds) => {
            newScheds.map { (s : Schedule) =>
              {
                ScheduleUtils.assertValid(s)
                ScheduleUtils.assertNoUnneccessaryDimsInSuffix(s)

                scheds.synchronized {
                  if (scheds.size < maxNumScheds) {
                    val h : ScheduleHash = hashSched(s)
                    if (!scheds.contains(h)) {
                      scheds.put(h, s)
                      myLogger.info("(schedule gen worker #" + idx
                        + ")New random schedule: " + s.toString())
                      addedAny = true
                    }
                  }
                }
              }
            }
          }
        }
        if (addedAny) {
          numFailures = 0
        } else
          numFailures += 1
      }
    }
    val schedGenWorkers : ParArray[Unit => Unit] = new ParArray(conf.numScheduleGenThreads)
    for (i <- 0 until schedGenWorkers.length)
      schedGenWorkers(i) = genScheds(i)
    val pool = new ForkJoinPool(conf.numScheduleGenThreads)
    schedGenWorkers.tasksupport = new ForkJoinTaskSupport(pool)
    schedGenWorkers.map(f => f(()))
    pool.shutdown()
    return scheds.values.toSet
  }

  /**
    * Expands the given partial schedule (a schedule that has only as many
    * dimensions as are required to carry all dependences) to a full schedule
    * with d dimensions for d original iteration variables.
    *
    * @param maxNumRays maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumLines maximum number of lines per linear combination that forms a schedule coefficient vector
    *
    * @throws InterruptedException thrown if either the current thread has been
    * interrupted or if number of required Isl-operations exceeds the
    * configuration option {@code islComputeout}.
    */
  def expandToFullSchedule(conf : Config, sampler : SamplingStrategy, samplerParams : SamplingStrategyParams,
    sched : Schedule, linIndepSchedVectGen : (Schedule, SamplingStrategy, SamplingStrategyParams, Config) => Option[(List[Rat], Set[ScheduleSummand])]) : Schedule = {

    /*
     * Use an own ISL context in here in order to be able to work with
     * isl_ctx_set_max_operations on multiple threads. This requires the
     * transfer of any existing ISL that is used to the new context.
     */
    val tmpCtx : isl.Ctx = Isl.initCtx()
    val fullSchedTmpCtx : Schedule = sched.clone().transferToCtx(tmpCtx)

    var newSchedDim : Option[(List[Rat], Set[ScheduleSummand])] = null

    def computeNewSchedDim() {
      tmpCtx.setMaxOperations(conf.islComputeout)
      tmpCtx.resetOperations()
      newSchedDim = linIndepSchedVectGen(fullSchedTmpCtx, sampler, samplerParams, conf)
      tmpCtx.resetOperations()
      tmpCtx.setMaxOperations(0)
    }

    try {
      computeNewSchedDim()
      while (newSchedDim.isDefined) {
        if (Thread.interrupted())
          throw new InterruptedException()
        val coeffs : List[Rat] = newSchedDim.get._1
        val schedSummands : Set[ScheduleSummand] = newSchedDim.get._2
        fullSchedTmpCtx.addScheduleVector(coeffs, schedSummands)
        tmpCtx.resetOperations()
        computeNewSchedDim()
      }
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
    return fullSchedTmpCtx.transferToCtx(sched.domInfo.ctx, sched.deps,
      sched.domInfo)
  }

  /**
    * Compute a schedule coefficient vector that is linearly independent from
    * the coefficients of schedule {@code s}.
    *
    * @param maxNumRays maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumRays maximum number of lines per linear combination that forms a schedule coefficient vector
    *
    * @return Returns {@code None} if no such vector exists. Returns {@code Some(v)}
    * with {@code v} being a schedule coefficient vector that meets the criteria
    * described above.
    */
  def generateLinIndepScheduleVector(s : Schedule, sampler : SamplingStrategy, samplerParams : SamplingStrategyParams,
    conf : Config) : Option[(List[Rat], Set[ScheduleSummand])] = {
    return generateLinIndepScheduleVector1(s, s.numDims, sampler : SamplingStrategy, samplerParams : SamplingStrategyParams, conf)
  }

  /**
    * Compute a schedule coefficient vector that is linearly independent from
    * the coefficients of dimension 0 (inclusive) to {@code maxDim} (exclusive)
    * of schedule {@code s}.
    *
    * @param maxNumRays maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumLines maximum number of lines per linear combination that forms a schedule coefficient vector
    *
    * @return Returns {@code None} if no such vector exists. Returns {@code Some(v)}
    * with {@code v} being a schedule coefficient vector that meets the criteria
    * described above.
    */
  def generateLinIndepScheduleVector1(s : Schedule, maxDim : Int, sampler : SamplingStrategy,
    samplerParams : SamplingStrategyParams, conf : Config) : Option[(List[Rat], Set[ScheduleSummand])] = {
    if (maxDim < 0 || maxDim > s.numDims)
      throw new IllegalArgumentException("maxDim must be from the interval [0, "
        + s.numDims + "(: " + maxDim)
    val linIndepCoeffSpace : isl.Set = s.computeLinIndepSpace(maxDim, !conf.linIndepVectsDoNotFixDims) match {
      case None     => return None
      case Some(sp) => sp
    }
    if (linIndepCoeffSpace.isEmpty())
      return None

    // Randomly choose one basic set from linIndepCoeffSpace
    val linIndepBSets : isl.BasicSetList = linIndepCoeffSpace.getBasicSetList
    val randLinIndepBSet : isl.BasicSet = linIndepBSets
      .getBasicSet(Random.nextInt(linIndepBSets.nBasicSet()))

    val p : Polyhedron = sampler.preparePolyhedron(randLinIndepBSet, conf)
    return Some(sampler.sampleCoeffVect(p, s.domInfo, conf, samplerParams))
  }

  /**
    * Takes the schedule union map presentation {@code m} and converts it into
    * a coefficient matrix according to {@code domInfo}.
    */
  def islUnionMap2CoeffMatrix(
    domInfo : DomainCoeffInfo,
    m : isl.UnionMap) : List[Array[BigInt]] = {
    val matrix : ArrayBuffer[Array[BigInt]] = ArrayBuffer.empty
    val dimList : List[isl.UnionMap] = Isl.splitMultiDimUnionMap(m)
    // iterate over the schedule dimensions.
    for ((dimSched : isl.UnionMap, row : Int) <- dimList.zipWithIndex) {
      dimSched.foreachMap((m : isl.Map) => {
        val stmtId : String = m.getTupleName(T_IN)
        val sInfo : StmtCoeffInfo = domInfo.stmtInfo(stmtId)
        val stmtExp : isl.PwAff = isl.PwMultiAff.fromMap(m).getPwAff(0)
        val nCols : Int = domInfo.dim
        var nPieces : Int = 0
        stmtExp.foreachPiece((_ : isl.Set, p : isl.Aff) => {
          if (nPieces > 0)
            throw new IllegalArgumentException("Cannot process union maps that "
              + "result in a piecewise affine expression.")
          nPieces += 1
          Util.addValToMatrix(row, sInfo.cstIdx, nCols, matrix,
            p.getConstantVal)
          for (iterIdx <- 0 until sInfo.nrIt) {
            Util.addValToMatrix(row, sInfo.itStart + iterIdx, nCols, matrix,
              p.getCoefficientVal(T_IN, iterIdx))
          }
          for (paramIdx <- 0 until domInfo.nrParPS) {
            Util.addValToMatrix(row, sInfo.parStart + paramIdx, nCols, matrix,
              p.getCoefficientVal(T_PAR, paramIdx))
          }
        })
      })
    }
    return matrix.toList
  }

  /**
    * Converts a schedule coefficient matrix into a representation of the same
    * schedule as an Isl union map.
    */
  def coeffMatrix2IslUnionMap(domInfo : DomainCoeffInfo, coeffs : List[BigInt]*) : isl.UnionMap = {
    if (coeffs.isEmpty)
      return null

    val ctx : isl.Ctx = domInfo.ctx
    var schedule : isl.UnionMap = isl.UnionMap.empty(domInfo.scheduleParamSpace)
    implicit val bigint2Val = (i : BigInt) => isl.Val.fromBigInteger(ctx, i.bigInteger)

    val coeffsA = coeffs.view.map { coeff => coeff.toArray }

    for ((stmt, sInfo) <- domInfo.stmtInfo) {
      val setSpace : isl.Space = domInfo.scheduleParamSpace.addDims(T_SET, sInfo.nrIt)
      val mapSpace : isl.Space = domInfo.scheduleParamSpace.addDims(T_IN, sInfo.nrIt).addDims(T_OUT, coeffsA.size)
      var mAff = isl.MultiAff.zero(mapSpace)
      val lspace = isl.LocalSpace.fromSpace(setSpace)
      for ((coeff, dim) <- coeffsA.view.zipWithIndex) {
        var aff = isl.Aff.zeroOnDomain(lspace)
        for (i <- 0 until sInfo.nrIt)
          aff = aff.setCoefficientVal(isl.DimType.In, i, coeff(sInfo.itStart + i))
        for (i <- 0 until domInfo.nrParPS)
          aff = aff.setCoefficientVal(isl.DimType.Param, i, coeff(sInfo.parStart + i))
        aff = aff.setConstantVal(coeff(sInfo.cstIdx))
        mAff = mAff.setAff(dim, aff)
      }
      val stmtSchedule = isl.Map.fromMultiAff(mAff).setTupleName(isl.DimType.In, stmt)
      schedule = schedule.union(stmtSchedule)
    }

    return schedule
  }

  /**
    * Takes a schedule represented by an Isl union map. Calculates the set of
    * dependences that is newly carried by each dimension of the schedule.
    */
  def calcDependencesPartition(
    s : isl.UnionMap,
    deps : Set[Dependence]) : List[Set[Dependence]] = {
    val schedNumDims : Int = s.getSpace.dim(T_OUT)
    var dimScheds : List[isl.UnionMap] = Isl.splitMultiDimUnionMap(s)
    val alreadyCarriedDeps : HashSet[Dependence] = HashSet.empty

    for (dimSched : isl.UnionMap <- dimScheds) yield {
      val carriedDeps = getDepsCarriedBySchedule(dimSched, deps)
      val newlyCarried : Set[Dependence] = carriedDeps -- alreadyCarriedDeps
      newlyCarried.map(alreadyCarriedDeps.add)
      newlyCarried
    }
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

  def genCode(schedMap : isl.UnionMap, domain : isl.UnionSet) : String = {
    val ctx : isl.Ctx = schedMap.getCtx
    val astBuilder : isl.AstBuild = isl.AstBuild.fromContext(domain.params())
    val p : isl.Printer = astBuilder.astFromSchedule(schedMap.intersectDomain(domain)).print(isl.Printer.toStr(ctx), isl.AstPrintOptions.alloc(ctx))
    return p.getStr
  }

  def genCode(schedTree : isl.Schedule, domain : isl.UnionSet) : String = {
    val ctx : isl.Ctx = schedTree.getCtx
    val astBuilder : isl.AstBuild = isl.AstBuild.fromContext(domain.params())
    val p : isl.Printer = astBuilder.astFromSchedule(schedTree.getMap.intersectDomain(domain)).print(isl.Printer.toStr(ctx), isl.AstPrintOptions.alloc(ctx))
    return p.getStr
  }
}
