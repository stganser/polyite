package polyite.schedule

import scala.math.BigInt.int2bigInt
import isl.Conversions._
import isl.VoidCallback1
import isl.VoidCallback2
import isl.Isl.TypeAliases._
import polyite.util.Util.vec2List
import org.exastencils.schedopt.chernikova.Generators
import org.exastencils.schedopt.chernikova.Chernikova
import scala.collection.parallel.mutable.ParArray
import polyite.util.Rat
import isl.Isl
import scala.collection.mutable.ListBuffer
import isl.IslException
import scala.util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import java.util.logging.Logger
import polyite.util.Util
import polyite.config.Config
import polyite.config.MinimalConfig
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import polyite.config.ConfigGA
import polyite.config.MinimalConfig.NumGeneratorsLimit
import java.math.BigInteger
import polyite.util.Util.GeneratorsRat
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.config.MinimalConfig.RandLimit
import polyite.config.MinimalConfig.LimitedGenerators

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
  def getDepsCarriedBySchedule(s : isl.UnionMap,
    deps : Set[Dependence]) : Set[Dependence] = {
    return deps.filter { d => getDirectionOfDep(s, d).isPositiveOnly }
  }

  /**
    * Filters {@code deps} for dependencies that are carried at least weakly by
    * the schedule given through the union map {@code s}.
    */
  def getDepsCarriedWeaklyBySchedule(s : isl.UnionMap,
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
          val fstSttmntConstCoeffs : List[Rat] = getConstCoeffsForSttmnt(v,
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
    * Generates a coefficient for a Chernikova line generator. The coefficient
    * will be an integer value from the set {@code [minLineCoeff, maxLineCoeff] \\ {0}}.
    */
  def getRandLineCoeff(minLineCoeff : Int, maxLineCoeff : Int) : Rat = {
    if (minLineCoeff > maxLineCoeff)
      throw new IllegalArgumentException("minLineCoeff > maxLineCoeff: " + minLineCoeff + " > " + maxLineCoeff)

    if (minLineCoeff == maxLineCoeff)
      return Rat(minLineCoeff)
    var c : Int = 0
    while (c == 0)
      c = Random.nextInt(maxLineCoeff - minLineCoeff + 1) + minLineCoeff
    return Rat(c)
  }

  /**
    * Generates a coefficient for a Chernikova ray generator. The coefficient
    * will be an integer value from the range (0, maxRayCoeff].
    */
  def getRandRayCoeff(maxRayCoeff : Int) : Rat =
    Rat(Random.nextInt(maxRayCoeff + 1) + 1)

  /**
    * Generates a schedule coefficient vector from the given set of Chernikova
    * generators.
    * @param maxRayCoeff Coefficients for rays are selected from the interval {@code (0, maxRayCoeff]}.
    * @param minLineCoeff Coefficients for lines are selected from the set {@code [minLineCoeff, maxLineCoeff] \\ {0}}.
    * @param maxLineCoeff Coefficients for lines are selected from the set {@code [minLineCoeff, maxLineCoeff] \\ {0}}.
    * @param maxNumRays maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumRays maximum number of lines per linear combination that forms a schedule coefficient vector
    */
  def generateScheduleVector(g : GeneratorsRat, maxRayCoeff : Int, minLineCoeff : Int, maxLineCoeff : Int,
    maxNumRays : NumGeneratorsLimit, maxNumLines : NumGeneratorsLimit, conf : Config) : (List[Rat], Set[ScheduleSummand]) = {

    val verticesRand : List[List[Rat]] = Random.shuffle(g.vertices)

    val raysRand : List[List[Rat]] = Random.shuffle(g.rays)
    val linesRand : List[List[Rat]] = Random.shuffle(g.lines)

    val numRays = maxNumRays match {
      case MinimalConfig.AllGenerators =>
        Random.nextInt(raysRand.size + 1)
      case MinimalConfig.LimitedGenerators(n) =>
        Random.nextInt(math.min(n, raysRand.size) + 1)
      case MinimalConfig.RandLimit =>
        Random.nextInt(raysRand.size + 1)
    }

    val numLines = maxNumLines match {
      case MinimalConfig.AllGenerators =>
        Random.nextInt(linesRand.size + 1)
      case MinimalConfig.LimitedGenerators(n) =>
        Random.nextInt(math.min(n, linesRand.size) + 1)
      case MinimalConfig.RandLimit =>
        Random.nextInt(linesRand.size + 1)
    }
    val v = verticesRand.head
    val chosenRays = raysRand.take(numRays)
    val chosenLines = linesRand.take(numLines)
    val schedSummands : HashSet[ScheduleSummand] = HashSet.empty

    var coeffs : List[Rat] = v
    schedSummands.add(new VertexSummand(v, Rat(BigInt(1))))
    for (r : List[Rat] <- chosenRays) {
      val c : Rat = getRandRayCoeff(maxRayCoeff)
      coeffs = ScheduleVectorUtils.add(coeffs, r, c)
      schedSummands.add(new RaySummand(r, c))
    }
    for (l : List[Rat] <- chosenLines) {
      val c : Rat = getRandLineCoeff(minLineCoeff, maxLineCoeff)
      coeffs = ScheduleVectorUtils.add(coeffs, l, c)
      schedSummands.add(new LineSummand(l, c))
    }
    return (coeffs, schedSummands.toSet)
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
    conf : Config, numCompletions : Int, deps2CarryStrongly : Set[Dependence]) : HashSet[Schedule] = {

    // Construct the schedule space
    val scheduleSpaceGenerators : Iterable[GeneratorsRat] = ScheduleSpaceUtils.constructScheduleSpace(sched, deps2CarryStrongly, conf)

    // Find out the maximum number of generators of all dimensions
    val maxNumRaysAllDims : Int = ScheduleSpaceUtils.getMaxNumRays(scheduleSpaceGenerators)
    val maxNumLinesAllDims : Int = ScheduleSpaceUtils.getMaxNumLines(scheduleSpaceGenerators)

    // Extract as many schedules from the above constructed schedule space as
    // required.
    var result : HashSet[Schedule] = HashSet.empty
    for (i <- 0 until numCompletions) {

      val currNumRaysLimit : NumGeneratorsLimit = numRaysLimit match {
        case RandLimit => LimitedGenerators(Random.nextInt(maxNumRaysAllDims) + 1)
        case _         => numRaysLimit
      }
      val currNumLinesLimit : NumGeneratorsLimit = numLinesLimit match {
        case RandLimit => LimitedGenerators(Random.nextInt(maxNumLinesAllDims) + 1)
        case _         => numLinesLimit
      }

      val newSched : Schedule = sched.clone
      val gIter : Iterator[GeneratorsRat] = scheduleSpaceGenerators.iterator
      while (gIter.hasNext && newSched.getCarriedDeps.size < newSched.deps.size) {
        val (coeffs : List[Rat], schedSummands : Set[ScheduleSummand]) = generateScheduleVector(gIter.next(),
          conf.rayCoeffsRange, -conf.lineCoeffsRange, conf.lineCoeffsRange, currNumRaysLimit, currNumLinesLimit, conf)
        newSched.addScheduleVector(coeffs, schedSummands)
      }
      val schedCleaned : Schedule = simplify(newSched)

      /*
       * complete the schedule to a full schedule if it carries all dependences.
       * Otherwise we were asked to generate a schedule prefix.
       */
      if (schedCleaned.deps.size == (schedCleaned.getCarriedDeps.size)) {
        val fullSched : Schedule = expandToFullSchedule(conf, currNumRaysLimit, currNumLinesLimit, schedCleaned,
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
    */
  def genRandSchedules(domInfo : DomainCoeffInfo, deps : Set[Dependence], maxNumScheds : Int,
    maxNumRays : NumGeneratorsLimit, maxNumLines : NumGeneratorsLimit, conf : Config) : Set[Schedule] =
    genRandSchedules(domInfo, deps, maxNumScheds, Set.empty, maxNumRays, maxNumLines, conf)

  /**
    * Generates a set of random schedules. Any schedules contained by {@code basis}
    * are also part of the result. The result contains at most
    * max(|basis|, maxNumScheds) schedules.
    *
    * @param maxNumRays maximum number of rays per linear combination that forms a schedule coefficient vector
    * @param maxNumRays maximum number of lines per linear combination that forms a schedule coefficient vector
    */
  def genRandSchedules(domInfo : DomainCoeffInfo,
    deps : Set[Dependence], maxNumScheds : Int, basis : Set[Schedule], maxNumRays : NumGeneratorsLimit,
    maxNumLines : NumGeneratorsLimit, conf : Config) : Set[Schedule] = {
    val scheds : HashSet[Schedule] = HashSet.empty
    basis.map(scheds.add)

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
          math.min(Random.nextInt(conf.maxNumSchedsAtOnce + 1) + 1,
            maxNumScheds - scheds.size)
        }
        val args : Array[Any] = Array(new Schedule(domInfo, deps), maxNumRays, maxNumLines, conf, numSchedsAtOnce, deps)
        val schedsMaybe : Option[Iterable[Schedule]] = Util.runWithTimeout(args,
          ((args : Array[Any]) => ScheduleUtils.completeSchedule(
            args(0).asInstanceOf[Schedule], args(1).asInstanceOf[NumGeneratorsLimit],
            args(2).asInstanceOf[NumGeneratorsLimit], args(3).asInstanceOf[Config], args(4).asInstanceOf[Int],
            args(5).asInstanceOf[Set[Dependence]])), conf.randSchedsTimeout * 1000)
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
                  if (scheds.size < maxNumScheds)
                    if (scheds.add(s)) {
                      myLogger.info("(schedule gen worker #" + idx
                        + ")New random schedule: " + s.toString())
                      addedAny = true
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
    schedGenWorkers.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(conf.numScheduleGenThreads))
    schedGenWorkers.map(f => f(()))
    return scheds.toSet
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
  def expandToFullSchedule(conf : Config, maxNumRays : NumGeneratorsLimit,
    maxNumLines : NumGeneratorsLimit, sched : Schedule, linIndepSchedVectGen : (Schedule, NumGeneratorsLimit, NumGeneratorsLimit, Config) => Option[(List[Rat], Set[ScheduleSummand])]) : Schedule = {

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
      newSchedDim = linIndepSchedVectGen(fullSchedTmpCtx, maxNumRays, maxNumLines, conf)
      tmpCtx.resetOperations()
      tmpCtx.setMaxOperations(0)
    }

    try {
      computeNewSchedDim()
      while (newSchedDim.isDefined) {
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
  def generateLinIndepScheduleVector(s : Schedule, maxNumRays : NumGeneratorsLimit,
    maxNumLines : NumGeneratorsLimit, conf : Config) : Option[(List[Rat], Set[ScheduleSummand])] = {
    return generateLinIndepScheduleVector1(s, s.numDims, maxNumRays, maxNumLines, conf)
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
  def generateLinIndepScheduleVector1(s : Schedule, maxDim : Int, maxNumRays : NumGeneratorsLimit,
    maxNumLines : NumGeneratorsLimit, conf : Config) : Option[(List[Rat], Set[ScheduleSummand])] = {
    if (maxDim < 0 || maxDim > s.numDims)
      throw new IllegalArgumentException("maxDim must be from the interval [0, "
        + s.numDims + "(: " + maxDim)
    val linIndepCoeffSpace : isl.Set = s.computeLinIndepSpace(maxDim, !conf.linIndepVectsDoNotFixDims) match {
      case None     => return None
      case Some(sp) => sp
    }
    if (linIndepCoeffSpace.isEmpty())
      throw new IllegalStateException("The coefficient space is empty: " + linIndepCoeffSpace)

    // Randomly choose one basic set from linIndepCoeffSpace
    val linIndepBSets : isl.BasicSetList = linIndepCoeffSpace.getBasicSetList
    val randLinIndepBSet : isl.BasicSet = linIndepBSets
      .getBasicSet(Random.nextInt(linIndepBSets.nBasicSet()))

    val g : GeneratorsRat = Util.constraints2GeneratorsRat(randLinIndepBSet, conf.moveVertices, conf.rayPruningThreshold)
    return Some(generateScheduleVector(g, conf.rayCoeffsRange, -conf.lineCoeffsRange, conf.lineCoeffsRange, maxNumRays,
      maxNumLines, conf))
  }

  /**
    * Takes the schedule union map presentation {@code m} and converts it into
    * a coefficient matrix according to {@code domInfo}.
    */
  def islUnionMap2CoeffMatrix(domInfo : DomainCoeffInfo,
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
      schedule = if (schedule == null) stmtSchedule else schedule.union(stmtSchedule)
    }

    return schedule
  }

  def getGeneratorCoeffs(g : Generators, v : List[BigInt],
    ctx : isl.Ctx) : (isl.Set, Generators2CoeffIndices) = {
    val vertices : List[List[BigInt]] = g.vertices.map(t => {
      if ((!t._2.isValidInt && t._2.intValue() == 0))
        throw new IllegalArgumentException("Cannot handle rational numbers.")
      t._1.toList
    }).toList
    val rays : List[List[BigInt]] = g.rays.map(vec2List).toList
    val lines : List[List[BigInt]] = g.lines.map(vec2List).toList

    val generatorsWithIdx : List[(Array[BigInt], Int)] = (vertices ++ rays ++ lines)
      .map(_.toArray).zipWithIndex

    /*
     * build an Isl set (LP) that contains all coefficients for convex linear
     * combinations of generators from g that form v.
     */
    val nVCoeffs : Int = vertices.size
    val vCoeffsStart : Int = 0
    val nRCoeffs : Int = rays.size
    val rCoeffsStart = nVCoeffs
    val nLCoeffs : Int = lines.size
    val lCoeffsStart = nVCoeffs + nRCoeffs

    val nCoeffs : Int = nVCoeffs + nRCoeffs + nLCoeffs

    var validChernikovaCoeffs : isl.Set = isl.Set.universe(isl.Space
      .setAlloc(ctx, 0, nCoeffs))
    val localSpace : isl.LocalSpace = isl.LocalSpace.fromSpace(validChernikovaCoeffs.getSpace)
    // Construct the constraints for the basic linear combination
    for (dim <- 0 until v.size) {
      var dimConstraint : isl.Constraint = isl.Constraint.allocEquality(localSpace)
      for ((g, idx) <- generatorsWithIdx) {
        dimConstraint = dimConstraint.setCoefficientVal(T_SET, idx,
          isl.Val.fromBigInteger(ctx, g(dim).bigInteger))
      }
      dimConstraint = dimConstraint.setConstantVal(isl.Val.fromBigInteger(ctx,
        v(dim).bigInteger.negate()))
      validChernikovaCoeffs = validChernikovaCoeffs.addConstraint(dimConstraint)
    }

    def makeVarsPos(min : Int, nVars : Int) {
      for (vIdx <- min until min + nVars) {
        var constr = isl.Constraint.allocInequality(localSpace)
        constr = constr.setCoefficientSi(T_SET, vIdx, 1)
        validChernikovaCoeffs = validChernikovaCoeffs.addConstraint(constr)
      }
    }

    // The coefficients of rays must be positive.
    makeVarsPos(rCoeffsStart, nRCoeffs)

    /* 
     * The coefficients of vertices must be positive and sum up to 1. Otherwise
     * the linear combination isn't convex.
     */
    makeVarsPos(vCoeffsStart, nVCoeffs)
    var coeffSumConstr : isl.Constraint = isl.Constraint.allocEquality(localSpace)
    coeffSumConstr = coeffSumConstr.setConstantSi(-1)
    for (vIdx <- vCoeffsStart until vCoeffsStart + nVCoeffs)
      coeffSumConstr = coeffSumConstr.setCoefficientSi(T_SET, vIdx, 1)
    validChernikovaCoeffs = validChernikovaCoeffs.addConstraint(coeffSumConstr)

    // Construct a mapping from generators to coefficient indices.
    val vertexCoeffsMapping : HashMap[List[BigInt], Int] = HashMap.empty
    for ((vertex, idx) <- vertices.zipWithIndex)
      vertexCoeffsMapping.put(vertex, idx + vCoeffsStart)

    val rayCoeffsMapping : HashMap[List[BigInt], Int] = HashMap.empty
    for ((ray, idx) <- rays.zipWithIndex)
      rayCoeffsMapping.put(ray, idx + rCoeffsStart)

    val lineCoeffsMapping : HashMap[List[BigInt], Int] = HashMap.empty
    for ((line, idx) <- lines.zipWithIndex)
      lineCoeffsMapping.put(line, idx + lCoeffsStart)

    return (Isl.simplify(validChernikovaCoeffs),
      Generators2CoeffIndices(
        vertices,
        rays,
        lines,
        vCoeffsStart,
        rCoeffsStart,
        lCoeffsStart,
        nVCoeffs,
        nRCoeffs,
        nLCoeffs))
  }

  case class Generators2CoeffIndices(
    verticesOrder : List[List[BigInt]],
    raysOrder : List[List[BigInt]],
    linesOrder : List[List[BigInt]],
    verticesStart : Int,
    raysStart : Int,
    linesStart : Int,
    nVertices : Int,
    nRays : Int,
    nLines : Int)

  /**
    * Takes a schedule represented by an Isl union map. Calculates the set of
    * dependences that is newly carried by each dimension of the schedule.
    */
  def calcDependencesPartition(s : isl.UnionMap,
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
