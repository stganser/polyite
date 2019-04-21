package polyite.evolution

import java.util.logging.Logger

import scala.Range
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.math.BigInt.int2bigInt
import scala.util.Random

import polyite.config.ConfigGA
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Schedule
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.ScheduleUtils
import polyite.schedule.ScheduleVectorUtils
import polyite.util.Rat
import polyite.util.Util

import isl.Isl
import isl.IslException
import polyite.ScopInfo
import polyite.schedule.sampling.Polyhedron
import polyite.schedule.sampling.ChernikovaSamplingStrategy
import polyite.schedule.sampling.ChernikovaSamplingStrategy.GeneratorsRat
import polyite.schedule.sampling.VertexSummand
import polyite.schedule.sampling.ScheduleSummand
import polyite.schedule.sampling.RaySummand
import polyite.schedule.sampling.LineSummand
import polyite.schedule.sampling.SamplingStrategy
import polyite.schedule.sampling.SamplingStrategyParams
import polyite.config.Config

/**
  * Schedule mutation strategies.
  */
object MutationStrategies {
  var myLogger : Logger = Logger.getLogger("")

  /**
    * Randomly replaces dimensions of the given schedule. The aggressiveness
    * depends on {@code conf.probabilityToMutateSchedRow} and decreases with
    * growing values of {@code generation} (simulated annealing).
    */
  def replaceDims(conf : ConfigGA, scop : ScopInfo, generation : Int, sampler : SamplingStrategy)(s : Schedule) : Option[Schedule] = {
    val tmpCtx : isl.Ctx = Isl.initCtx()
    val sTmpCtx : Schedule = s.transferToCtx(tmpCtx)
    val currProbabilityToMutateSchedRow : Double = annealMutationProbability(conf, generation, conf.probabilityToMutateSchedRow)
    val dims2Replace : Set[Int] = Random.shuffle((0 until sTmpCtx.numDims).toList).take((sTmpCtx.numDims * currProbabilityToMutateSchedRow).ceil.toInt).toSet
    val newSched : Schedule = sTmpCtx.clone()
    for (dim : Int <- 0 until newSched.numDims) {
      if (dims2Replace.contains(dim)) {
        tmpCtx.setMaxOperations(conf.islComputeout)
        val newScheduleDim : Option[(List[Rat], Set[ScheduleSummand])] =
          try {
            createNewScheduleVector(newSched, dim, conf, sampler)
          } catch {
            case e : IslException => {
              //              tmpCtx.resetError()
              if (tmpCtx.getRemainingOperations <= 0) {
                val msg : String = "The number of Isl operations for schedule space " +
                  "construction has reached islComputeout: " + conf.islComputeout
                myLogger.warning(msg)
                throw new InterruptedException(msg)
              } else
                throw e
            }
          }
        tmpCtx.resetOperations()
        tmpCtx.setMaxOperations(0)
        newScheduleDim match {
          case None => ()
          case Some((coeffs, schedSummands)) => {
            newSched.replaceScheduleVector(dim, coeffs, schedSummands)
          }
        }
      }
    }
    val newSchedSimplified : Schedule = ScheduleUtils.simplify(newSched.transferToCtx(s.domInfo.ctx))
    val samplerParams : SamplingStrategyParams = sampler.createSamplingStrategyParamsFromConf(conf)
    return Some(ScheduleUtils.expandToFullSchedule(conf, sampler, samplerParams, newSchedSimplified,
      ScheduleUtils.generateLinIndepScheduleVector))
  }

  private def createNewScheduleVector(s : Schedule, dim : Int,
    conf : ConfigGA, sampler : SamplingStrategy) : Option[(List[Rat], Set[ScheduleSummand])] = {
    val domInfo : DomainCoeffInfo = s.domInfo
    val carriedDeps : Set[Dependence] =
      if (dim == 0)
        Set.empty
      else
        s.getDependencesCarriedUpToDim(dim - 1)
    val uncarriedDeps : Set[Dependence] = s.deps -- carriedDeps
    val newlyCarriedDeps : Set[Dependence] = s.getDepsNewlyCarriedBy(dim)

    val samplerParams : SamplingStrategyParams = sampler.createSamplingStrategyParamsFromConf(conf)

    if (!(uncarriedDeps.isEmpty && newlyCarriedDeps.isEmpty)) {
      val (coeffSpace : isl.Set, _ : Set[Dependence]) = ScheduleSpaceUtils
        .calculateCoeffSpace(uncarriedDeps, newlyCarriedDeps, domInfo.universe)
      val p : Polyhedron = sampler.preparePolyhedron(coeffSpace, conf)

      return Some(sampler.sampleCoeffVect(p, s.domInfo, conf, samplerParams))
    } else {
      return ScheduleUtils.generateLinIndepScheduleVector1(s, dim, sampler, samplerParams, conf)
    }
  }

  /**
    * Replaces a block of dimensions in the given schedule. Simulated annealing controls the block size. At smallest
    * possible block size is two for schedules with at least two dimensions, in order to avoid convergence to dimension replacement.
    */
  def replaceBlocksOfDims(conf : ConfigGA, scop : ScopInfo, generation : Int, sampler : SamplingStrategy)(s : Schedule) : Option[Schedule] = {
    val tmpCtx : isl.Ctx = Isl.initCtx()
    val schedTmp : Schedule = s.transferToCtx(tmpCtx)
    var blockSize : Int = if (s.numDims > 1)
      (annealMutationProbability(conf, generation, conf.probabilityToMutateSchedRow) * (Random.nextInt(s.numDims - 1) + 1)).ceil.toInt
    else
      1
    if (blockSize >= s.numDims)
      blockSize -= 1
    if (blockSize <= 1 && s.numDims > 1)
      blockSize = 2
    else if (s.numDims == 1)
      blockSize = 1
    val fstDimIndex = Random.nextInt(s.numDims - blockSize + 1)

    val newSched : Schedule = new Schedule(schedTmp.domInfo, schedTmp.deps)

    for (d : Int <- 0 until fstDimIndex) {
      newSched.addForeignDim(schedTmp, d)
    }
    val deps2WeaklySolve : Set[Dependence] = schedTmp.deps -- (schedTmp.getDependencesCarriedUpToDim(fstDimIndex - 1))
    var deps2Carry : Set[Dependence] = Set.empty
    for (d : Int <- fstDimIndex until fstDimIndex + blockSize)
      deps2Carry ++= schedTmp.getDepsNewlyCarriedBy(d)
    val preparePolyhedron = (p : isl.Set, conf : Config) => sampler.preparePolyhedron(p, conf)
    val searchSpace : Iterable[Polyhedron] = ScheduleSpaceUtils.constructScheduleSpace(newSched, deps2Carry, preparePolyhedron, conf)
    val samplerParams : SamplingStrategyParams = sampler.createSamplingStrategyParamsFromConf(conf)

    for (p : Polyhedron <- searchSpace) {
      val (coeffs, schedSummands) = sampler.sampleCoeffVect(p, newSched.domInfo, conf, samplerParams)
      newSched.addScheduleVector(coeffs, schedSummands)
    }
    for (d <- fstDimIndex + blockSize until schedTmp.numDims) {
      newSched.addForeignDim(schedTmp, d)
    }
    val schedSimpl : Schedule = ScheduleUtils.simplify(newSched)
    val schedCompleted : Schedule = ScheduleUtils.expandToFullSchedule(conf, sampler, samplerParams, schedSimpl, ScheduleUtils.generateLinIndepScheduleVector)
    return Some(schedCompleted.transferToCtx(s.domInfo.ctx))
  }

  /**
    * Replaces the suffix of a given schedule. The number of preserved dimensions
    * is randomly chosen and increases with growing values of {@code generation}
    * (simulated annealing).
    */
  def replaceSuffix(conf : ConfigGA, scop : ScopInfo, generation : Int, sampler : SamplingStrategy)(s : Schedule) : Option[Schedule] = {
    if (s.numDims < 2)
      return None
    var numDims2Preserve : Int = s.numDims - (annealMutationProbability(conf, generation, conf.probabilityToMutateSchedRow) * (Random.nextInt(s.numDims - 1) + 1)).ceil.toInt
    if (numDims2Preserve >= s.numDims)
      numDims2Preserve -= 1
    val newSched : Schedule = new Schedule(s.domInfo, s.deps)
    for (i <- 0 until numDims2Preserve)
      newSched.addForeignDim(s, i)
    val schedSet : HashSet[Schedule] = ScheduleUtils.completeSchedule(newSched, conf.maxNumRays, conf.maxNumLines,
      conf, 1, newSched.deps.filterNot(newSched.getCarriedDeps.contains), sampler)

    // The completed schedule is a full schedule by definition.
    return Some(schedSet.head)
  }

  /**
    * Replaces the prefix of a given schedule. The number of preserved dimensions
    * is randomly chosen and increases with growing values of {@code generation}
    * (simulated annealing).
    */
  def replacePrefix(conf : ConfigGA, scop : ScopInfo, generation : Int, sampler : SamplingStrategy)(s : Schedule) : Option[Schedule] = {
    if (s.numDims < 2)
      return None
    var numDims2Replace : Int = (annealMutationProbability(conf, generation, conf.probabilityToMutateSchedRow) * (Random.nextInt(s.numDims - 1) + 1)).ceil.toInt
    if (numDims2Replace >= s.numDims)
      numDims2Replace -= 1

    val tmpCtx : isl.Ctx = Isl.initCtx()
    val sTmpCtx : Schedule = s.transferToCtx(tmpCtx)

    /*
     * replacePrefix degenerates to a full replacement of the schedule if
     * deps2CarryStrongly = s.deps!
     */
    val deps2CarryStrongly : Set[Dependence] = sTmpCtx.getDependencesCarriedUpToDim(numDims2Replace - 1)
    val samplerParams : SamplingStrategyParams = sampler.createSamplingStrategyParamsFromConf(conf)

    val newSched : Schedule =
      if (deps2CarryStrongly.isEmpty) {
        val prefixLength : Int = Random.nextInt(numDims2Replace + 1)
        val polyhedra : ListBuffer[Polyhedron] = ListBuffer.empty
        var uncarriedDeps : Set[Dependence] = sTmpCtx.deps
        for (i : Int <- 0 until prefixLength) {
          val coeffSpace : isl.Set = ScheduleSpaceUtils.calculateCoeffSpace(sTmpCtx.domInfo, uncarriedDeps, false)
          polyhedra.append(sampler.preparePolyhedron(coeffSpace, conf))
          uncarriedDeps = uncarriedDeps.filterNot(ScheduleSpaceUtils.checkCarried(sTmpCtx.domInfo, coeffSpace))
        }
        val tmp : Schedule = new Schedule(sTmpCtx.domInfo, sTmpCtx.deps)
        for (p : Polyhedron <- polyhedra) {
          val (coeffs : List[Rat], schedSummands : Set[ScheduleSummand]) = sampler.sampleCoeffVect(p, sTmpCtx.domInfo, conf,
            samplerParams)
          tmp.addScheduleVector(coeffs, schedSummands)
        }
        tmp
      } else {
        ScheduleUtils.completeSchedule(new Schedule(sTmpCtx.domInfo, sTmpCtx.deps), conf.maxNumRays, conf.maxNumLines,
          conf, 1, deps2CarryStrongly, sampler).head
      }

    for (i <- numDims2Replace until sTmpCtx.numDims)
      newSched.addForeignDim(sTmpCtx, i)
    val newSchedSimplified : Schedule = ScheduleUtils.simplify(newSched)
    val res = ScheduleUtils.expandToFullSchedule(conf, sampler, samplerParams, newSchedSimplified,
      ScheduleUtils.generateLinIndepScheduleVector)
    return Some(res.transferToCtx(s.domInfo.ctx))
  }

  //  private def annealMutationProbability(generation: Int, originalProbability: Double): Double =
  //    originalProbability / math.sqrt(generation)

  private def annealMutationProbability(conf : ConfigGA, generation : Int, originalProbability : Double) : Double = {
    if (conf.useConvexAnnealingFunction)
      return originalProbability * (-math.pow((generation - 1).toDouble / conf.maxGenerationToReach, 2.0) + 1)
    else
      return originalProbability / math.sqrt(math.log(generation + math.E - 1))
  }

  /**
    * Randomly mutates single dimensions of a given schedule by changing the
    * coefficients in the linear combination of Chernikova generators that
    * originally formed the coefficient vectors. In certain cases it becomes
    * necessary to replace the suffix of a mutated schedule with newly generated
    * suitable dimensions in order to preserve legality.
    */
  def mutateGeneratorCoeffs(conf : ConfigGA, scop : ScopInfo, generation : Int, sampler : SamplingStrategy)(s : Schedule) : Option[Schedule] = {

    if (sampler != ChernikovaSamplingStrategy)
      throw new IllegalStateException("Generator coefficient mutation only works if the sampling strategy relies on Chernikova.")

    val newSched : Schedule = new Schedule(s.domInfo, s.deps)
    val dimsIter : Iterator[Int] = Range(0, s.numDims).iterator
    val currProbabilityToMutateSchedRow : Double = annealMutationProbability(conf, generation,
      conf.probabilityToMutateSchedRow)
    val currProbabilityToMutateGeneratorCoeff : Double = annealMutationProbability(conf, generation,
      conf.probabilityToMutateSchedRow)

    val dims2Replace : Set[Int] = Random.shuffle((0 until s.numDims).toList).take((s.numDims * currProbabilityToMutateSchedRow).ceil.toInt).toSet

    while (dimsIter.hasNext && (newSched.getCarriedDeps.size < s.deps.size)) {
      val dim : Int = dimsIter.next
      val coeffs : List[Rat] = s.getScheduleVector(dim)
      val schedSummands : Set[ScheduleSummand] = s.getSchedSummands(dim)
      if (dims2Replace.contains(dim)) {
        val vertexSummands : ArrayBuffer[VertexSummand] = new ArrayBuffer()
        val lineSummands : ArrayBuffer[LineSummand] = new ArrayBuffer()
        val raySummands : ArrayBuffer[RaySummand] = new ArrayBuffer()

        schedSummands.map { s =>
          {
            s match {
              case s1 @ VertexSummand(_, _) => vertexSummands.append(s1)
              case s1 @ RaySummand(_, _)    => raySummands.append(s1)
              case s1 @ LineSummand(_, _)   => lineSummands.append(s1)
            }
          }
        }

        val schedSummandsNew : HashSet[ScheduleSummand] = HashSet.empty

        mutateVertexSummands(conf, schedSummandsNew, vertexSummands,
          currProbabilityToMutateGeneratorCoeff)
        mutateRaySummands(conf, schedSummandsNew, raySummands,
          currProbabilityToMutateGeneratorCoeff)
        mutateLineSummands(conf, schedSummandsNew, lineSummands,
          currProbabilityToMutateGeneratorCoeff)

        val zeroVect : List[Rat] = List.fill[Rat](coeffs.size)(Rat(0))
        val coeffsNew : List[Rat] = schedSummandsNew
          .foldLeft(zeroVect)((v : List[Rat], s : ScheduleSummand) => ScheduleVectorUtils.add(v, s.v, s.coeff))
        newSched.addScheduleVector(coeffsNew, schedSummandsNew.toSet)
      } else {
        newSched.addForeignDim(s, dim)
      }
      val prevUncarried : Set[Dependence] =
        if (newSched.numDims == 1)
          Set.empty[Dependence]
        else
          newSched.deps -- newSched.getDependencesCarriedUpToDim(newSched.numDims - 2)

      /*
       * Given is a schedule s. s is the result of replaceSuffix for another
       * schedule t. When building s, replaceSuffix has preserved the first i
       * dimensions of t. Data dependency d is carried by dimension i of s and t
       * by coincidence but not by construction. I.e. it cannot be guaranteed,
       * that any linear combination of the generators of dimension i results in
       * a schedule carrying d. Therefore if dimensions i and (i + 1) are both
       * modified by muatateGeneratorCoeffs it can happen that the new dimension
       * i does no longer carry d strongly and, as i + 1 is not required to
       * carry d as well, i + 1 does not even carry d weakly. Therefore the new
       * dimension i + 1 is invalid.
       */
      if (!prevUncarried.forall(newSched.getDependencesSatisfiedWeaklyByDim(newSched.numDims - 1).contains)) {
        myLogger.warning("Generator mutation produced an invalid schedule vector.")
        newSched.removeLastScheduleVector
        // The completed schedule is a full schedule by definition.
        return Some(ScheduleUtils.completeSchedule(newSched, conf.maxNumRays, conf.maxNumLines, conf, 1, prevUncarried, sampler).head)
      }
    }
    val stillUncarried : Set[Dependence] = newSched.deps -- newSched.getCarriedDeps
    // The completed schedule is a full schedule by definition.
    if (!stillUncarried.isEmpty)
      return Some(ScheduleUtils.completeSchedule(newSched, conf.maxNumRays, conf.maxNumLines, conf, 1, stillUncarried, sampler).head)
    val samplerParams : SamplingStrategyParams = sampler.createSamplingStrategyParamsFromConf(conf)
    val schedSimpl : Schedule = ScheduleUtils.simplify(newSched)
    return Some(ScheduleUtils.expandToFullSchedule(conf, sampler, samplerParams, schedSimpl,
      ScheduleUtils.generateLinIndepScheduleVector))
  }

  private def mutateVertexSummands(
    conf : ConfigGA,
    schedSummandsNew : HashSet[ScheduleSummand],
    vertexSummands : ArrayBuffer[VertexSummand], p : Double) {

    val summandsToMutate : Set[Int] = Random.shuffle((0 until vertexSummands.size).toList.take((vertexSummands.size * p).ceil.toInt)).toSet

    // The sum of the new coefficients must be 1!
    if (vertexSummands.size > 1)
      for (i : Int <- 0 until vertexSummands.size) {
        if (summandsToMutate.contains(i)) {
          val vOld : VertexSummand = vertexSummands(i)
          val cNew : Rat = getRandomCoeff(Rat(1, conf.generatorCoeffMaxDenominator), Rat(1), conf)
          vertexSummands(i) = new VertexSummand(vOld.v, cNew)
          var diff : Rat = Rat(1) - cNew
          for (j : Int <- 0 until vertexSummands.size) {
            if (j != i) {
              val vOld1 : VertexSummand = vertexSummands(j)
              val cNew1 : Rat =
                if ((j == vertexSummands.size - 1) || (i == vertexSummands.size - 1 && j == i - 1))
                  diff
                else {
                  val tmp : Rat = getRandomCoeff(Rat(0), diff, conf)
                  diff -= tmp
                  tmp
                }
              vertexSummands(j) = new VertexSummand(vOld1.v, cNew1)
            }
          }
        }
      }
    vertexSummands map { (s : ScheduleSummand) => schedSummandsNew.add(s) }
  }

  private def mutateRaySummands(
    conf : ConfigGA,
    schedSummandsNew : HashSet[ScheduleSummand],
    raySummands : ArrayBuffer[RaySummand], p : Double) {

    val summandsToMutate : Set[Int] = Random.shuffle((0 until raySummands.size).toList.take((raySummands.size * p).ceil.toInt)).toSet

    for (i : Int <- 0 until raySummands.size) {
      schedSummandsNew.add(if (summandsToMutate.contains(i)) {
        val cNew : Rat = getRandomCoeff(Rat(0), Rat(conf.rayCoeffsRange), conf)
        new RaySummand(raySummands(i).v, cNew)
      } else
        raySummands(i))
    }
  }

  private def mutateLineSummands(
    conf : ConfigGA,
    schedSummandsNew : HashSet[ScheduleSummand],
    lineSummands : ArrayBuffer[LineSummand], p : Double) {

    val summandsToMutate : Set[Int] = Random.shuffle((0 until lineSummands.size).toList.take((lineSummands.size * p).ceil.toInt)).toSet

    for (i : Int <- 0 until lineSummands.size) {
      schedSummandsNew.add(if (summandsToMutate.contains(i)) {
        val cNew : Rat = getRandomCoeff(Rat(-conf.lineCoeffsRange), Rat(conf.lineCoeffsRange), conf)
        new LineSummand(lineSummands(i).v, cNew)
      } else
        lineSummands(i))
    }
  }

  private def getRandomCoeff(min : Rat, max : Rat, conf : ConfigGA) : Rat = Rat
    .getRandomRat(min, max, conf.generatorCoeffMaxDenominator)
}