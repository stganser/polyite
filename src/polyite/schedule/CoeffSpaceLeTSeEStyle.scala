package polyite.schedule

import java.util.logging.Logger

import scala.collection.mutable.BitSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.forkjoin.ForkJoinPool
import scala.math.BigInt.int2bigInt
import scala.util.Random

import isl.Isl
import isl.Isl.TypeAliases.T_SET
import polyite.ScopInfo
import polyite.config.Config
import polyite.config.ConfigRandLeTSeEStyle
import polyite.config.MinimalConfig.LimitedGenerators
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.config.MinimalConfig.RandLimit
import polyite.schedule.sampling.ChernikovaSamplingStrategy.ChernikovaParams
import polyite.schedule.hash.ScheduleHash
import polyite.util.Rat
import polyite.util.Util
import polyite.schedule.sampling.ChernikovaSamplingStrategy.GeneratorsRat
import polyite.schedule.sampling.ScheduleSummand
import polyite.schedule.sampling.SamplingStrategyParams
import polyite.schedule.sampling.ChernikovaSamplingStrategy
import polyite.schedule.sampling.SamplingStrategy
import polyite.config.MinimalConfig

/**
  * Generates random schedules in LeTSeE style. Each dependence is carried by
  * every schedule on the same dimension.
  */
object CoeffSpaceLeTSeEStyle {

  val myLogger : Logger = Logger.getLogger("")

  /**
    * Generates random schedules from the given {@code DomainCoeffInfo} and SCoP
    * that carry all data dependences in {@code deps} in a way similar to LeTSeE.
    * @return A set containing at most {@code maxNumSchedules} schedules. Any
    * schedules contained by {@code basis} are also part of the result. The
    * result contains at most max(|basis|, maxNumScheds) schedules. If the search space
    * is empty, {@code None} is returned.
    * @param domInfo models the schedule coefficient vector space
    * @param deps data dependences of the program to optimize
    * @param conf configuration properties
    * @param hashSched this function determines the criterion according to which two schedules are equivalent.
    */
  def genRandSchedules(scop : ScopInfo)(
    domInfo : DomainCoeffInfo,
    deps : Set[Dependence], maxNumSchedules : Int, basis : Set[Schedule],
    conf : ConfigRandLeTSeEStyle,
    hashSched : Schedule => ScheduleHash) : Option[Set[Schedule]] = {
    myLogger.info("Constructing the schedule space.")
    val schedSpace : List[isl.Set] = constructScheduleSpace(deps, conf, domInfo, scop) match {
      case None     => return None
      case Some(sp) => sp
    }
    val schedSpaceDual : List[GeneratorsRat] = schedSpace.zipWithIndex.map((t : (isl.Set, Int)) => {
      myLogger.info("dualizing dimension polyhedron " + t._2)
      ChernikovaSamplingStrategy.constraints2GeneratorsRat(t._1)
    })
    myLogger.info("Starting to generate " + maxNumSchedules + " random schedules.")

    if (conf.moveVertices)
      return Some(genSchedsFromCoeffSpace(domInfo, deps, moveVertices(schedSpaceDual, schedSpace), conf, maxNumSchedules, basis, hashSched))
    else
      return Some(genSchedsFromCoeffSpace(domInfo, deps, schedSpaceDual, conf, maxNumSchedules, basis, hashSched))
  }

  def moveVertices(gs : List[GeneratorsRat], constraints : List[isl.Set]) : List[GeneratorsRat] = {
    val allInteger : List[GeneratorsRat] = gs.zip(constraints).map(t => ChernikovaSamplingStrategy.moveVertices(t._1, t._2))
    var result : List[GeneratorsRat] = List.empty

    for ((g, p) <- allInteger.zip(constraints)) {
      val pBounded : isl.Set = Isl.boundDims(p, -1, 1)
      var verticesNew : List[List[Rat]] = List.empty
      for (v <- g.vertices) {
        var unsuitableDims : List[Int] = List.empty
        for ((c, i) <- v.zipWithIndex) {
          if (c.abs > Rat(1))
            unsuitableDims ::= i
        }
        if (unsuitableDims.isEmpty)
          verticesNew ::= v
        else {
          var pCurr : isl.Set = pBounded
          val dims2Fix : List[Int] = Random.shuffle(((0 until pCurr.dim(T_SET)).toSet -- unsuitableDims).toList)
          for (dim <- dims2Fix) {
            val withDimFixed = pCurr.fixVal(T_SET, dim, isl.Val.fromBigInteger(pCurr.getCtx, v(dim).getIntegerValue.bigInteger))

            if (!withDimFixed.isEmpty())
              pCurr = withDimFixed
          }
          verticesNew ::= Util.islPoint2RatList(pCurr.samplePoint())
        }
      }
      result ::= GeneratorsRat(verticesNew, g.rays, g.lines)
    }
    return result.reverse
  }

  private def preprocess(g : GeneratorsRat) : GeneratorsRat = {

    var xorSum : BitSet = new BitSet(g.vertices.head.length)

    implicit def toBitSet(v : List[Rat]) : BitSet = {
      var result : BitSet = new BitSet(v.length)
      for ((c, idx) <- v.zipWithIndex) {
        if (c != Rat(0))
          result.add(idx)
      }
      return result
    }

    for (v <- g.vertices) {
      xorSum ^= v
    }
    for (v <- g.rays) {
      xorSum ^= v
    }
    for (v <- g.lines) {
      xorSum ^= v
    }

    def isUsableGenerator(v : List[Rat]) : Boolean = {
      for ((c, idx) <- v.zipWithIndex) {
        if (xorSum(idx) && c != Rat(-1) && c != Rat(0) && c != Rat(1)) {
          return false
        }
      }
      return true
    }

    //    val verticesFiltered : List[List[Rat]] = gp.vertices.filter(isUsableGenerator)
    val raysFiltered : List[List[Rat]] = g.rays.filter(isUsableGenerator)
    val linesFiltered : List[List[Rat]] = g.lines.filter(isUsableGenerator)

    //    if (verticesFiltered.isEmpty) {
    //      myLogger.warning("No vertex is suitable for LeTSeE style sampling.")
    //      return None
    //    } else
    return GeneratorsRat(g.vertices, raysFiltered, linesFiltered)
  }

  private def genSchedsFromCoeffSpace(
    domInfo : DomainCoeffInfo,
    deps : Set[Dependence], generators : List[GeneratorsRat], conf : ConfigRandLeTSeEStyle,
    maxNumScheds : Int, basis : Set[Schedule], hashSched : Schedule => ScheduleHash) : Set[Schedule] = {
    val result : HashMap[ScheduleHash, Schedule] = HashMap.empty
    basis.foreach((s : Schedule) => result.put(hashSched(s), s))

    // Find out the maximum number of generators of all dimensions
    val maxNumRaysAllDims : Int = ChernikovaSamplingStrategy.getMaxNumRays(generators)
    val maxNumLinesAllDims : Int = ChernikovaSamplingStrategy.getMaxNumLines(generators)

    def genScheds(idx : Int)(x : Unit) {

      var numConseqFailures : Int = 0

      def continue : Boolean = {
        result.synchronized {
          var cancel : Boolean = false
          if (numConseqFailures >= conf.genSchedsMaxAllowedConseqFailures)
            cancel = true
          return !cancel && result.size < maxNumScheds
        }
      }

      while (continue) {
        val arguments : Array[Any] = Array()
        val newSchedMaybe : Option[Schedule] = Util.runWithTimeout(arguments, (args : Array[Any]) => {

          val currNumRaysLimit : NumGeneratorsLimit = conf.maxNumRays match {
            case RandLimit => LimitedGenerators(Random.nextInt(maxNumRaysAllDims) + 1)
            case _         => conf.maxNumRays
          }
          val currNumLinesLimit : NumGeneratorsLimit = conf.maxNumLines match {
            case RandLimit => LimitedGenerators(Random.nextInt(maxNumLinesAllDims) + 1)
            case _         => conf.maxNumLines
          }

          var newSchedInner : Schedule = new Schedule(domInfo, deps)

          // As long as the generated coefficient vector contains values that are not from {-1, 0, 1}, build a new vector.
          for ((gs : GeneratorsRat, dim : Int) <- generators.zipWithIndex) {
            var (coeffs : List[Rat], schedSummands : Set[ScheduleSummand]) = ChernikovaSamplingStrategy.generateScheduleVector(gs, 1, -1, 1,
              currNumRaysLimit, currNumLinesLimit, conf)

            /*
           * Rejection sampling: As long as coeffs contains coefficients that are not in {-1, 0, 1}, replace the vector.
           * May be disabled by configuration.
           */

            while (conf.boundSchedCoeffs && !hasSuitableCoeffValues(coeffs)) {
              val (coeffs1, schedSummands1) = ChernikovaSamplingStrategy.generateScheduleVector(gs, 1, -1, 1, currNumRaysLimit,
                currNumLinesLimit, conf)
              coeffs = coeffs1
              schedSummands = schedSummands1
            }
            newSchedInner.addScheduleVector(coeffs, schedSummands)
          }

          if (conf.completeSchedules) {
            try {
              val samplerParams : ChernikovaParams = ChernikovaParams(currNumRaysLimit, currNumLinesLimit)
              newSchedInner = ScheduleUtils.expandToFullSchedule(conf, ChernikovaSamplingStrategy, samplerParams,
                newSchedInner, generateLinIndepScheduleVector)
            } catch {
              case t : Throwable => {
                println(t.getClass.getName)
                throw t
              }
            }
          }
          newSchedInner
        }, conf.randSchedsTimeout * 1000)

        var addedSomething : Boolean = false
        if (newSchedMaybe.isDefined) {
          result.synchronized {
            if (result.size < maxNumScheds) {
              ScheduleUtils.assertValid(newSchedMaybe.get)
              val h : ScheduleHash = hashSched(newSchedMaybe.get)
              if (!result.contains(h)) {
                result.put(h, newSchedMaybe.get)
                numConseqFailures = 0
                myLogger.info("(schedule gen worker #" + idx
                  + ")New random schedule: " + newSchedMaybe.get)
                addedSomething = true
              }
            }
          }
        } else {
          myLogger.warning("(schedule gen worker #" + idx
            + ") Timeout of schedule completion.")
        }
        if (!addedSomething)
          numConseqFailures += 1
      }
    }

    val schedGenWorkers : ParArray[Unit => Unit] = new ParArray(conf.numScheduleGenThreads)
    for (i <- 0 until schedGenWorkers.length)
      schedGenWorkers(i) = genScheds(i)
    val pool = new ForkJoinPool(conf.numScheduleGenThreads)
    schedGenWorkers.tasksupport = new ForkJoinTaskSupport(pool)
    schedGenWorkers.map(f => f(()))
    pool.shutdown()

    if (result.size < maxNumScheds)
      myLogger.warning("Failed to produce as many schedules as requested: " + result.size)
    return result.values.toSet
  }

  /**
    * Check whether the given schedule coefficient vector contains only values from {@code {-1, 0, 1}}.
    */
  private def hasSuitableCoeffValues(schedCoeffVect : List[Rat]) : Boolean = {
    for (c : Rat <- schedCoeffVect) {
      if (c.abs > Rat(1) || !c.isInteger)
        return false
    }
    return true
  }

  /**
    * Constructs the space of schedules carrying all data dependences in {@code deps}
    * in a way similar to  LeTSeE. The result is a list of Isl sets: One polyhedron per dimension.
    *
    * @return Returns the list of dimension polyhedra if a schedule that meets LeTSeE's criteria exists. Otherwise,
    * returns None.
    */
  def constructScheduleSpace(deps : Set[Dependence], conf : ConfigRandLeTSeEStyle, domInfo : DomainCoeffInfo,
    scop : ScopInfo) : Option[List[isl.Set]] = {
    myLogger.info("Sorting dependences by traffic size and grade of interference.")
    val initBoundingBox : isl.Set = Isl.boundDims(domInfo.universe, -1, 1)
    var depsSorted : List[Dependence] = sortDeps(deps.toList, scop, initBoundingBox, conf)
    var dimPolyhedra : List[isl.Set] = List.empty

    myLogger.info("Starting to construct the schedule space.")

    while (!depsSorted.isEmpty) {
      val (coeffSpace : isl.Set, stillUncarried : Set[Dependence]) = calculateCoeffSpace(depsSorted, depsSorted, domInfo,
        initBoundingBox) match {
        case None     => return None
        case Some(sp) => sp
      }
      dimPolyhedra ::= coeffSpace
      depsSorted = depsSorted.filter(stillUncarried.contains)
      System.gc()
      System.gc()
      System.gc()
      depsSorted = sortDeps(depsSorted, scop, initBoundingBox, conf)
    }
    myLogger.info("The schedule space has " + dimPolyhedra.size + " dimensions.")
    return Some(dimPolyhedra.reverse)
  }

  private def calculateCoeffSpace(
    deps : Iterable[Dependence],
    depsToCarry : Iterable[Dependence], domInfo : DomainCoeffInfo, universeBounded : isl.Set) : Option[(isl.Set, Set[Dependence])] = {

    var coeffSpace : isl.Set = domInfo.universe

    // weakly satisfy all dependences
    for (dep <- deps) {
      coeffSpace = coeffSpace.intersect(dep.weakConstr)
    }

    if (coeffSpace.intersect(universeBounded).isEmpty()) {
      myLogger.warning("No schedules with coefficients in {0, 1, -1} exists.")
      return None
    }

    // try to satisfy strongly as many dependences from depsToCarry as possible.
    val unsatisfiableDeps : HashSet[Dependence] = HashSet.empty
    for (dep <- depsToCarry) {
      val coeffSpaceOld : isl.Set = coeffSpace
      coeffSpace = coeffSpace.intersect(dep.strongConstr)
      if (coeffSpace.isEmpty() || coeffSpace.intersect(universeBounded).isEmpty()) {
        coeffSpace = coeffSpaceOld
        unsatisfiableDeps.add(dep)
      }
    }
    return Some(coeffSpace, unsatisfiableDeps.toSet)
  }

  private var dep2TrafficSize : Map[Dependence, Long] = null

  private def sortDeps(deps : List[Dependence], scop : ScopInfo,
    initBoundingBox : isl.Set, conf : ConfigRandLeTSeEStyle) : List[Dependence] = {
    if (dep2TrafficSize == null)
      dep2TrafficSize = conf.depsWeighingMethod match {
        case ConfigRandLeTSeEStyle.DepsWeighingMethod.STMT_MEM_TRAFFIC        => ScheduleSpaceUtils.calcMemTrafficSizesOfDepStmts(deps, scop, conf)
        case ConfigRandLeTSeEStyle.DepsWeighingMethod.APPROX_STMT_MEM_TRAFFIC => ScheduleSpaceUtils.calcMemTrafficSizesOfDepStmtsApprox(deps, scop, conf)
        case ConfigRandLeTSeEStyle.DepsWeighingMethod.DEP_CACHE_FOOT_PRINT    => ScheduleSpaceUtils.calcTrafficSizesOfDeps(deps, scop, conf)
      }
    val dep2Interference : Map[Dependence, Int] = ScheduleSpaceUtils.calcInterferenceOfDeps(
      deps,
      initBoundingBox)
    implicit val depOrd : Ordering[Dependence] = new Ordering[Dependence]() {

      def compare(d1 : Dependence, d2 : Dependence) : Int = {
        val d1Traffic : Long = dep2TrafficSize(d1)
        val d2Traffic : Long = dep2TrafficSize(d2)
        if (d1Traffic < d2Traffic)
          return -1
        else if (d1Traffic > d2Traffic)
          return 1
        else {
          val d1Interf = dep2Interference(d1)
          val d2Interf = dep2Interference(d2)
          return d1Interf - d2Interf
        }
      }
    }
    return deps.sorted
  }

  def generateLinIndepScheduleVector(s : Schedule, sampler : SamplingStrategy, samplerParams : SamplingStrategyParams,
    conf : Config) : Option[(List[Rat], Set[ScheduleSummand])] = {
    val maxNumRays : NumGeneratorsLimit = samplerParams.asInstanceOf[ChernikovaParams].currNumRaysLimit
    val maxNumLines : NumGeneratorsLimit = samplerParams.asInstanceOf[ChernikovaParams].currNumLinesLimit
    val confLetsee : ConfigRandLeTSeEStyle = conf.asInstanceOf[ConfigRandLeTSeEStyle]

    val linIndepCoeffSpace : isl.Set = s.computeLinIndepSpace(s.numDims, !conf.linIndepVectsDoNotFixDims) match {
      case None     => return None
      case Some(sp) => sp
    }
    if (linIndepCoeffSpace.isEmpty())
      throw new IllegalStateException("The coefficient space is empty: " + linIndepCoeffSpace)

    val linIndepBSets : isl.BasicSetList = linIndepCoeffSpace.getBasicSetList

    var schedVect : List[Rat] = null
    var schedSummands : Set[ScheduleSummand] = null

    while (schedVect == null) {
      val randLinIndepBSet : isl.BasicSet = linIndepBSets.getBasicSet(Random.nextInt(linIndepBSets.nBasicSet()))
      var g : GeneratorsRat = ChernikovaSamplingStrategy.constraints2GeneratorsRat(randLinIndepBSet, conf.moveVertices, conf.rayPruningThreshold)
      if (conf.moveVertices) {
        g = moveVertices(List(g), List(randLinIndepBSet)).head
      }
      val (schedVect1 : List[Rat], schedSummands1 : Set[ScheduleSummand]) = ChernikovaSamplingStrategy.generateScheduleVector(g, 1, -1, 1, maxNumRays, maxNumLines, conf)
      if (!confLetsee.boundSchedCoeffs || hasSuitableCoeffValues(schedVect1)) {
        schedVect = schedVect1
        schedSummands = schedSummands1
      }
    }
    return Some((schedVect, schedSummands))
  }
}
