package polyite.schedule.sampling

import java.math.BigInteger

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.util.Random

import org.exastencils.schedopt.chernikova.Chernikova
import org.exastencils.schedopt.chernikova.Generators

import isl.Isl
import isl.Isl.TypeAliases.T_SET
import polyite.config.Config
import polyite.config.MinimalConfig
import polyite.config.MinimalConfig.LimitedGenerators
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.config.MinimalConfig.RandLimit
import polyite.util.Rat
import polyite.util.Util
import polyite.util.Util.vec2List
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.Dependence
import polyite.schedule.ScheduleVectorUtils

/**
 * Schedule sampling technique based on Chernikova's algorithm. Schedule coefficient vectors are the result of
 * choosing randomly a vertex, and adding a linear combination of lines and rays.
 */
object ChernikovaSamplingStrategy extends SamplingStrategy {

  case class ChernikovaParams(currNumRaysLimit : NumGeneratorsLimit, currNumLinesLimit : NumGeneratorsLimit) extends SamplingStrategyParams

  def preparePolyhedron(p : isl.Set, conf : Config) : Polyhedron = {
    return GeometricRepr(constraints2GeneratorsRat(p, conf.moveVertices, conf.rayPruningThreshold))
  }

  def prepareSamplingStrategyParams(region : Iterable[Polyhedron], conf : Config, numRaysLimit : NumGeneratorsLimit,
    numLinesLimit : NumGeneratorsLimit) : SamplingStrategyParams = {
    // Find out the maximum number of generators of all dimensions
    val generators : Iterable[GeneratorsRat] = region.map(_.asInstanceOf[GeometricRepr].generators)
    val maxNumRaysAllDims : Int = getMaxNumRays(generators)
    val maxNumLinesAllDims : Int = getMaxNumLines(generators)
    val currNumRaysLimit : NumGeneratorsLimit = numRaysLimit match {
      case RandLimit => LimitedGenerators(Random.nextInt(maxNumRaysAllDims) + 1)
      case _         => numRaysLimit
    }
    val currNumLinesLimit : NumGeneratorsLimit = numLinesLimit match {
      case RandLimit => LimitedGenerators(Random.nextInt(maxNumLinesAllDims) + 1)
      case _         => numLinesLimit
    }
    return ChernikovaParams(currNumRaysLimit, currNumLinesLimit)
  }

  def createSamplingStrategyParamsFromConf(conf : Config) : SamplingStrategyParams = ChernikovaParams(conf.maxNumRays, conf.maxNumLines)

  def sampleCoeffVect(p : Polyhedron, domInfo : DomainCoeffInfo, conf : Config, params : SamplingStrategyParams) : (List[Rat], Set[ScheduleSummand]) = {
    val generators : GeneratorsRat = p.asInstanceOf[GeometricRepr].generators
    val paramsC : ChernikovaParams = params.asInstanceOf[ChernikovaParams]
    return generateScheduleVector(generators, conf.rayCoeffsRange, -conf.lineCoeffsRange, conf.lineCoeffsRange,
      paramsC.currNumRaysLimit, paramsC.currNumLinesLimit, conf)
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
      dimConstraint = dimConstraint.setConstantVal(isl.Val.fromBigInteger(
        ctx,
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

    return (
      Isl.simplify(validChernikovaCoeffs),
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

  case class GeneratorsRat(vertices : List[List[Rat]], rays : List[List[Rat]], lines : List[List[Rat]]) {
    override def toString() : String = {
      val sb : StringBuilder = StringBuilder.newBuilder

      def mkGeneratorsList(l : List[List[Rat]]) : String = {
        return l.map((v : List[Rat]) => v.mkString("(", ", ", ")")).mkString("{ ", ",\n\t", " }")
      }

      sb.append("vertices : ")
      sb.append(mkGeneratorsList(vertices))
      sb.append("\nrays : ")
      sb.append(mkGeneratorsList(rays))
      sb.append("\nlines : ")
      sb.append(mkGeneratorsList(lines))
      return sb.toString()
    }
  }

  /**
    * Converts the given set of generators into vectors of rational numbers.
    * @return An instance of {@code GeneratorsProcessed}.
    */
  def preprocess(g : Generators) : GeneratorsRat = {
    def bigInt2Rat(v : Vector[BigInt]) : List[Rat] = (v map (Rat(_)))
    val vertices : List[List[Rat]] = g.vertices.toList map {
      (v : (Chernikova.V, BigInt)) =>
        (v._1 map {
          c => Rat(c, v._2)
        }).toList
    }
    val rays : List[List[Rat]] = g.rays.toList.map(bigInt2Rat)
    val lines : List[List[Rat]] = g.lines.toList.map(bigInt2Rat)
    return GeneratorsRat(vertices, rays, lines)
  }

  /**
    * Converts the given Isl set to its dual representation. Throws an
    * {@code IllegalStateException} if no or more than one set of generators
    * results.
    *
    * @param mvVertices Optionally, vertices can be moved such that they have only integer coordinates.
    * @param coordinateThreshold rays and lines with coordinates whose absolute value is larger than {@code coordinateThreshold}
    * are pruned. If {@code coordinateThreshold == None} pruning is disabled.
    */
  def constraints2GeneratorsRat(s : isl.Set, mvVertices : Boolean, coordinateThreshold : Option[Rat]) : GeneratorsRat = {
    var gs : GeneratorsRat = preprocess(constraints2Generators(s))
    if (mvVertices)
      gs = moveVertices(gs, s)
    if (coordinateThreshold.isDefined)
      gs = pruneRaysAndLines(gs, coordinateThreshold.get)
    return gs
  }

  /**
    * Converts the given Isl set to its dual representation. Throws an
    * {@code IllegalStateException} if no or more than one set of generators
    * results.
    */
  def constraints2GeneratorsRat(s : isl.Set) : GeneratorsRat = constraints2GeneratorsRat(s, false, None)

  def constraints2Generators(s : isl.Set) : Generators = {
    val gs : Set[Generators] = Chernikova.constraintsToGenerators(s.detectEqualities().removeRedundancies())
    if (gs.size == 1)
      return gs.head
    else
      throw new IllegalStateException("expected exactly one set of generators: " + gs.size)
  }

  /**
    * Replaces rational coefficients of vertices with (close-by) integer coefficients. This prevents large coefficients
    * in schedule coefficients vectors. Some of the integer coefficients may change as well.
    */
  def moveVertices(gs : GeneratorsRat, constraints : isl.Set) : GeneratorsRat = {
    val verticesNew : List[List[Rat]] = (for (v : List[Rat] <- gs.vertices) yield {
      val dims2Replace : Set[Int] = v.zipWithIndex.filterNot(_._1.isInteger).map(_._2).toSet
      if (dims2Replace.isEmpty)
        v
      else {
        val ls : isl.LocalSpace = isl.LocalSpace.fromSpace(constraints.getSpace)
        val ctx : isl.Ctx = constraints.getCtx
        var s : isl.Set = (0 until v.length).foldLeft(constraints)((constr : isl.Set, i : Int) => {
          if (dims2Replace.contains(i)) {
            constr
          } else {
            val withIFixed : isl.Set = constr.fixVal(T_SET, i, isl.Val.fromBigInteger(ctx, v(i).intCeil.bigInteger))
            if (withIFixed.isEmpty())
              constr
            else
              withIFixed
          }
        })
        assert(!s.isEmpty())
        s = dims2Replace.foldLeft(s)((constr : isl.Set, i : Int) => {
          val c : Rat = v(i)
          val neighboringInts : List[BigInteger] = List(c.intCeil.bigInteger, c.intFloor.bigInteger).sorted
          val limitedToFst : isl.Set = constr.fixVal(T_SET, i, isl.Val.fromBigInteger(ctx, neighboringInts.head))
          if (!limitedToFst.isEmpty())
            limitedToFst
          else {
            val limitedToSnd : isl.Set = constr.fixVal(T_SET, i, isl.Val.fromBigInteger(ctx, neighboringInts(1)))
            if (!limitedToSnd.isEmpty())
              limitedToSnd
            else
              constr
          }
        })
        assert(!s.isEmpty())
        Util.islPoint2RatList(s.samplePoint())
      }
    }).toList
    return GeneratorsRat(verticesNew, gs.rays, gs.lines)
  }

  private def pruneRaysAndLines(gs : GeneratorsRat, coordinateThreshold : Rat) : GeneratorsRat = {
    def checkKeep(g : List[Rat]) : Boolean = g.forall { _.abs <= coordinateThreshold }
    val raysNew : List[List[Rat]] = gs.rays.filter(checkKeep)
    val linesNew : List[List[Rat]] = gs.lines.filter(checkKeep)
    return GeneratorsRat(gs.vertices, raysNew, linesNew)
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
    * Constructs a schedule space as a list of Chernikova generator sets. The
    * schedules in the constructed schedule space will carry all dependences
    * contained by {@code deps} in the order given in {@code depsPartition}. The
    * function stops generating new dimensions as soon as all dependencimport polyite.util.Utiles have
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
  def constructScheduleSpaceFromDepsPartitionDual(deps : Set[Dependence], depsPartition : List[Set[Dependence]],
    domInfo : DomainCoeffInfo) : Option[Iterable[GeneratorsRat]] = {
    ScheduleSpaceUtils.checkIsValidPartitioning(deps, depsPartition)
    ScheduleSpaceUtils.constructScheduleSpaceFromDepsPartition(deps, depsPartition, domInfo) match {
      case None => return None
      case Some(sp) => {
        return Some(sp.map(constraints2GeneratorsRat))
      }
    }
  }
}