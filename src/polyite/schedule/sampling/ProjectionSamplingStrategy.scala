package polyite.schedule.sampling

import scala.util.Random
import org.apache.commons.math3.distribution.GeometricDistribution
import isl.Isl
import isl.Isl.TypeAliases._
import polyite.config.Config
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.util.Rat
import isl.IslException
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.StmtCoeffInfo
import scala.math.BigInt.int2bigInt

/**
 * Schedule sampling technique based on projection. Start with a dimension of the schedule coefficient vector space, determine
 * its lower and upper bound, randomly choose a legal value (values close to zero are more likely, since a geometric distribution will be used)
 * between these bounds, fix the value of the dimension and recursively continue with the next dimension. The algorithm
 * is guaranteed to terminate at a single point with integer coordinates. Projection is used to determine the bounds per
 * dimension.
 */
object ProjectionSamplingStrategy extends SamplingStrategy {

  def createSamplingStrategyParamsFromConf(conf : Config) : SamplingStrategyParams = new SamplingStrategyParams() {}

  def preparePolyhedron(p : isl.Set, conf : Config) : Polyhedron = {
    return ConstraintRepr(p)
  }

  def prepareSamplingStrategyParams(region : Iterable[Polyhedron], conf : Config, numRaysLimit : NumGeneratorsLimit,
    numLinesLimit : NumGeneratorsLimit) : SamplingStrategyParams = createSamplingStrategyParamsFromConf(conf)

  def sampleCoeffVect(p : Polyhedron, domInfo : DomainCoeffInfo, conf : Config, params : SamplingStrategyParams) : (List[Rat], Set[ScheduleSummand]) = {

    val coeffsMin : Int = conf.schedCoeffsMin.getOrElse({
      throw new IllegalArgumentException("minimum value for schedule coefficients is not set in the configuration.")
    })
    val coeffsMax : Int = conf.schedCoeffsMax.getOrElse({
      throw new IllegalArgumentException("maximum value for schedule coefficients is not set in the configuration.")
    })

    val coeffsExpVal : Double = conf.schedCoeffsExpectationValue.getOrElse({
      throw new IllegalArgumentException("expectation value of absolute value of schedule coefficients is not set in the configuration.")
    })
    val distr : GeometricDistribution = new GeometricDistribution(1 / coeffsExpVal)

    var polyhedron : isl.Set = p.asInstanceOf[ConstraintRepr].constraints
    polyhedron = polyhedron.removeRedundancies()
    val result : Array[Rat] = new Array(domInfo.dim)

    // start by sampling values for the iteration variable coefficients in random order.
    val iterCoeffPossRandomOrder : List[Int] = Random.shuffle(domInfo.stmtInfo.map(_._2).map((stmtInfo : StmtCoeffInfo) => {
      (stmtInfo.itStart until stmtInfo.itStart + stmtInfo.nrIt).toList
    }).flatten.toList)
    for (coeffIdx : Int <- iterCoeffPossRandomOrder) {
      val coeff : Int = sampleCoeff(polyhedron, coeffIdx, coeffsMin, coeffsMax, distr)
      result(coeffIdx) = Rat(coeff)
      polyhedron = polyhedron.fixVal(T_SET, coeffIdx, isl.Val.fromInt(polyhedron.getCtx, coeff))
    }

    // sample coefficients for the constant
    val cstCoeffPossRandomOrder : List[Int] = Random.shuffle(domInfo.stmtInfo.map(_._2).map((stmtInfo : StmtCoeffInfo) => {
      stmtInfo.cstIdx
    }).toList)
    for (coeffIdx : Int <- cstCoeffPossRandomOrder) {
      val coeff : Int = sampleCoeff(polyhedron, coeffIdx, coeffsMin, coeffsMax, distr)
      result(coeffIdx) = Rat(coeff)
      polyhedron = polyhedron.fixVal(T_SET, coeffIdx, isl.Val.fromInt(polyhedron.getCtx, coeff))
    }

    // sample coefficients for the parameters
    val paramCoeffPossRandomOrder : List[Int] = Random.shuffle(domInfo.stmtInfo.map(_._2).map((stmtInfo : StmtCoeffInfo) => {
      (stmtInfo.parStart until stmtInfo.parStart + domInfo.nrParPS).toList
    }).flatten.toList)
    for (coeffIdx : Int <- paramCoeffPossRandomOrder) {
      val coeff : Int = sampleCoeff(polyhedron, coeffIdx, coeffsMin, coeffsMax, distr)
      result(coeffIdx) = Rat(coeff)
      polyhedron = polyhedron.fixVal(T_SET, coeffIdx, isl.Val.fromInt(polyhedron.getCtx, coeff))
    }

    return (result.toList, Set.empty)
  }

  private def sampleCoeff(p : isl.Set, dim : Int, coeffsMin : Int, coeffsMax : Int, distr : GeometricDistribution) : Int = {
    val projection : isl.Set = Isl.islSetProjectOntoDim(p, dim)
    if (!projection.removeDivs().subtract(projection).isEmpty())
      return sampleFrom1dSetWithDivs(coeffsMin, coeffsMax, projection, distr)
    val dimMin : Int = if (projection.dimHasLowerBound(T_SET, 0)) {
      val dimMinAff : isl.Aff = Isl.pwAff2Aff(projection.dimMin(0))
      if (dimMinAff.isCst())
        dimMinAff.getConstantVal.getNumSi.toInt
      else
        throw new IllegalArgumentException(f"dimension ${dim} of ${p} has a parametric lower bound.")
    } else {
      Int.MinValue
    }
    val dimMax : Int = if (projection.dimHasUpperBound(T_SET, 0)) {
      val dimMaxAff : isl.Aff = Isl.pwAff2Aff(projection.dimMax(0))
      if (dimMaxAff.isCst())
        dimMaxAff.getConstantVal.getNumSi.toInt
      else
        throw new IllegalArgumentException(f"dimension ${dim} of ${p} has a parametric lower bound.")
    } else {
      Int.MaxValue
    }

    val lb : Int = if (dimMax < coeffsMin)
      dimMax
    else if (dimMin > coeffsMin)
      dimMin
    else
      coeffsMin

    val ub : Int = if (dimMin > coeffsMax)
      dimMin
    else if (dimMax < coeffsMax)
      dimMax
    else
      coeffsMax

    if (ub < coeffsMin)
      return ub
    else if (lb > coeffsMax)
      return lb
    else {
      var coeff : Int = lb + Random.nextInt(ub - lb + 1)
      while (!accept(lb, ub, coeff, distr)) {
        coeff = lb + Random.nextInt(ub - lb + 1)
      }
      return coeff
    }
  }

  private def sampleCoeff1(p : isl.Set, dim : Int, coeffsMin : Int, coeffsMax : Int, distr : GeometricDistribution) : Int = {
    //    val projection : isl.Set = Isl.islSetProjectOntoDim(p, dim)
    val dimMinAff : isl.Aff = try {
      Isl.pwAff2Aff(p.dimMin(dim))
    } catch {
      case e : IslException => null
    }
    val dimMin : Int = if (dimMinAff == null)
      Int.MinValue
    else if (dimMinAff.isCst())
      dimMinAff.getConstantVal.getNumSi.toInt
    else
      throw new IllegalArgumentException(f"dimension ${dim} of ${p} has a parametric lower bound.")
    val dimMaxAff : isl.Aff = try {
      Isl.pwAff2Aff(p.dimMax(dim))
    } catch {
      case e : IslException => null
    }
    val dimMax : Int = if (dimMaxAff == null)
      Int.MaxValue
    else if (dimMaxAff.isCst())
      dimMaxAff.getConstantVal.getNumSi.toInt
    else
      throw new IllegalArgumentException(f"dimension ${dim} of ${p} has a parametric lower bound.")

    val lb : Int = if (dimMax < coeffsMin)
      dimMax
    else if (dimMin > coeffsMin)
      dimMin
    else
      coeffsMin

    val ub : Int = if (dimMin > coeffsMax)
      dimMin
    else if (dimMax < coeffsMax)
      dimMax
    else
      coeffsMax

    if (ub < coeffsMin)
      return ub
    else if (lb > coeffsMax)
      return lb
    else {
      var coeff : Int = lb + Random.nextInt(ub - lb + 1)
      while (!accept(lb, ub, coeff, distr)) {
        coeff = lb + Random.nextInt(ub - lb + 1)
      }
      return coeff
    }
  }

  private def sampleFrom1dSetWithDivs(coeffsMin : Int, coeffsMax : Int, p : isl.Set, distr : GeometricDistribution) : Int = {
    val ctx : isl.Ctx = p.getCtx
    val lb : isl.Point = isl.Point.zero(p.getSpace).setCoordinateVal(T_SET, 0, isl.Val.fromInt(ctx, coeffsMin))
    val ub : isl.Point = isl.Point.zero(p.getSpace).setCoordinateVal(T_SET, 0, isl.Val.fromInt(ctx, coeffsMax))
    val coeffsInterval : isl.Set = isl.Set.boxFromPoints(lb, ub)
    if (coeffsInterval.intersect(p).isEmpty()) {
      val lsp : isl.LocalSpace = isl.LocalSpace.fromSpace(p.getSpace)
      val geqConstr : isl.Constraint = isl.Constraint.allocInequality(lsp).setCoefficientSi(T_SET, 0, 1)
      val leqConstr : isl.Constraint = isl.Constraint.allocInequality(lsp).setCoefficientSi(T_SET, 0, -1)
      val geqP : isl.Set = p.addConstraint(geqConstr)
      val leqP : isl.Set = p.addConstraint(leqConstr)
      if ((Random.nextBoolean() && !geqP.isEmpty()) || leqP.isEmpty()) {
        return geqP.lexmin().samplePoint().getCoordinateVal(T_SET, 0).getNumSi.toInt
      } else {
        return leqP.lexmax().samplePoint().getCoordinateVal(T_SET, 0).getNumSi.toInt
      }
    } else {
      var coeff : Int = coeffsMin + Random.nextInt(coeffsMax - coeffsMin + 1)
      while (!accept(coeffsMin, coeffsMax, coeff, distr) || p.fixVal(T_SET, 0, isl.Val.fromInt(ctx, coeff)).isEmpty()) {
        coeff = coeffsMin + Random.nextInt(coeffsMax - coeffsMin + 1)
      }
      return coeff
    }
  }

  private def accept(lb : Int, ub : Int, x : Int, distr : GeometricDistribution) : Boolean = {
    val xAbsMoved =
      if (lb > 0)
        x - lb
      else if (ub < 0) {
        math.abs(x) - math.abs(ub)
      } else
        math.abs(x)
    return Random.nextDouble() <= distr.probability(xAbsMoved)
  }

  private def boundMostly(s : isl.Set, min : Int, max : Int) : isl.Set = {
    var result : isl.Set = s
    val lsp : isl.LocalSpace = isl.LocalSpace.fromSpace(result.getSpace)

    for (dim : Int <- 0 until s.dim(T_SET)) {
      val lb : isl.Constraint = isl.Constraint.allocInequality(lsp).setCoefficientSi(T_SET, dim, 1).setConstantSi(-min)
      val ub : isl.Constraint = isl.Constraint.allocInequality(lsp).setCoefficientSi(T_SET, dim, -1).setConstantSi(max)
      val lBounded : isl.Set = result.addConstraint(lb)
      if (!lBounded.isEmpty())
        result = lBounded
      val uBounded : isl.Set = result.addConstraint(ub)
      if (!uBounded.isEmpty())
        result = uBounded
    }
    return result.removeRedundancies()
  }
}
