package polyite.schedule.sampling

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import isl.Conversions.convertBigIntegerToBigInt
import isl.Conversions.convertLambdaToVoidCallback1
import isl.Conversions.convertLambdaToVoidCallback2
import isl.Conversions.convertValToBigInt
import isl.Isl
import isl.Isl.TypeAliases.T_SET
import polyite.config.Config
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.schedule.DomainCoeffInfo
import polyite.util.Rat
import polyite.util.Util

/**
  * Sampling of schedules based on the geometric divide-and-conquer approach described in Dominik Danner's master's thesis.
  * The code has been ported from Dominik's implementation.
  *
  * Geometric Divide-and-Conquer recursively splits a given Z-polytope in two halfs of approximatively cardinality. The
  * algorithm selects randomly one half and continues recursively until the remaining polytope contains a single point.
  * This point will be returned. Two split the polyhedra, the algorithm relies on Barvinok's counting algorithm and
  * binary search. The technique was suggested by Igor Pak, ("ON SAMPLING INTEGER POINTS IN POLYHEDRA", Foundations of Computational Mathematics, 2002)
  */
object GeomDivideAndConquerSamplingStrategy extends SamplingStrategy {

  case class SetWithSize(s : isl.Set, size : BigInt) extends Polyhedron

  def preparePolyhedron(p : isl.Set, conf : Config) : Polyhedron = {
    var maxSchedCoeffsAbs : Int = conf.schedCoeffsAbsMax.get
    var res : isl.Set = Isl.boundDims(p, -maxSchedCoeffsAbs, maxSchedCoeffsAbs)
    while (res.isEmpty()) {
      maxSchedCoeffsAbs += 1
      res = Isl.boundDims(p, -maxSchedCoeffsAbs, maxSchedCoeffsAbs)
    }
    val size : BigInt = Isl.islSetCountNPoints(conf.barvinokBinary, conf.barvinokLibraryPath, res).max().getNum()
    return SetWithSize(res.coalesce(), size)
  }

  def prepareSamplingStrategyParams(region : Iterable[Polyhedron], conf : Config, numRaysLimit : NumGeneratorsLimit,
    numLinesLimit : NumGeneratorsLimit) : SamplingStrategyParams = {
    return new SamplingStrategyParams {}
  }

  def createSamplingStrategyParamsFromConf(conf : Config) : SamplingStrategyParams = {
    return new SamplingStrategyParams {}
  }

  def sampleCoeffVect(p : Polyhedron, domInfo : DomainCoeffInfo, conf : Config, params : SamplingStrategyParams) : (List[Rat], Set[ScheduleSummand]) = {
    val point : isl.Point = sampleUniformFromSet(conf, p.asInstanceOf[SetWithSize])
    return (Util.islPoint2RatList(point), Set.empty)
  }

  /**
    * This method implements the sampling algorithm suggested by Igro Pak.
    *
    * As an Oracle the Barvinok binary is used.
    *
    * First a hyperplane is found by binary search. This hyperplane has the property, that
    * it divides the given {@code set} into two hyperspaces that only contains less then
    * the half of the points from {@code set}. With the given propability
    * (number of points in halfspace/hyperplane divided by number of points in {@code set})
    * one of the three new spaces is selected. This results in either a dimension drop or
    * reduction of the points to be considered by more the half.
    *
    * This algorithm runs recursive until the number of possible points is small enough to be
    * sampled directly. The number depends on the dimension of the Space and the RAM.
    *
    * NOTE: This can be implemented much faster by using Barvinok internals, or
    * choosing a hyperplane with good conditions.
    *
    * This method runs O(d^2 L^2) times the barvinok binary
    *
    * @return one uniform sample from the given {@code set}
    */
  private def sampleUniformFromSet(conf : Config, set : SetWithSize) : isl.Point = {
    // Terminal case: the set is so small, that you can just compute one
    // uniform random point from it.
    if (set.size == 0) throw new IllegalArgumentException("Set must not be empty")
    val size = 1000
    if (set.size < size) {
      val buffer : ArrayBuffer[isl.Point] = new ArrayBuffer(set.size.toInt)
      set.s.foreachPoint((p : isl.Point) => buffer.append(p))
      return buffer(Random.nextInt(buffer.length))
    }

    var (hplus, hyperplane, hminus) = getHalfspaceHyperplane(conf, set)
    // alpha + beta + gamma = 1
    val alpha = Rat(hplus.size, set.size)
    val beta = Rat(hminus.size, set.size)
    val gamma = Rat(hyperplane.size, set.size)

    val list = List((alpha, hplus), (gamma, hyperplane), (beta, hminus))
    return sampleUniformFromSet(conf, Util.selectRandom(list))
  }

  /**
    * This method generates the hyperplane that halves the given set.
    *
    * This algorithm runs in O(dL)
    *
    * TODO optimize by first taking dimensions with many constraints -> faster
    *
    * @return
    */
  private def getHalfspaceHyperplane(conf : Config, set : SetWithSize) : (SetWithSize, SetWithSize, SetWithSize) = {
    var dim = set.s.dim(T_SET) - 1
    while (dim >= 0) {
      // calculate min and max of the dimension
      var max : BigInt = null
      set.s.dimMax(dim).foreachPiece((_ : isl.Set, piece : isl.Aff) => max = piece.getConstantVal.getNum)
      var min : BigInt = null
      set.s.dimMin(dim).foreachPiece((_ : isl.Set, piece : isl.Aff) => min = piece.getConstantVal.getNum)
      var interval = max - min

      // swap to new ctx
      val ctxTmp = Isl.ctx
      val ls = isl.LocalSpace.fromSpace(set.s.getSpace)

      /*
       * Start binary search on diff.
       */
      while (interval > 0) {
        val c = min + (interval / BigInt(2L))

        // initialize Sets
        var hplaneSet : isl.Set = isl.Set.readFromStr(ctxTmp, set.s.toString())
        var hplusSet : isl.Set = isl.Set.readFromStr(ctxTmp, set.s.toString())
        var hminusSet : isl.Set = isl.Set.readFromStr(ctxTmp, set.s.toString())

        // hyperplane dim = c
        var constraint = isl.Constraint.allocEquality(ls)
        constraint = constraint.setCoefficientSi(T_SET, dim, 1)
        constraint = constraint.setConstantVal(isl.Val.fromBigInteger(ctxTmp, (-c).bigInteger))
        hplaneSet = hplaneSet.addConstraint(constraint)
        hplaneSet.coalesce()

        // hplus dim > c
        constraint = isl.Constraint.allocInequality(ls)
        constraint = constraint.setCoefficientSi(T_SET, dim, 1)
        constraint = constraint.setConstantVal(isl.Val.fromBigInteger(ctxTmp, (-(c + 1)).bigInteger))
        hplusSet = hplusSet.addConstraint(constraint)
        hplusSet.coalesce()

        // hminus dim < c
        constraint = isl.Constraint.allocInequality(ls)
        constraint = constraint.setCoefficientSi(T_SET, dim, -1)
        constraint = constraint.setConstantVal(isl.Val.fromBigInteger(ctxTmp, (c - 1).bigInteger))
        hminusSet = hminusSet.addConstraint(constraint)
        hminusSet.coalesce()

        // build sizes with barvinok
        val hplane = new SetWithSize(hplaneSet, Isl.islSetCountNPoints(conf.barvinokBinary, conf.barvinokLibraryPath, hplaneSet).max.asVal())
        val hplus = new SetWithSize(hplusSet, Isl.islSetCountNPoints(conf.barvinokBinary, conf.barvinokLibraryPath, hplusSet).max.asVal())
        val hminus = new SetWithSize(hminusSet, Isl.islSetCountNPoints(conf.barvinokBinary, conf.barvinokLibraryPath, hminusSet).max.asVal())

        val alpha = hplus.size.toDouble / set.size.toDouble
        val beta = hminus.size.toDouble / set.size.toDouble
        val gamma = hplane.size.toDouble / set.size.toDouble

        // exit, if a halving hyperplane is found
        if (alpha <= 0.5 && beta <= 0.5) {
          return (hminus, hplane, hplus)
        }

        // go on with binary search
        if (alpha > 0.5) {
          // c++
          min = c + 1
        } else { // beta > 0.5
          // c--
          max = c
        }
        interval = max - min
      }
      dim = dim - 1
    }

    // error
    println("Space :" + set.toString())
    throw new IllegalStateException("Something terribly went wrong reboot your mind!")
  }
}