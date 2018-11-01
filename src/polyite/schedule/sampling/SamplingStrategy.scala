package polyite.schedule.sampling

import polyite.config.Config
import polyite.util.Rat
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.config.MinimalConfig.NumGeneratorsLimit
import polyite.schedule.DomainCoeffInfo

/**
  * Some sampling strategies may need to inspect the list of polyhedra that model a region of the schedule search space.
  * The result of the inspection may be stored in an instance of {@code SamplingStrategyParams}.
  */
trait SamplingStrategyParams {}

/**
  * Interface for schedule sampling strategies. A sampling strategy randomly selects a (rational) schedule coefficient
  * vector from a polyhedron of possible schedule coefficient vectors.
  */
trait SamplingStrategy {

  /**
    * The search space construction yields descriptions of search space regions. These descriptions consist of lists of
    * polyhedra that are represented by linearly affine constraints. Sampling strategies may use this method to
    * convert polyhedra to the representation that they use and deliver an according implementation of {@code Polyhedron}.
    *
    * @param p the polyhedron to translate
    * @param conf Polyite configuration.
    */
  def preparePolyhedron(p : isl.Set, conf : Config) : Polyhedron

  /**
    * Some sampling strategies may need to inspect the list of polyhedra that model a region of the schedule search space.
    * The result of the inspection may be stored in an instance of {@code SamplingStrategyParams}. Implement this method
    * to perform any such inspection. The returned object will be passed to
    * {@code SamplingStrategyParams.sampleCoeffVect(Polyhedron, DomainCoeffInfo, Config, SamplingStrategyParams)} whenever
    * it is being called to sample a schedule coefficient vector from one of the polyhedra in {@code region}.
    * @param region the search space region to analyze in the representation that resulted from calling {@code SamplingStrategy.preparePolyhedron(Set, Config)}
    * for each polyhedron in the region's representation.
    * @param conf Polyite configuration
    * @param numRaysLimit parameter that is particularly related to Chernikova sampling.
    * {@code numRaysLimit} is the maximum number of rays that may have a coefficient unequal to zero in the linear combination
    * that forms a schedule coefficient vector.
    * @param numLinesLimit parameter that is particularly related to Chernikova sampling.
    * {@code numLinesLimit} is the maximum number of lines that may have a coefficient unequal to zero in the linear
    * combination that forms a schedule coefficient vector.
    */
  def prepareSamplingStrategyParams(region : Iterable[Polyhedron], conf : Config, numRaysLimit : NumGeneratorsLimit,
    numLinesLimit : NumGeneratorsLimit) : SamplingStrategyParams

  /**
    * Alternativ to {@code SampligStrategy.prepareSamplingStrategyParams(Iterable[Polyhedron], Config, NumGeneratorsLimit, NumGeneratorsLimit)}
    * that will be called in order to generate the sampling strategy parameters independent of a particular
    * search space region.
    */
  def createSamplingStrategyParamsFromConf(conf : Config) : SamplingStrategyParams

  /**
   * Sample a schedule coefficient vector from {@code p}. The polyhedron is represented in the format created by
   * {@code SamplingStrategy.preparePolyhedron(Set, Config)}.
   * @param p polyhedron in the format created by {@code SamplingStrategy.preparePolyhedron(Set, Config)}.
   * @param domInfo description of the schedule coefficient vector space
   * @param conf Polyite configuration
   * @param params result of search space region inspection by {@code SampligStrategy.prepareSamplingStrategyParams(Iterable[Polyhedron], Config, NumGeneratorsLimit, NumGeneratorsLimit)}.
   * @return the sampled schedule coefficient vector as a list of rational numbers and in case of Chernikova sampling
   * a set of generators with coefficients that form the yielded schedule coefficient vector. Leave this set empty
   * for other sampling techniques.
   */
  def sampleCoeffVect(p : Polyhedron, domInfo : DomainCoeffInfo, conf : Config, params : SamplingStrategyParams) : (List[Rat], Set[ScheduleSummand])
}