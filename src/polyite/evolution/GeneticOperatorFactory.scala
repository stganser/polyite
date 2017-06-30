package polyite.evolution

import polyite.config.ConfigGA
import polyite.schedule.Schedule
import scala.collection.mutable.HashSet

/**
  * Factory for genetic operators.
  */
object GeneticOperatorFactory {

  /**
    * Identifiers of the existing genetic operators.
    */
  object GeneticOperators extends Enumeration {
    val ReplaceDims = Value("replaceDims")
    val ReplacePrefix = Value("replacePrefix")
    val ReplaceSuffix = Value("replaceSuffix")
    val MutateGeneratorCoeffs = Value("mutateGeneratorCoeffs")
    val GeometricCrossover = Value("geometricCrossover")
    val RowCrossover = Value("rowCrossover")

    /**
      * Check, whether the given operator is a mutator.
      */
    def isMutator(op : Value) : Boolean = {
      return op != GeometricCrossover && op != RowCrossover
    }
  }

  private val mutationStrategies : Map[GeneticOperators.Value, (ConfigGA, Int) => (Schedule => Option[Schedule])] = Map(
    (GeneticOperators.ReplaceDims, MutationStrategies.replaceDims),
    (GeneticOperators.ReplacePrefix, MutationStrategies.replacePrefix),
    (GeneticOperators.ReplaceSuffix, MutationStrategies.replaceSuffix),
    (GeneticOperators.MutateGeneratorCoeffs, MutationStrategies.mutateGeneratorCoeffs))

  private val crossoverStrategies : Map[GeneticOperators.Value, ConfigGA => ((Schedule, Schedule) => HashSet[Schedule])] = Map(
    (GeneticOperators.GeometricCrossover, CrossoverStrategies.geometricCrossover),
    (GeneticOperators.RowCrossover, CrossoverStrategies.rowCrossover))

  /**
    * Produces a mutator for the requested operation and the given configuration. Simulated annealing is adjusted for
    * the given generation.
    */
  def createMutator(op : GeneticOperators.Value, conf : ConfigGA, generation : Int) : (Schedule => Option[Schedule]) = {
    return mutationStrategies(op)(conf, generation)
  }

  /**
    * Produces a crossover function for the requested operation and the given configuration.
    */
  def createCrossover(op : GeneticOperators.Value, conf : ConfigGA) : (Schedule, Schedule) => HashSet[Schedule] = {
    return crossoverStrategies(op)(conf)
  }
}