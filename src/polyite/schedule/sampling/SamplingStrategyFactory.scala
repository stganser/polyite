package polyite.schedule.sampling

import polyite.config.Config
import polyite.config.MinimalConfig

object SamplingStrategyFactory {
  
  def createSamplingStrategy(conf : Config) : SamplingStrategy = {
    conf.samplingStrategy match {
      case MinimalConfig.SamplingStrategy.CHERNIKOVA => ChernikovaSamplingStrategy
      case MinimalConfig.SamplingStrategy.PROJECTION => ProjectionSamplingStrategy
      case MinimalConfig.SamplingStrategy.GEOMETRIC_DIVIDE_AND_CONQUER => GeomDivideAndConquerSamplingStrategy
    }
  }
}