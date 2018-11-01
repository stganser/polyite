package polyite.evolution.migration

import polyite.config.ConfigGA
import mpi.MPI
import polyite.pmpi.IMPI

object MigrationStrategyFactory {

  def createMigrationStrategy(mpi : IMPI, conf : ConfigGA) : Option[MigrationStrategy] = {
    if (conf.executionMode == ConfigGA.ExecutionMode.MPI) {
      conf.migrationStrategy.get match {
        case ConfigGA.MigrationStrategy.NEIGHBOR     => return Some(new MigrateNeighbor(mpi, conf))
        case ConfigGA.MigrationStrategy.NEIGHBORHOOD => Some(new MigrateNeighborhood(mpi, conf))
      }
    }
    return None
  }
}