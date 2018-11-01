package polyite.evolution.migration

import polyite.schedule.Schedule
import polyite.sched_eval.Fitness
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.Dependence
import polyite.pmpi.IMPI
import polyite.config.ConfigGA
import polyite.util.Rat
import scala.util.Random
import polyite.export.JSONLogExporter
import scala.collection.mutable.HashMap

/**
  * A migrating strategy migrates schedules among the Polyite processes that execute the distributed genetic algorithm.
  */
trait MigrationStrategy {

  /**
    * Calling this method performs one round of schedule migration.
    *
    * @param basis schedules that may potentially be migrated together with their fitness
    * @param domInfo model of the schedule coefficient vector space
    * @param deps data dependences
    * @param currentGeneration the current generation of the distributed genetic algorithm
    * @return The schedules that have been received during migration together with their fitness.
    */
  def migrate(basis : Array[(Schedule, Fitness)], domInfo : DomainCoeffInfo, deps : Set[Dependence], currentGeneration : Int) : List[(Schedule, Fitness)]

  /**
    * Converts a schedule to JSON.
    */
  protected def sched2json(sched : Schedule, fit : Fitness, currentGeneration : Int) : String = {
    return JSONLogExporter.population2JSON(List((sched, fit)), currentGeneration, true);
  }

  /**
    * Converts a JSON-ified schedule back to a schedule object.
    */
  protected def json2sched(json : String, domInfo : DomainCoeffInfo, deps : Set[Dependence]) : ((Schedule, Fitness), Int) = {
    val rec : (HashMap[Schedule, Fitness], Int) = JSONLogExporter.json2Population(json, domInfo, deps, true);

    val sched : Schedule = rec._1.head._1
    val fit : Fitness = rec._1.head._2
    val gen : Int = rec._2

    return ((sched, fit), gen)
  }
}

/**
  * With this schedule migration strategy, the Polyite process with rank k sends schedules either to the process with
  * rank k + 1 or k - 1 (modulo the number of processes). Calling this method performs one round of schedule migration.
  * This method relies on MPI.
  *
  * @param mpi gives access to MPI
  * @param conf the configuration of Polyite
  */
class MigrateNeighbor(mpi : IMPI, conf : ConfigGA) extends MigrationStrategy {

  override def migrate(basis : Array[(Schedule, Fitness)], domInfo : DomainCoeffInfo, deps : Set[Dependence], currentGeneration : Int) : List[(Schedule, Fitness)] = {

    val numScheds2Migrate : Int = (Rat(basis.size) * conf.migrationVolume.get).intFloor.toInt
    mpi.info(s"ready to migrate $numScheds2Migrate schedules")

    val neighborRankRight = if ((currentGeneration % 2) == 0) mpi.getRightNeighborRank() else mpi.getLeftNeighborRank()
    val neighborRankLeft = if ((currentGeneration % 2) == 0) mpi.getLeftNeighborRank() else mpi.getRightNeighborRank()

    if ((currentGeneration % conf.migrationRate.get) != 0) {
      mpi.info("skipped migration for generation " + currentGeneration)
      return List.empty
    }

    if ((mpi.size() % 2) != 0 && mpi.size() > 1 && mpi.rank() == mpi.size() - 1) {
      mpi.warn("highest mpi-instance does not have a partner and cannot participate")
      return List.empty
    }

    val lengthStr : String = numScheds2Migrate.toString
    var migrantsCount = 0
    if ((mpi.rank() % 2) == 0) {
      mpi.sendString(lengthStr, neighborRankRight)
      val partner_length = mpi.recvString(neighborRankRight)
      migrantsCount = math.min(partner_length.toInt, numScheds2Migrate)
    } else {
      val partnerLength = mpi.recvString(neighborRankLeft)
      mpi.sendString(lengthStr, neighborRankLeft)
      migrantsCount = math.min(partnerLength.toInt, numScheds2Migrate)
    }

    mpi.info("Agreed on migrantcount = " + migrantsCount)
    var migrants : List[(Schedule, Fitness)] = List.empty

    val scheds2MigrateIndices : List[Int] = Random.shuffle((0 until basis.size).toList).take(migrantsCount)

    for ((i, r) <- scheds2MigrateIndices.zipWithIndex) {

      val sched : Schedule = basis(i)._1
      val fit : Fitness = basis(i)._2
      mpi.info(s"round of migration r = $r in generation $currentGeneration")

      val schedSend : String = if (fit.isCompletelyEvaluated) sched2json(sched, fit, currentGeneration) else ""
      if (schedSend.isEmpty()) {
        mpi.fine("Schedule is not migrated because it failed to evaluate.")
      }

      var schedRecv : String = ""
      if ((mpi.rank() % 2) == 0) {
        mpi.fine("partner: " + neighborRankRight)
        mpi.sendString(schedSend, neighborRankRight)
        schedRecv = mpi.recvString(neighborRankRight)
      } else {
        mpi.fine("partner: " + neighborRankLeft)
        schedRecv = mpi.recvString(neighborRankLeft)
        mpi.sendString(schedSend, neighborRankLeft)
        if (schedRecv != "") {
          val migrated = json2sched(schedRecv, domInfo, deps);
          if (migrated._2 != currentGeneration)
            mpi.warn("Generation mismatch in mpi migration! Expected %d, Received %d".format(currentGeneration, migrated._2))
          migrants ::= migrated._1
        } else {
          mpi.fine("Received invalid schedule, discard it. This is not an error.")
        }
      }
      mpi.info("migration success")
    }
    return migrants
  }
}

/**
  * This schedule migration strategy arranges the <i>n</i> Polyite processes in a square of edge length <i>sqrt(n)</i>.
  * The communication partner of each process alternates from its top, to its bottom, to its left, to its right neighbor. For this strategy to be applicable, the
  * total number of Polyite processes must be quadratic. This strategy relies on MPI.
  *
  * @param mpi Provides access to MPI.
  * @param conf provides access to the configuration of the genetic algorithm.
  */
class MigrateNeighborhood(mpi : IMPI, conf : ConfigGA) extends MigrationStrategy {
  override def migrate(basis : Array[(Schedule, Fitness)], domInfo : DomainCoeffInfo, deps : Set[Dependence], currentGeneration : Int) : List[(Schedule, Fitness)] = {

    if ((currentGeneration % conf.migrationRate.get) != 0) {
      mpi.info("skipped migration for generation " + currentGeneration)
      return List.empty
    }

    val migrationCount = currentGeneration / conf.migrationRate.get
    val numScheds2Migrate : Int = (Rat(basis.size) * conf.migrationVolume.get).intFloor.toInt

    val rc : Int = Math.sqrt(mpi.size() + 1).toInt
    if (rc * rc != mpi.size()) {
      mpi.warn("mpi instances need to be of type x^2, mpi.size = " + mpi.size() + ", rc = " + rc)
      return List.empty
    }

    if (mpi.size() < 4) {
      mpi.warn("neighborhood migration strategy only works with 4 or more mpi instances!")
      return List.empty
    }

    mpi.fine(f"mpi migration count: ${migrationCount}, neighbor: schedule to migrate: ${numScheds2Migrate}")

    var neighborRank : Int = 0
    var migrants : List[(Schedule, Fitness)] = List.empty

    val myRank = mpi.rank()
    val maxRank = mpi.size() - 1

    val numRows = rc
    val numCols = rc

    val mup = () => if (myRank - rc < 0) (maxRank - rc) + myRank + 1 else myRank - rc
    val mdown = () => if (myRank + rc > maxRank) (myRank + rc) % rc else (myRank + rc)

    val mright = () => if (myRank % rc == rc - 1) myRank - numCols + 1 else myRank + 1
    val mleft = () => if (myRank % rc == 0) myRank + numCols - 1 else myRank - 1

    var sched_recv : String = ""

    val scheds2MigrateIndices : List[Int] = Random.shuffle((0 until basis.size).toList).take(numScheds2Migrate)

    for (i <- scheds2MigrateIndices) {

      val sched = basis(i)._1
      val fit = basis(i)._2
      val schedSend = sched2json(sched, fit, currentGeneration)

      //determine migration partner for neighborhood topology

      var route = migrationCount % 4

      val myrow = myRank / rc;
      val mycol = myRank % rc;

      if (route == 0 || route == 1) {
        //up || down

        neighborRank = if (myrow % 2 == 0) mup() else mdown()
        if (route == 1)
          neighborRank = if (myrow % 2 == 0) mdown() else mup()

        mpi.fine(f"migration partner = ${neighborRank} (route = ${route})")

        if (myrow % 2 == 0) {
          mpi.sendString(schedSend, neighborRank)
          sched_recv = mpi.recvString(neighborRank)
        } else {
          sched_recv = mpi.recvString(neighborRank)
          mpi.sendString(schedSend, neighborRank)
        }

      } else if (route == 2 || route == 3) {
        //right || left

        if (mycol % 2 == 0)
          neighborRank = mright()
        else
          neighborRank = mleft()

        neighborRank = if (mycol % 2 == 0) mright() else mleft()
        if (route == 3)
          neighborRank = if (mycol % 2 == 0) mleft() else mright()

        mpi.fine(f"migration partner = ${neighborRank} (route = ${route})")

        if (mycol % 2 == 0) {
          mpi.sendString(schedSend, neighborRank)
          sched_recv = mpi.recvString(neighborRank)
        } else {
          sched_recv = mpi.recvString(neighborRank)
          mpi.sendString(schedSend, neighborRank)
        }
      }

      val migrated = json2sched(sched_recv, domInfo, deps)

      if (migrated._2 != currentGeneration) {
        mpi.warn(f"Generation mismatch in mpi migration! Expected ${currentGeneration}, Received ${migrated._2}")
        return List.empty
      }

      migrants ::= migrated._1
    }
    return migrants
  }
}