package polyite

import scala.util.Random
import java.util.concurrent.LinkedBlockingQueue
import java.util.logging.Logger
import java.util.concurrent.TimeUnit
import scala.collection.parallel.CollectionsHaveToParArray

object FakeMPI {
  def Finalize() : Unit = ()

  def Init(args : Array[String]) : Unit = ()

  def abort_all_and_terminate(reason : String, errorcode : Int) : Unit = ()

  def is_root() : Boolean = true

  def rank(rank : Int) : Int = rank

  def recv_string(rank : Int, src : Int) : String = {
    var res : String = null
    while (res == null) {
      res = postBoxes(src)(rank).poll(10, TimeUnit.MILLISECONDS)
      if (res == null)
        Thread.sleep(100)
    }
    println(f"${rank}: Receiving ${res} from ${src}")
    return res
  }

  def send_string(str : String, rank : Int, dest : Int) : Unit = {
    println(f"${rank}: Sending ${str} to ${dest}.")
    var success = false
    while (!success) {
      success = postBoxes(rank)(dest).offer(str, 10, TimeUnit.MILLISECONDS)
      if (!success)
        Thread.sleep(100)
    }
  }

  def size() : Int = 16

  val postBoxes : Array[Array[LinkedBlockingQueue[String]]] = new Array(size())
  for (i <- 0 until size()) {
    postBoxes(i) = new Array(size())
    for (j <- 0 until size()) {
      postBoxes(i)(j) = new LinkedBlockingQueue(1)
    }
  }

  def get_neighbor_rank(rank : Int) : Int = {
    if (size() == 1)
      return rank
    else if (rank == size() - 1)
      return 0;
    else
      return rank + 1;
  }

  def get_left_neighbor_rank(rank : Int) : Int = {
    if (size() == 1)
      return rank
    else if (rank == 0)
      return size() - 1
    else
      return rank - 1
  }

  protected val logger = Logger.getLogger("")
  def info(rank : Int, l : String) = logger.info(add_log_prefix(rank, l))
  def warn(rank : Int, l : String) = logger.warning(add_log_prefix(rank, l))
  def fine(rank : Int, l : String) = logger.finer(add_log_prefix(rank, l))

  def log_prefix(rank : Int) : String = f"mpi[${rank} / ${size}]"

  def add_log_prefix(rank : Int, l : String) : String = f"${log_prefix(rank)} ${l}"
}

object MigrationStrategiesSimulation {

  val mpi = FakeMPI

  val migration_rate : Int = 1
  val migration_volume = 50

  def neighbor_migrate_double(rank : Int)(basis : Array[String], currentGeneration : Int) : List[String] = {

    val numScheds2Migrate : Int = (basis.size * migration_volume) / 100
    mpi.info(rank, s"ready to migrate $numScheds2Migrate schedules")

    val neighbor_rank_right = if ((currentGeneration % 2) == 0) mpi.get_neighbor_rank(rank) else mpi.get_left_neighbor_rank(rank)
    val neighbor_rank_left = if ((currentGeneration % 2) == 0) mpi.get_left_neighbor_rank(rank) else mpi.get_neighbor_rank(rank)

    if ((currentGeneration % migration_rate) != 0) {
      mpi.info(rank, "skipped migration for generation " + currentGeneration)
      return List.empty
    }

    if ((mpi.size() % 2) != 0 && mpi.size() > 1 && mpi.rank(rank) == mpi.size() - 1) {
      mpi.warn(rank, "highest mpi-instance does not have a partner and cannot participate")
      return List.empty
    }

    val lengthstr : String = numScheds2Migrate.toString
    var migrantcount = 0
    if ((mpi.rank(rank) % 2) == 0) {
      mpi.send_string(lengthstr, rank, neighbor_rank_right)
      val partner_length = mpi.recv_string(rank, neighbor_rank_right)
      migrantcount = math.min(partner_length.toInt, numScheds2Migrate)
    } else {
      val partner_length = mpi.recv_string(rank, neighbor_rank_left)
      mpi.send_string(lengthstr, rank, neighbor_rank_left)
      migrantcount = math.min(partner_length.toInt, numScheds2Migrate)
    }

    mpi.info(rank, "Agreed on migrantcount = " + migrantcount)
    var migrants : List[String] = List.empty

    val scheds2MigrateIndices : List[Int] = Random.shuffle((0 until basis.size).toList).take(migrantcount)

    for ((i, r) <- scheds2MigrateIndices.zipWithIndex) {
      mpi.info(rank, s"round of migration r = $r in generation $currentGeneration")

      val sched_send : String = basis(i)
      if (sched_send.isEmpty()) {
        mpi.fine(rank, "Schedule is not migrated because it failed to evaluate.")
      }

      var sched_recv : String = ""
      if ((mpi.rank(rank) % 2) == 0) {
        mpi.fine(rank, "partner: " + neighbor_rank_right)
        mpi.send_string(sched_send, rank, neighbor_rank_right)
        sched_recv = mpi.recv_string(rank, neighbor_rank_right)
      } else {
        mpi.fine(rank, "partner: " + neighbor_rank_left)
        sched_recv = mpi.recv_string(rank, neighbor_rank_left)
        mpi.send_string(sched_send, rank, neighbor_rank_left)
        if (sched_recv != "") {
          migrants ::= sched_recv
        } else {
          mpi.fine(rank, "Received invalid schedule, discard it. This is not an error.")
        }
      }

      mpi.info(rank, "migration success")

    }

    return migrants
  }

  def neighbor_migrate_neig(rank : Int)(basis : Array[String], currentgeneration : Int) : List[String] = {

    if ((currentgeneration % migration_rate) != 0) {
      mpi.info(rank, "skipped migration for generation " + currentgeneration)
      return List.empty
    }

    val migration_count = currentgeneration / migration_rate // Shouldn't migration rate determine whether we migrate at all in this round? (compare the other strategy)
    val numScheds2Migrate : Int = (basis.size * migration_volume) / 100

    val rc : Int = Math.sqrt(mpi.size() + 1).toInt
    if (rc * rc != mpi.size()) {
      mpi.warn(rank, "mpi instances need to be of type x^2, mpi.size = " + mpi.size() + ", rc = " + rc)
      return List.empty
    }

    if (mpi.size() < 4) {
      mpi.warn(rank, "neighborhood migration strategy only works with 4 or more mpi instances!")
      return List.empty
    }

    mpi.fine(rank, f"mpi migration count: ${migration_count}, neighbor: schedule to migrate: ${numScheds2Migrate}")

    var neighbor_rank : Int = 0
    var migrants : List[String] = List.empty

    val myrank = mpi.rank(rank)
    val maxrank = mpi.size() - 1

    val num_rows = rc
    val num_cols = rc

    val mup = () => if (myrank - rc < 0) (maxrank - rc) + myrank + 1 else myrank - rc
    val mdown = () => if (myrank + rc > maxrank) (myrank + rc) % rc else (myrank + rc)

    val mright = () => if (myrank % rc == rc - 1) myrank - num_cols + 1 else myrank + 1
    val mleft = () => if (myrank % rc == 0) myrank + num_cols - 1 else myrank - 1

    var sched_recv : String = ""

    val scheds2MigrateIndices : List[Int] = Random.shuffle((0 until basis.size).toList).take(numScheds2Migrate)

    for (i <- scheds2MigrateIndices) {

      val message : String = basis(i)

      //determine migration partner for neighborhood topology

      var route = migration_count % 4 //up, down, right, left
      //      route = 3 // ???

      val myrow = myrank / rc;
      val mycol = myrank % rc;

      if (route == 0 || route == 1) {
        //up || down

        neighbor_rank = if (myrow % 2 == 0) mup() else mdown()
        if (route == 1)
          neighbor_rank = if (myrow % 2 == 0) mdown() else mup()

        mpi.fine(rank, f"migration partner = ${neighbor_rank} (route = ${route})")

        if (myrow % 2 == 0) {
          mpi.send_string(message, rank, neighbor_rank)
          sched_recv = mpi.recv_string(rank, neighbor_rank)
        } else {
          sched_recv = mpi.recv_string(rank, neighbor_rank)
          mpi.send_string(message, rank, neighbor_rank)
        }

      } else if (route == 2 || route == 3) {
        //right || left

        if (mycol % 2 == 0)
          neighbor_rank = mright() //if (mycol == rc-1) myrank - num_cols + 1 else myrank  + 1//(myrow * num_cols) + ((mycol + 1) % num_cols)
        else
          neighbor_rank = mleft() //if (mycol == 0) myrank + num_cols - 1 else myrank - 1

        neighbor_rank = if (mycol % 2 == 0) mright() else mleft()
        if (route == 3)
          neighbor_rank = if (mycol % 2 == 0) mleft() else mright()

        mpi.fine(rank, f"migration partner = ${neighbor_rank} (route = ${route})")

        if (mycol % 2 == 0) {
          mpi.send_string(message, rank, neighbor_rank)
          sched_recv = mpi.recv_string(rank, neighbor_rank)
        } else {
          sched_recv = mpi.recv_string(rank, neighbor_rank)
          mpi.send_string(message, rank, neighbor_rank)
        }
      }

      migrants ::= sched_recv
    }
    return migrants
  }

  def main(args : Array[String]) : Unit = {
    val workers = (for (i <- 0 until mpi.size()) yield neighbor_migrate_neig(i)_).toParArray
    val basis = Array("a", "b", "c", "d")
    for (gen <- 0 until 10) {
      println(f"===============================================================\ngeneration: ${gen}\n\n\n")
      val threads : List[Thread] = (for (i <- 0 until workers.length) yield {
        val t = (new Thread() {
          override def run() {
            workers(i)(basis, gen)
            println("end")
          }
        })
        t.start()
        t
      }).toList
      threads.foreach(_.join())
    }
  }
}