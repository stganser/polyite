package polyite.pmpi

import mpi._
import java.util.logging.Logger
import java.net.InetAddress

/**
 * OpenMPI implementation
 */
object OpenMPI extends IMPI {

  private var myrank: Int = 0
  private var total: Int = 0

  override def Init(args: Array[String]) {

    MPI.Init(args)
    myrank = MPI.COMM_WORLD.getRank()
    total = MPI.COMM_WORLD.getSize()
    fine("MPI init with rank " + rank() + " of " + size() + " on host " + InetAddress.getLocalHost().getHostName())
    fine("getnextrank: " + getRightNeighborRank())
    fine("machinename: " + InetAddress.getLocalHost().getHostName())
  }

  override def Finalize() {
    MPI.Finalize()
    fine("MPI finalized")
  }

  override def rank(): Int = myrank
  override def size(): Int = total
  override def isRoot(): Boolean = myrank == 0

  override def sendString(str: String, dest: Int) {

    val strArray = str.toCharArray();
    MPI.COMM_WORLD.send(strArray, strArray.length, MPI.CHAR, dest, 0)
  }

  override def recvString(src: Int): String = {

    val msg = new mpi.Message()
    val status: mpi.Status = msg.mProbe(src, 0, MPI.COMM_WORLD)
    val len = status.getCount(MPI.CHAR)
    val recvStr: Array[Char] = Array.fill(len) { '0' }
    msg.mRecv(recvStr, len, MPI.CHAR)
    return recvStr.mkString("")
  }
  
  
  override def abortAllAndTerminate(reason : String, errorcode : Int = 0) {
    warn("Terminating all MPI instances, reason: " + reason)
    MPI.COMM_WORLD.abort(errorcode)
  }

  override def fullBarrier() {
    MPI.COMM_WORLD.barrier()
  }
}

