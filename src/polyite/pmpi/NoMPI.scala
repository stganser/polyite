package polyite.pmpi

/**
  * Do-Nothing dummy for when MPI is not used.
  */
object NoMPI extends IMPI {

  override def Init(args : Array[String]) {}
  override def Finalize() {}

  override def rank() : Int = 0
  override def size() : Int = 1
  override def isRoot() : Boolean = true

  override def getRightNeighborRank() : Int = 0

  override def abortAllAndTerminate(reason : String, errorcode : Int = 0) = {}

  override def sendString(str : String, dest : Int) {}
  override def recvString(src : Int) : String = ""
}