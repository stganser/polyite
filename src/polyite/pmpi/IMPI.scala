package polyite.pmpi

import java.util.logging.Logger

/**
  * Interface to hide concrete MPI implementations.
  */
trait IMPI {

  /**
    * Initializes MPI.
    * Required to be called before using any MPI functions.
    *
    * @param args: The program's command line arguments.
    */
  def Init(args : Array[String])

  /**
    * Shuts down MPI.
    * Required to be called after finishing to use MPI and
    * before program termination.
    * No MPI functions may be called afterward.
    */
  def Finalize()

  /**
    * MPI ID/index of the calling instance.
    */
  def rank() : Int

  /**
    * Number of parallel MPI nodes.
    */
  def size() : Int

  /**
    * Usually index 0  is considered the root instance.
    * Conceptually it is not different from any other instance.
    */
  def isRoot() : Boolean

  /**
    * Return the index of the right-neighbor instance.
    * Wraps around for the highest index.
    */
  def getRightNeighborRank() : Int = {
    if (size() == 1)
      return rank()
    else if (rank() == size() - 1)
      return 0;
    else
      return rank() + 1;
  }

  /**
    * Return the index of the left-neighbor instance.
    * Wraps around for the highest index.
    */
  def getLeftNeighborRank() : Int = {
    if (size() == 1)
      return rank()
    else if (rank() == 0)
      return size() - 1
    else
      return rank() - 1
  }

  /**
    * Creates a prefix that shows the the rank of the MPI process and the total number of MPI processes for log messages.
    */
  protected def log_prefix() : String = f"mpi[${rank()} / ${size}]"

  /**
    * Adds a prefix that contains the rank of the MPI process and the total number of MPI processes to the given message.
    */
  protected def add_log_prefix(l : String) : String = f"${log_prefix()} ${l}"

  /**
    * A full barrier synchronization.
    */
  def fullBarrier() {}

  /**
    * Send a string to another instance.
    *
    * @param str: the string to send
    * @param dest: the destination MPI index
    */
  def sendString(str : String, dest : Int)

  /**
    * Receive a string from another instance.
    *
    * @param src: the index to receive the string from
    *
    * @return: the received string
    */
  def recvString(src : Int) : String

  /**
    * Aborts the session and terminates all MPI instances.
    */
  def abortAllAndTerminate(reason : String, errorcode : Int = 0)

  protected val logger = Logger.getLogger("")

  /**
    * Logs the given message using log-level INFO and prefixes the log message by the rank of the MPI process.
    */
  def info(l : String) = logger.info(add_log_prefix(l))

  /**
    * Logs the given message using log-level WARNING and prefixes the log message by the rank of the MPI process.
    */
  def warn(l : String) = logger.warning(add_log_prefix(l))

  /**
    * Logs the given message using log-level FINE and prefixes the log message by the rank of the MPI process.
    */
  def fine(l : String) = logger.fine(add_log_prefix(l))
}
