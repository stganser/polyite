package polyite.sched_eval

/**
 * A {@code BenchmarkingException} can be used to signal that the benchmarking
 * of a schedule or something else has failed severely. This should yield
 * termination of the application.
 */
class BenchmarkingException(msg : String) extends Exception(msg)