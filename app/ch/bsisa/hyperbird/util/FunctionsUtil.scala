package ch.bsisa.hyperbird.util

import scala.concurrent.{ blocking }

/**
 * Provides reusable functions
 */
object FunctionsUtil {

  /**
   *
   * Retries `n` times function `fn`.
   *
   * Returns T, throwing the exception on failure *
   *
   * Directly from one of the multiple proposals at:
   * http://stackoverflow.com/questions/7930814/whats-the-scala-way-to-implement-a-retry-able-call-like-this-one
   */
  @annotation.tailrec
  def retry[T](n: Int)(fn: => T): T = {
    util.Try { fn } match {
      case util.Success(x) => x
      case _ if n > 1 => retry(n - 1)(fn)
      case util.Failure(e) => throw e
    }
  }

  /**
   * Retries `n` times function `fn` with a delay of `waitMillis` between each retry.
   *
   * Returns T, throwing the exception on failure
   */
  @annotation.tailrec
  def retry[T](n: Int, waitMillis: Long = 0)(fn: => T): T = {
    util.Try { fn } match {
      case util.Success(x) => x
      case _ if n > 1 => {
        blocking(Thread.sleep(waitMillis))
        retry(n - 1, waitMillis)(fn)
      }
      case util.Failure(e) => throw e
    }
  }

}