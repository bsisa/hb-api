package ch.bsisa.hyperbird.util

import scala.concurrent.duration._
import play.api.Logger

/**
 * Helper to compute elapsed time
 *
 * @author Patrick Refondini
 */
object ElapsedTime {

  val logger = Logger("ch.bsisa.hyperbird.util.ElapsedTime")

  /**
   * Helper to compute execution time of a block of code
   */
  def time[U](codeBlock: => U, message: String): U = {
    val start = System.nanoTime()
    // Execute codeBlock 
    val result = codeBlock
    val stop = System.nanoTime()
    val duration = Duration((stop - start), NANOSECONDS)
    logger.info(s"Elapsed time: ${message}: ${duration.toMillis} [MILLISECONDS], ${duration} [NANOSECONDS]")
    result
  }

}