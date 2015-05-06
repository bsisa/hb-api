package ch.bsisa.hyperbird.patman.simulations

import play.api.Play
import ch.bsisa.hyperbird.util.DateUtil

/**
 * Patman configuration object used to pass implicit parameters to functions.
 * 
 * Patman configuration must be optional since the patman module is currently
 * always packaged with hyperbird common base API. This might be reviewed 
 * once patman will be package separately to its own Play module.
 * 
 * @author Patrick Refondini
 */
class PatmanConfig {

  /**
   * Optional simulation refresh time value.
   *
   * Expected string format is 23:08:45:889 for 23h 8min 45 sec 889 millis
   * The string precision will be interpreted as:
   *
   * ""    			=>  0h 0min  0sec 0millis
   * "14"  			=> 14h 0min  0sec 0millis
   * "14:01" 		=> 14h 1min  0sec 0millis
   * "14:01:23" 	=> 14h 1min 23sec 0millis
   * "14:01:23:2" 	=> 14h 1min 23sec 2millis
   *
   *  Simulation will be cleaned up and run every 24 hours at above specified time.
   */
  val simulationRefreshTime: Option[(Option[Int], Option[Int], Option[Int], Option[Int])] =
    Play.current.configuration.getString(PatmanConfig.SIMULATION_REFRESH_TIME_KEY) match {
      case Some(refreshTimeString) => DateUtil.parseTime(refreshTimeString)
      case None => None
    }

}

object PatmanConfig {

  val SIMULATION_REFRESH_TIME_KEY = "hb.patman.simulation.refresh-time"

}