package ch.bsisa.hyperbird

import play.api.Logger

/**
 * Logs information regarding optional configurations status. Intended for use at application startup.
 * 
 * Note: Mandatory configurations throw exceptions when missing and are dealt with where required, mainly in Api controller.
 *  
 * @author Patrick Refondini
 */
object OptionalConfigLogger {

  def log()(implicit apiConfig: ApiConfig) : Unit = {
    
    // Compute log friendly states
    val dataManagerSecurityState = if (apiConfig.dataManagerSecurityEnabled) { "on" } else { "off" }
    val queryCacheState = if (apiConfig.queryCacheEnabled) { "on" } else { "off" }
    
    // Log optional configuration states
    Logger.info(s"Optional server side query cache is turned ${queryCacheState}")
    Logger.info(s"Optional data manager based security is turned ${dataManagerSecurityState}")

  }
  
}