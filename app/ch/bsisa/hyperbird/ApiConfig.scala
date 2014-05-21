package ch.bsisa.hyperbird

import play.api.Play

/**
 * Api configuration object.
 * 
 * Provides JavaScript client with dynamic configurations
 *
 * @author Patrick Refondini
 */
class ApiConfig {

  /**
   *  Used by Restangular to configure required baseUrl.
   */
  val baseUrl: String = Play.current.configuration.getString(ApiConfig.BaseUrlKey) match {
    case Some(baseUrlValue) => baseUrlValue
    case None => throw ApiConfigException(s"ApiConfig base URL information ${ApiConfig.BaseUrlKey} missing")
  }

  /**
   * Used by Angular $logProvider service to enable or disable debug log.
   */
  val clientDebugEnabled: Boolean = Play.current.configuration.getBoolean(ApiConfig.ClientDebugEnabledUrlKey) match {
    case Some(clientDebugEnabledValue) => clientDebugEnabledValue
    case None => throw ApiConfigException(s"ApiConfig client debug enabled information ${ApiConfig.ClientDebugEnabledUrlKey} missing")
  }  


}

/**
 * ApiConfig exception class
 */
case class ApiConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

/**
 * Companion object containing constants
 */
object ApiConfig {

  private val BaseUrlKey = "hb.api.baseUrl"
  private val ClientDebugEnabledUrlKey = "hb.api.clientDebugEnabled"

}
