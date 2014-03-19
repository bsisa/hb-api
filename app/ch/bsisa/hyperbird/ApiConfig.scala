package ch.bsisa.hyperbird

import play.api.Play

/**
 * Api configuration object.
 * 
 * Relates to Restangular required baseUrl
 *
 * @author Patrick Refondini
 */
class ApiConfig {

  /**
   *  
   *
   */
  val baseUrl: String = Play.current.configuration.getString(ApiConfig.BaseUrlKey) match {
    case Some(baseUrlKeyId) => baseUrlKeyId
    case None => throw ApiConfigException(s"ApiConfig base URL information ${ApiConfig.BaseUrlKey} missing")
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

}
