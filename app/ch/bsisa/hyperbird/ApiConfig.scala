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
   *  Used for ELFIN Annexes flat file storage
   */
  val annexesRootFolder: String = Play.current.configuration.getString(ApiConfig.AnnexesRootFolderKey) match {
    case Some(annexesRootFolderValue) => annexesRootFolderValue
    case None => throw ApiConfigException(s"ApiConfig annexes root folder information ${ApiConfig.AnnexesRootFolderKey} missing")
  }

  /**
   *  Used for ELFIN Annexes flat file temporary upload
   */
  val temporaryUploadFolder: String = Play.current.configuration.getString(ApiConfig.TemporaryUploadFolderKey) match {
    case Some(temporaryUploadFolderValue) => temporaryUploadFolderValue
    case None => throw ApiConfigException(s"ApiConfig temporary upload folder information ${ApiConfig.TemporaryUploadFolderKey} missing")
  }

  /**
   * Used by Angular $logProvider service to enable or disable debug log.
   */
  val clientDebugEnabled: Boolean = Play.current.configuration.getBoolean(ApiConfig.ClientDebugEnabledUrlKey) match {
    case Some(clientDebugEnabledValue) => clientDebugEnabledValue
    case None => throw ApiConfigException(s"ApiConfig client debug enabled information ${ApiConfig.ClientDebugEnabledUrlKey} missing")
  }

  /**
   * Used by Api service to enable or disable queries cache feature.
   *
   * '''WARNING''': This feature is still in alpha state and should not be activated in production yet. Before production use it requires design review, extended testing and tuning (TTL,...) to make sure no unintended side effects exist.
   */
  val queryCacheEnabled: Boolean = Play.current.configuration.getBoolean(ApiConfig.QueryCacheEnabledUrlKey) match {
    case Some(queryCacheEnabledValue) => queryCacheEnabledValue
    case None => false // This property is optional, fallback to false without requiring configuration.
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

  private val TemporaryUploadFolderKey = "hb.api.temporaryUploadFolder"
  private val AnnexesRootFolderKey = "hb.api.annexesRootFolder"
  private val BaseUrlKey = "hb.api.baseUrl"
  private val ClientDebugEnabledUrlKey = "hb.api.clientDebugEnabled"
  private val QueryCacheEnabledUrlKey = "hb.api.queryCacheEnabled"

}
