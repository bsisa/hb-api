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
   */
  val queryCacheEnabled: Boolean = Play.current.configuration.getBoolean(ApiConfig.QueryCacheEnabledUrlKey) match {
    case Some(queryCacheEnabledValue) => queryCacheEnabledValue
    case None => false // This property is optional, fallback to false without requiring configuration.
  }
  
  
  /**
   * Used by Api service to enable or disable data manager based security feature.
   */
  val dataManagerSecurityEnabled: Boolean = Play.current.configuration.getBoolean(ApiConfig.DataManagerSecurityEnabledUrlKey) match {
    case Some(dataManagerSecurityEnabledValue) => dataManagerSecurityEnabledValue
    case None => false // This property is optional, fallback to false without requiring configuration.
  }  
  

  /**
   * Used by Api service to enable or disable serverSideNotification service. Currently used to provide automatic offline detection feature.
   */
  val serverSideNotificationEnabled: Option[Int] = Play.current.configuration.getInt(ApiConfig.ServerSideNotificationEnabledUrlKey) 
  
  
  /**
   * Used by Api service to enable or disable services related to orders statistics module, such as orders id service (`OrdersIdActor`).
   */
  val ordersStatiticsModuleEnabled: Option[Boolean] = Play.current.configuration.getBoolean(ApiConfig.OrdersStatiticsModuleEnabledKey) 
  
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
  private val DataManagerSecurityEnabledUrlKey = "hb.api.dataManagerSecurityEnabled"
  private val ServerSideNotificationEnabledUrlKey = "hb.api.serverSideNotificationEnabled"
  private val OrdersStatiticsModuleEnabledKey = "hb.modules.ordersStatistics.enabled"
  

}
