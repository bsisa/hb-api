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

  def log()(implicit apiConfig: ApiConfig): Unit = {

    // Compute log friendly states
    val dataManagerSecurityState = if (apiConfig.dataManagerSecurityEnabled) { "on" } else { "off" }
    val queryCacheState = if (apiConfig.queryCacheEnabled) { "on" } else { "off" }
    val serverSideNotification = if (apiConfig.serverSideNotificationEnabled.isDefined) { "on" } else { "off" }
    val ordersStatisticsModule = apiConfig.ordersStatiticsModuleEnabled match {
      case Some(value) => if (value) { "on" } else { "off" }
      case None        => "off"
    }

    // Log optional configuration states
    Logger.info(s"HyperBird optional server side query cache is turned ${queryCacheState}")
    Logger.info(s"HyperBird optional data manager based security is turned ${dataManagerSecurityState}")
    Logger.info(s"HyperBird optional server side notification is turned ${serverSideNotification}")
    Logger.info(s"HyperBird optional orders statistics module is turned ${ordersStatisticsModule}")

  }

}