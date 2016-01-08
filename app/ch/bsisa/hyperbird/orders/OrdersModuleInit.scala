package ch.bsisa.hyperbird.orders

import ch.bsisa.hyperbird.orders.actors.OrdersIdActor
import ch.bsisa.hyperbird.orders.messages.OrdersMaxValueInit

import ch.bsisa.hyperbird.ApiConfig
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.Implicits._

import akka.actor.Props
import akka.actor.Actor

import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._

import scala.concurrent.duration._

/**
 * Contains logic used to initialise orders statistics module.
 * 
 * @author Patrick Refondini
 */
object OrdersModuleInit {

  val GET_ORDERS_NUMBER_MAX_VALUE_XQUERY_FILENAME = "get_COMMANDE_NOM_max_number.xq"
  
  /**
   * Proceed to orders statistics module initialisation if the module is enabled.
   */
  def tryInitialisingOrdersModule()(implicit apiConfig: ApiConfig): Unit = {

    apiConfig.ordersStatiticsModuleEnabled match {

      case Some(ordersStatiticsModuleEnabledValue) =>
        if (ordersStatiticsModuleEnabledValue) {
          val ordersIdActor = Akka.system.actorOf(Props(new OrdersIdActor), name = "ordersIdActor")
          //val cl = Akka.system.scheduler.schedule(15.seconds, serverSideNotificationRefreshRate.seconds, serverSideNotificationActor, s"HB service on")

          
          XQueryWSHelper.runXQueryFile(GET_ORDERS_NUMBER_MAX_VALUE_XQUERY_FILENAME, None).map { response =>
            val jsonString = response.body.mkString
            Logger.debug(s">>>> OrdersModuleInit: Result of type ${response.ahcResponse.getContentType} received. Expected JSON value: " + jsonString)
            val jsonObj = play.api.libs.json.Json.parse(jsonString)
            val maxValueJs = jsonObj.\("max-value")
            val maxValue = maxValueJs.as[Int]
            Logger.debug(s"maxValue obtained is = ${maxValue}")
            ordersIdActor ! OrdersMaxValueInit(maxValue)
          }
          
          Logger.info(s"Orders statistics module enabled.")
        } else {
          Logger.info(s"Orders statistics module currently disabled in configurations.")
        }
      case None => // nothing to do, optional property not available holds same semantic as present with false value.
    }

  }

}
