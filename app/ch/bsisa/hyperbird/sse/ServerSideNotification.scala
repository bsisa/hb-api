package ch.bsisa.hyperbird.sse

import play.api._
import play.api.Logger
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.JsValue
import play.api.libs.iteratee.{ Concurrent, Enumeratee }
import play.api.libs.EventSource

/**
 * ServerSideNotification controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * <i>Closely inspired after https://github.com/matthiasn/sse-chat</i>
 * 
 * @author Patrick Refondini 
 */
object ServerSideNotification extends Controller with securesocial.core.SecureSocial {

  /** Central hub for notifying groups */
  val (notificationOut, notificationChannel) = Concurrent.broadcast[JsValue]

  /** Controller action for POSTing notification to group */
  def postMessage = Action(parse.json) { req =>
    Logger.info(s"HyperBird application ServerSideNotification postMessage = ${req.body}")
    notificationChannel.push(req.body)
    Ok
  }

  /** Enumeratee filtering messages based on group */
  def filter(group: String) = Enumeratee.filter[JsValue] { json: JsValue => (json \ "group").as[String] == group }

  /** Enumeratee detecting SSE stream disconnection */
  def connDeathWatch(addr: String): Enumeratee[JsValue, JsValue] =
    Enumeratee.onIterateeDone { () =>
      Logger.info(s"${addr} - SSE disconnected")
    }

  /** Controller action serving activity based on group */
  def notificationFeed(group: String) = Action { req =>
    Logger.info(s"${req.remoteAddress} - SSE connected")
    Ok.feed(notificationOut
      &> filter(group)
      &> Concurrent.buffer(1500)
      &> connDeathWatch(req.remoteAddress)
      &> EventSource()).as("text/event-stream")
  }

}