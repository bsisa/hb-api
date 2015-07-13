package ch.bsisa.hyperbird.sse

import akka.actor.{ Actor, ActorLogging }
import play.api.libs.json.Json
import java.util.Date
import play.api.libs.json.Json.toJsFieldJsValueWrapper

/**
 * ServerSideNotificationActor pushes basic message to provide connection status.
 * 
 * @author Patrick Refondini
 */
class ServerSideNotificationActor extends Actor with ActorLogging {

  def receive = {

    case messageReceived:String =>
      val currentDate = s"${new Date()}"
      val messageToSend = Json.obj("group" -> "status", "text" -> messageReceived, "user" ->  "server", "time" -> currentDate )
      ServerSideNotification.notificationChannel.push(messageToSend)

    case _ =>
      log.warning(s"Received unexpected message at ${new Date()}")
  }


}



