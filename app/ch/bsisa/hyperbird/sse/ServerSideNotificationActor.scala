package ch.bsisa.hyperbird.sse

import akka.actor.{ Actor, ActorLogging }
import play.api.libs.json.Json
import play.api.libs.json.JsValue
//import play.api.libs.json.JsObject
import java.util.Date
import play.api.libs.json.Json.toJsFieldJsValueWrapper

/**
 * ServerSideNotificationActor pushes basic message to provide connection status.
 * 
 * @author Patrick Refondini
 */
class ServerSideNotificationActor extends Actor with ActorLogging {

  def receive = {
    
    case messageReceived:JsValue =>  
      log.debug("ServerSideNotificationActor reveived JsValue for ELFIN.Id {}" , messageReceived.\("elfin").\("Id"));
      ServerSideNotification.notificationChannel.push(messageReceived)
    
    case messageReceived:String =>
      log.debug("ServerSideNotificationActor reveived String");
      val currentDate = s"${new Date()}"
      val messageToSend = Json.obj("group" -> "status", "text" -> messageReceived, "user" ->  "server", "time" -> currentDate )
      ServerSideNotification.notificationChannel.push(messageToSend)

    case _ =>
      log.warning("Actor controller received unexpected message at {}", new Date())
  }


}



