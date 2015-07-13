package ch.bsisa.hyperbird.sse

import ch.bsisa.hyperbird.ApiConfig

import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import scala.concurrent.duration._
import akka.actor.Props
import akka.actor.Actor
import play.api.libs.concurrent.Execution.Implicits._


import java.util.Date

/**
 * Configures scheduling of server side notification according to `ApiConfig.serverSideNotificationEnabled`
 * property availability and value.
 *  
 */
object ServerSideNotificationScheduler {

  
  def trySchedulingNotification()(implicit apiConfig: ApiConfig) : Unit = { 
    
    apiConfig.serverSideNotificationEnabled match {
      
      case Some(serverSideNotificationRefreshRate) =>
       
        val serverSideNotificationActor = Akka.system.actorOf(Props(new ServerSideNotificationActor), name = "serverSideNotificationActor")
        val cl = Akka.system.scheduler.schedule(15.seconds, serverSideNotificationRefreshRate.seconds, serverSideNotificationActor, s"HB service on")
        Logger.info(s"Scheduling serverSideNotification every ${serverSideNotificationRefreshRate} seconds.")
        
      case None => // nothing to do 
    }

  }

}