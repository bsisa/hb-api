package ch.bsisa.hyperbird

import play.api.Application
import play.api.GlobalSettings
import play.api.Logger
import play.api.Play
import play.api.Play.current
import scala.concurrent.duration._
import play.api.libs.concurrent.Akka
import java.util.Date
import akka.actor.Props
import akka.actor.Actor
import org.joda.time.Seconds
import org.joda.time.DateTime
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.patman.simulations.SimulationScheduler

import ch.bsisa.hyperbird.sse.ServerSideNotificationScheduler
import play.api.libs.concurrent.Execution.Implicits._

import ch.bsisa.hyperbird.Implicits._

/**
 * Handles global settings for the application providing some hooks.
 *
 * Note: Do not try reading configuration properties
 * using play.api.Play at this stage:
 * <code>Play.current.configuration.getString("my.property")</code>
 *
 * @author Patrick Refondini
 */
object Global extends GlobalSettings {

  /**
   *  Hook into application life-cycle start event.
   */
  override def onStart(app: Application) = {
    Logger.info(s"HyperBird application started at ${new Date()}")

    OptionalConfigLogger.log()

    // Optional. Will only be effective if the corresponding configuration is available.
    SimulationScheduler.trySchedulingSimulation()

    // Optional. Will only be effective if the corresponding configuration is available.
    ServerSideNotificationScheduler.trySchedulingNotification() 

  }

  /**
   *  Hook into application life-cycle stop event.
   */
  override def onStop(app: Application) = {
    Logger.info(s"HyperBird application stopped at ${new Date()}")
  }

}