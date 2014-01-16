package ch.bsisa.hyperbird

import play.api.Application
import play.api.GlobalSettings
import play.api.Logger
import play.api.Play

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
    Logger.info("HyperBird application started.")
  }

  /**
   *  Hook into application life-cycle stop event.
   */
  override def onStop(app: Application) = {
    Logger.info("HyperBird application stopped.")
  }

}