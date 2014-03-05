package ch.bsisa.hyperbird

import play.api.Play

/**
 * Init configuration object.
 * 
 * Relates to former HB4 hb_init.xml configuration file
 *
 * @author Patrick Refondini
 */
class InitConfig {

  /**
   * Init id refers to hb_init.xml ELFIN configuration file Id.
   *  
   * Note: This Elfin.ID_G must match the hb.collection.configuration collection id.
   */
  val initElfinId: String = Play.current.configuration.getString(InitConfig.InitIdKey) match {
    case Some(initKeyId) => initKeyId
    case None => throw InitConfigException(s"Init Elfin.Id information ${InitConfig.InitIdKey} missing")
  }


}

/**
 * Init configuration exception class
 */
case class InitConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

/**
 * Companion object containing constants
 */
object InitConfig {

  private val InitIdKey = "hb.init.id"

}
