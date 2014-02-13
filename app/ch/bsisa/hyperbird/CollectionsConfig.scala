package ch.bsisa.hyperbird

import play.api.Play

/**
 * Collections configuration object
 *
 * @author Patrick Refondini
 */
class CollectionsConfig {

  /**
   * Configuration collection
   */
  val configurationCollectionId: String = Play.current.configuration.getString(CollectionsConfig.ConfigurationCollectionKey) match {
    case Some(configCollectionId) => configCollectionId
    case None => throw CollectionsConfigException(s"Configuration collection identifier information ${CollectionsConfig.ConfigurationCollectionKey} missing")
  }

  /**
   * Catalogue collection
   */
  val catalogueCollectionId: String = Play.current.configuration.getString(CollectionsConfig.CatalogueCollectionKey) match {
    case Some(catalogueCollectionId) => catalogueCollectionId
    case None => throw CollectionsConfigException(s"Catalogue collection identifier information ${CollectionsConfig.CatalogueCollectionKey} missing")
  }

}

/**
 * Collections configuration exception class
 */
case class CollectionsConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

/**
 * Companion object containing constants
 */
object CollectionsConfig {

  private val ConfigurationCollectionKey = "hb.collection.configuration"
  private val CatalogueCollectionKey = "hb.collection.catalogue"

}
