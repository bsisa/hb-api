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
   * Catalog collection
   */
  val catalogCollectionId: String = Play.current.configuration.getString(CollectionsConfig.CatalogCollectionKey) match {
    case Some(catalogueCollectionId) => catalogueCollectionId
    case None => throw CollectionsConfigException(s"Catalogue collection identifier information ${CollectionsConfig.CatalogCollectionKey} missing")
  }

  /**
   * XQueries collection
   */
  val xqueriesCollectionId: String = Play.current.configuration.getString(CollectionsConfig.XQueriesCollectionKey) match {
    case Some(xqueriesCollectionId) => xqueriesCollectionId
    case None => throw CollectionsConfigException(s"XQueries collection identifier information ${CollectionsConfig.XQueriesCollectionKey} missing")
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
  private val CatalogCollectionKey = "hb.collection.catalogue"
  private val XQueriesCollectionKey = "hb.collection.xqueries"

}
