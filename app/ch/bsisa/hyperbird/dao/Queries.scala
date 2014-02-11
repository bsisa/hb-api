package ch.bsisa.hyperbird.dao

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import play.api.Logger

/**
 * List of queries expected to be implemented by classes using this trait.
 * Basic queries independent from underlying implementation might be implemented directly in this trait.
 *
 * @author Patrick Refondini
 */
trait Queries {

  /**
   * Returns a query to list all hb collections
   */
  def allHbCollectionsQuery(implicit conf: DbConfig): String

  /**
   * Returns a query to list all ELFINs of a given collections which satisfies to the provided XPath query
   */
  def filteredCollectionQuery(collectionId: String, xpath: String)(implicit conf: DbConfig): String

  /**
   * Returns a query to access a single ELFIN given its collection and unique elfin id.
   * The collection is provided to improve performance, indeed the elfin id is unique across
   * the whole database.
   */
  def elfinQuery(collectionId: String, elfinId: String)(implicit conf: DbConfig): String

  /**
   * Returns a query to access a single ELFIN given its unique elfin id.
   */
  def elfinQuery(elfinId: String)(implicit conf: DbConfig): String

}

/**
 * ResultNotFound exception class
 */
case class ResultNotFound(message: String = null, cause: Throwable = null) extends Exception(message, cause)
  