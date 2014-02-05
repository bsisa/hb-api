package ch.bsisa.hyperbird.dao

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import play.api.Logger
import ch.bsisa.hyperbird.model.ELFIN

/**
 * List of update functions expected to be implemented by classes using this trait.
 *
 * @author Patrick Refondini
 */
trait Updates {

  /**
   * Creates a new ELFIN in the underlying database given the provided ELFIN.ID_G and ELFIN.CLASSE attributes values.
   * The returned ELFIN object contains a new ELFIN.Id value and other CLASSE dependent initial values.
   */
  //def create(elfinID_G: String, elfinCLASSE: String)(implicit conf: DbConfig): ELFIN
  def create(elfinID_G: String, elfinCLASSE: String)(implicit conf: DbConfig): Unit

  /**
   * Replace an existing ELFIN in the underlying database with the new provided ELFIN.
   */
  def replace(elfin: ELFIN)(implicit conf: DbConfig): Unit

  /**
   * Deletes any ELFIN from the underlying database which matches the provided ELFIN.ID_G and ELFIN.Id parameters.
   */
  def delete(elfinID_G: String, elfinId: String)(implicit conf: DbConfig): Unit

}