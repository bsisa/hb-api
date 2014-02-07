package ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.ElfinIdGenerator

/**
 * Utility functions to deal with ELFIN immutable state replacement such as updating
 * the Id or ID_G ELFIN attributes.
 *
 * @author Patrick Refondini
 *
 */
object ElfinUtil {

  /**
   * Replaces the value Id of the provided elfin with a newly generated Id leaving all 
   * other elfin information unchanged. The returned elfin is a new elfin instance.
   */
  def assignElfinId(elfin: ELFIN): ELFIN = {
    val newElfinId = ElfinIdGenerator.getNewElfinId
    new ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, elfin.CARACTERISTIQUE,
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, newElfinId,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }

  /**
   * Replaces the value ID_G of the provided elfin with newElfinID_G leaving all other elfin
   * information unchanged. The returned elfin is a new elfin instance.
   */
  def replaceElfinID_G(elfin: ELFIN, newElfinID_G: String): ELFIN = {

    new ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, elfin.CARACTERISTIQUE,
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      newElfinID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }

}