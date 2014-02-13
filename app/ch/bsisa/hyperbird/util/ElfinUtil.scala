package ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.model.NOM
import ch.bsisa.hyperbird.model.IDENTIFIANT

import java.util.Date

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

  /**
   * Replaces the elfin.IDENTIFIANT value by an IDENTIFIANT containing the provided user 
   * specific values instead.
   */
  def replaceElfinUserProperties(
      elfin: ELFIN, userName: String, userPwdInfo: String, validFrom: Date, validUntil: Date, 
      personId: String, personID_G: String) = {

    val dateDE = DateUtil.elfinIdentifiantDateFormat.format(validFrom)
    val dateA = DateUtil.elfinIdentifiantDateFormat.format(validUntil)

    val userIdentifiant = new IDENTIFIANT(NOM = Option(userName), ALIAS = Option(userPwdInfo), DE = Option(dateDE), A = Option(dateA))
    new ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, Option(userIdentifiant), elfin.CARACTERISTIQUE,
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }

}