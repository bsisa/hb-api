package ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.model.NOM
import ch.bsisa.hyperbird.model.IDENTIFIANT
import java.util.Date
import ch.bsisa.hyperbird.model.PARTENAIRE
import ch.bsisa.hyperbird.model.PERSONNEType
import ch.bsisa.hyperbird.CollectionsConfig

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
    ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, elfin.CARACTERISTIQUE,
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, newElfinId,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }

  /**
   * Replaces the value ID_G of the provided elfin with newElfinID_G leaving all other elfin
   * information unchanged. The returned elfin is a new elfin instance.
   */
  def replaceElfinID_G(elfin: ELFIN, newElfinID_G: String): ELFIN = {

    ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, elfin.CARACTERISTIQUE,
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      newElfinID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }

  /**
   * Replaces the elfin.IDENTIFIANT value by an IDENTIFIANT containing the provided user
   * specific values instead.
   */
  def replaceElfinUserProperties(
    elfinUser: ELFIN, userName: String, userPwdInfo: String, validFrom: Date, validUntil: Date,
    personId: String, personID_G: String)(implicit collectionsConfig: CollectionsConfig) = {

    val elfin = ElfinUtil.replaceElfinID_G(elfinUser, newElfinID_G = collectionsConfig.configurationCollectionId)
    
    val dateDE = DateUtil.elfinIdentifiantDateFormat.format(validFrom)
    val dateA = DateUtil.elfinIdentifiantDateFormat.format(validUntil)

    val userDetailsReference = PERSONNEType(mixed = Nil,
      Id = Option(personId),
      ID_G = Option(personID_G),
      NOM = None,
      GROUPE = None)

    val userDetailsContainer = PARTENAIRE(GERANT = None,
      USAGER = Option(userDetailsReference),
      FOURNISSEUR = None,
      PROPRIETAIRE = None)

    val userIdentifiant = IDENTIFIANT(NOM = Option(userName), ALIAS = Option(userPwdInfo), DE = Option(dateDE), A = Option(dateA))
    ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, Option(userIdentifiant), elfin.CARACTERISTIQUE,
      Option(userDetailsContainer), elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }

}