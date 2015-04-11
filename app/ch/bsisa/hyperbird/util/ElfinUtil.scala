package ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.model.CARACTERISTIQUE
import ch.bsisa.hyperbird.model.NOM
import ch.bsisa.hyperbird.model.IDENTIFIANT
import java.util.Date
import ch.bsisa.hyperbird.model.PARTENAIRE
import ch.bsisa.hyperbird.model.PERSONNEType
import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.model.MATRICEType
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._

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
  def assignElfinId(elfin: ELFIN): Future[ELFIN] = {
    val newElfinIdFuture : Future[String] = ElfinIdGenerator.getNewElfinId
    val futureElfin : Future[ELFIN] = newElfinIdFuture.map { newElfinId =>
      ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, elfin.CARACTERISTIQUE,
        elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, newElfinId,
        elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
    }
    futureElfin
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
   * Replaces the values of elfin attributes NATURE, GROUPE, SOURCE of the provided elfin with
   * the new provided ones leaving all other elfin information unchanged.
   *
   * The returned elfin is a new elfin instance.
   */
  def replaceElfinNatureGroupeSource(elfin: ELFIN, newNature: String, newGroupe: Option[String], newSource: Option[String]): ELFIN = {

    ELFIN(MUTATIONS = elfin.MUTATIONS, GEOSELECTION = elfin.GEOSELECTION, IDENTIFIANT = elfin.IDENTIFIANT, CARACTERISTIQUE = elfin.CARACTERISTIQUE,
      PARTENAIRE = elfin.PARTENAIRE, ACTIVITE = elfin.ACTIVITE, FORME = elfin.FORME, ANNEXE = elfin.ANNEXE, DIVERS = elfin.DIVERS, Id = elfin.Id,
      ID_G = elfin.ID_G, CLASSE = elfin.CLASSE, GROUPE = newGroupe, TYPE = elfin.TYPE, NATURE = newNature, SOURCE = newSource)
  }

  
  /**
   * Replaces the value elfin.CARACTERISTIQUE by an new CARACTERISTIQUE. 
   * The rest of the elfin information stays unchanged. 
   * The returned elfin is a new elfin instance.
   */
  def replaceElfinCaracteristique(elfin: ELFIN, newCaracteristique: ch.bsisa.hyperbird.model.CARACTERISTIQUE): ELFIN = {
    ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, Some(newCaracteristique),
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }  
  
  
  /**
   * Replaces the value elfin.CARACTERISTIQUE by an new CARACTERISTIQUE only containing FRACTION made of
   * the provided `newLSeq`. Any other CARACTERISTIQUE element is lost. The rest of the elfin information
   * stays unchanged. The returned elfin is a new elfin instance.
   */
  def replaceElfinCaracteristiqueFractionL(elfin: ELFIN, newLSeq: Seq[ch.bsisa.hyperbird.model.L]): ELFIN = {
    ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, Some(CARACTERISTIQUE(FRACTION = Some(MATRICEType(newLSeq: _*)))),
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
  }

  /**
   * Replaces the value elfin.IDENTIFIANT by newIdentifiant and returns a new ELFIN
   */
  def replaceElfinIdentifiant(elfin: ELFIN, newIdentifiant: IDENTIFIANT): ELFIN = {
    ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, Option(newIdentifiant), elfin.CARACTERISTIQUE,
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)

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

  /**
   * Replaces the elfin.IDENTIFIANT value by an IDENTIFIANT containing the provided user
   * specific values instead.
   */
  def replaceElfinUserPasswordInfo(
    elfinUser: ELFIN, userPwdInfo: String)(implicit collectionsConfig: CollectionsConfig) = {

    val userIdentifiant = IDENTIFIANT(
      NOM = elfinUser.IDENTIFIANT.get.NOM,
      ALIAS = Option(userPwdInfo),
      DE = elfinUser.IDENTIFIANT.get.DE,
      A = elfinUser.IDENTIFIANT.get.A)

    ELFIN(
      MUTATIONS = elfinUser.MUTATIONS, GEOSELECTION = elfinUser.GEOSELECTION,
      IDENTIFIANT = Option(userIdentifiant), CARACTERISTIQUE = elfinUser.CARACTERISTIQUE,
      elfinUser.PARTENAIRE,
      elfinUser.ACTIVITE,
      elfinUser.FORME,
      elfinUser.ANNEXE,
      elfinUser.DIVERS,
      elfinUser.Id,
      ID_G = elfinUser.ID_G,
      CLASSE = elfinUser.CLASSE,
      GROUPE = elfinUser.GROUPE,
      TYPE = elfinUser.TYPE,
      NATURE = elfinUser.NATURE,
      SOURCE = elfinUser.SOURCE)
  }

}