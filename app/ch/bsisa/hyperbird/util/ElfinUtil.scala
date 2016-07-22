package ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.model.CARACTERISTIQUE
import ch.bsisa.hyperbird.model.FORME
import ch.bsisa.hyperbird.model.NOM
import ch.bsisa.hyperbird.model.IDENTIFIANT
import ch.bsisa.hyperbird.model.MUTATION
import ch.bsisa.hyperbird.model.MUTATIONS
import ch.bsisa.hyperbird.model.PARTENAIRE
import ch.bsisa.hyperbird.model.PERSONNEType
import ch.bsisa.hyperbird.model.POINT
import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.model.MATRICEType
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
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
  def assignElfinId(elfin: ELFIN): Future[ELFIN] = {
    val newElfinIdFuture: Future[String] = ElfinIdGenerator.getNewElfinId
    val futureElfin: Future[ELFIN] = newElfinIdFuture.map { newElfinId =>
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
   * Creates a `MUTATION` object with `DATE` attribute set to current date and `UTILISATEUR` attribute set to provided userId value.
   * Attribute `ROLE` is set to empty string and `MOT_DE_PASSE` defaults to None.
   */
  def createMutationForUserId(userId: String): MUTATION = {
    MUTATION(DATE = ch.bsisa.hyperbird.util.DateUtil.elfinIdentifiantDateFormat.format(new Date()), ROLE = "", MOT_DE_PASSE = None, UTILISATEUR = Some(userId))
  }

  /**
   * Replaces ELFIN.MUTATIONS head MUTATION value of the provided elfin with newElfinMutation leaving all other elfin
   * information unchanged. The returned elfin is a new elfin instance.
   */
  def replaceElfinMutationsHead(elfin: ELFIN, newElfinMutation: MUTATION): ELFIN = {

    val newMutations = elfin.MUTATIONS match {
      // Mutations exist
      case Some(muts) => muts.MUTATION.headOption match {
        // Mutations has at least one entry, preserve tail in case it is non empty.
        case Some(mut) => MUTATIONS(MUTATION = (newElfinMutation :: muts.MUTATION.tail.toList))
        // Mutations was empty, create from scratch
        case None      => MUTATIONS(MUTATION = Seq(newElfinMutation))
      }
      // No mutations create from scratch
      case None => MUTATIONS(MUTATION = Seq(newElfinMutation))
    }

    ELFIN(Some(newMutations), elfin.GEOSELECTION, elfin.IDENTIFIANT, elfin.CARACTERISTIQUE,
      elfin.PARTENAIRE, elfin.ACTIVITE, elfin.FORME, elfin.ANNEXE, elfin.DIVERS, elfin.Id,
      elfin.ID_G, elfin.CLASSE, elfin.GROUPE, elfin.TYPE, elfin.NATURE, elfin.SOURCE)
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
    ELFIN(elfin.MUTATIONS, elfin.GEOSELECTION, elfin.IDENTIFIANT, Some(CARACTERISTIQUE(FRACTION = Some(MATRICEType(newLSeq)))),
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
      AUT = elfinUser.IDENTIFIANT.get.AUT,
      GER = elfinUser.IDENTIFIANT.get.GER,
      RES = elfinUser.IDENTIFIANT.get.RES,
      NOM = elfinUser.IDENTIFIANT.get.NOM,
      ALIAS = Option(userPwdInfo),
      ORIGINE = elfinUser.IDENTIFIANT.get.ORIGINE,
      OBJECTIF = elfinUser.IDENTIFIANT.get.OBJECTIF,
      QUALITE = elfinUser.IDENTIFIANT.get.QUALITE,
      COMPTE = elfinUser.IDENTIFIANT.get.COMPTE,
      DE = elfinUser.IDENTIFIANT.get.DE,
      A = elfinUser.IDENTIFIANT.get.A,
      PAR = elfinUser.IDENTIFIANT.get.PAR,
      VALEUR_A_NEUF = elfinUser.IDENTIFIANT.get.VALEUR_A_NEUF,
      VALEUR = elfinUser.IDENTIFIANT.get.VALEUR)

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

  /**
   * Helper returning Option[CARSET_CARType] for Scala find equivalent to XPath:
   * CARACTERISTIQUE/CARSET/CAR[@NOM = `name`] applied to `elfin`.
   */
  def getElfinCarByName(elfin: ELFIN, name: String) = {
    val carForNameOption = elfin.CARACTERISTIQUE.flatMap { _.CARSET.flatMap { _.CAR.find { _.NOM.getOrElse(false) == name } } }
    carForNameOption
  }

  def updateElfinForme(elfin: ELFIN, forme: FORME): ELFIN = {
    val elfinForMap = ELFIN(
      MUTATIONS = elfin.MUTATIONS, GEOSELECTION = elfin.GEOSELECTION,
      IDENTIFIANT = elfin.IDENTIFIANT, CARACTERISTIQUE = elfin.CARACTERISTIQUE,
      PARTENAIRE = elfin.PARTENAIRE,
      ACTIVITE = elfin.ACTIVITE,
      FORME = Some(forme),
      ANNEXE = elfin.ANNEXE,
      DIVERS = elfin.DIVERS,
      Id = elfin.Id,
      ID_G = elfin.ID_G,
      CLASSE = elfin.CLASSE,
      GROUPE = elfin.GROUPE,
      TYPE = elfin.TYPE,
      NATURE = elfin.NATURE,
      SOURCE = elfin.SOURCE)

    elfinForMap
  }

  /**
   * Replaces provided `forme` sequence of `POINT` by provided `points` sequence leaving LIGNE, ZONE, SYMBOLE unchanged.
   */
  private def replaceOrCreateFormePoints(formeOpt: Option[FORME], points: Seq[ch.bsisa.hyperbird.model.POINT]): FORME = {

    val newForme = formeOpt match {
      case Some(forme) => FORME(POINT = points,
        LIGNE = forme.LIGNE,
        ZONE = forme.ZONE,
        SYMBOLE = forme.SYMBOLE)
      case None => FORME(POINT = points)
    }
    newForme
  }

  /**
   * Replaces provided `forme` sequence of `POINT` by provided `points` sequence leaving all other ELFIN properties unchanged.
   */
  def updateElfinPoints(elfin: ELFIN, points: Seq[ch.bsisa.hyperbird.model.POINT]): ELFIN = {

    val updatedElfin = ELFIN(
      MUTATIONS = elfin.MUTATIONS, GEOSELECTION = elfin.GEOSELECTION,
      IDENTIFIANT = elfin.IDENTIFIANT, CARACTERISTIQUE = elfin.CARACTERISTIQUE,
      PARTENAIRE = elfin.PARTENAIRE,
      ACTIVITE = elfin.ACTIVITE,
      FORME = Some(replaceOrCreateFormePoints(elfin.FORME, points)),
      ANNEXE = elfin.ANNEXE,
      DIVERS = elfin.DIVERS,
      Id = elfin.Id,
      ID_G = elfin.ID_G,
      CLASSE = elfin.CLASSE,
      GROUPE = elfin.GROUPE,
      TYPE = elfin.TYPE,
      NATURE = elfin.NATURE,
      SOURCE = elfin.SOURCE)

    updatedElfin
  }

  /**
   * Replaces `XG`, `YG`, `ZG` `point` coordinates with provided `xg`, `yg`, `zg` coordinates.
   */
  def updatePointGpsCoordinates(point: POINT, xg: Option[Double], yg: Option[Double], zg: Option[Double]): POINT = {
    POINT(POS = point.POS,
      X = point.X,
      Y = point.Y,
      Z = point.Z,
      XG = xg,
      YG = yg,
      ZG = zg,
      KSI = point.KSI,
      ANGLE = point.ANGLE,
      ALPHA = point.ALPHA,
      XS = point.XS,
      YS = point.YS,
      ZS = point.ZS,
      KSIS = point.KSIS,
      ANGLES = point.ANGLES,
      ALPHAS = point.ALPHAS,
      Id = point.Id,
      ID_G = point.ID_G,
      FONCTION = point.FONCTION,
      CLASSE = point.CLASSE,
      GROUPE = point.GROUPE)
  }

  /**
   * Streamline ELFIN information to preserve only information relevant to map usage. (Actview POC)
   *
   * WARNING: Intended information loss.
   */
  def getElfinForMap(elfinFull: ELFIN): ELFIN = {
    val elfinForMap = ELFIN(
      MUTATIONS = None, GEOSELECTION = elfinFull.GEOSELECTION,
      IDENTIFIANT = elfinFull.IDENTIFIANT, CARACTERISTIQUE = None,
      PARTENAIRE = None, //PARTENAIRE = elfinFull.PARTENAIRE
      ACTIVITE = elfinFull.ACTIVITE,
      FORME = elfinFull.FORME,
      ANNEXE = None, //ANNEXE = elfinFull.ANNEXE,
      DIVERS = None, //DIVERS = elfinFull.DIVERS,
      Id = elfinFull.Id,
      ID_G = elfinFull.ID_G,
      CLASSE = elfinFull.CLASSE,
      GROUPE = elfinFull.GROUPE,
      TYPE = elfinFull.TYPE,
      NATURE = elfinFull.NATURE,
      SOURCE = elfinFull.SOURCE)

    elfinForMap
  }

}