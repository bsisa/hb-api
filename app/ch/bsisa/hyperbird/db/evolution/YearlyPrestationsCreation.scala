package ch.bsisa.hyperbird.db.evolution

import java.net.ConnectException
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import scala.concurrent.Future
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.IDENTIFIANT
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper

import ch.bsisa.hyperbird.dao.ResultNotFoundException
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.security.social.WithClasseEditRightException

/**
 * Review get_PRESTATION_list_for_year.xq script logic before changing
 * the following code.
 */
object YearlyPrestationsCreation {

  val SPLIT_REGEXP = "\\."

  def createPrestations(referenceYear: String, createYear: String, owner: String): Unit = {

    val xqueryFileName = "get_PRESTATION_list_for_year.xq"

    // Create HTTP URL parameters from function parameters
    val xqueryParameters = Some(s"refYear=${referenceYear}&owner=${owner}")
    // Query PRESTATIONS for reference year and owner
    val refPrestations = XQueryWSHelper.runXQueryFile(xqueryFileName, xqueryParameters).map { response =>
      Logger.debug(s"response.ahcResponse.getContentType() = ${response.ahcResponse.getContentType()}")
      val melfinWrappedBody = "<MELFIN>" + response.body.mkString + "</MELFIN>"
      ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))
    }

    refPrestations.map { prestations =>

      val prestationMutableIndexesPerBuilding: scala.collection.mutable.HashMap[String, Int] = scala.collection.mutable.HashMap()

      // First pass - Loop on each PRESTATION to obtain the highest index per building across every owner
      for (prestation <- prestations) {
        val prestationIndexTokensOpt = prestation.IDENTIFIANT.flatMap { i => i.OBJECTIF.map { o => o.split(SPLIT_REGEXP) } }
        val prestationIndexEntry = prestationIndexTokensOpt map { prestationIndexTokens =>
          // Condition reached if prestation index is of the expected format: "195.23" 
          if (prestationIndexTokens.size == 2) {
            val buildingObjNb = prestationIndexTokens(0)
            val prestationIdx = prestationIndexTokens(1).toInt
            val currentIdx = prestationMutableIndexesPerBuilding get buildingObjNb
            currentIdx match {
              case Some(i) =>
                // Keep greatest idx value
                if (i < prestationIdx) {
                  prestationMutableIndexesPerBuilding += (buildingObjNb -> prestationIdx)
                }
              case None => prestationMutableIndexesPerBuilding += (buildingObjNb -> prestationIdx)
            }
          }
          //Logger.error(s"""PRESTATION Id $prestation.Id WITH STRANGE OBJECTIF = $prestation.IDENTIFIANT.get.OBJECTIF.getOrElse("NO OBJECTIF")""")
        }
      }

      Logger.debug(s"prestationMutableIndexesPerBuilding.keySet.size = ${prestationMutableIndexesPerBuilding.keySet.size}")
      Logger.debug(s"prestationMutableIndexesPerBuilding = ${prestationMutableIndexesPerBuilding}")

      // Second pass - Make use of the prestations indexes Map and indent it while creating new prestation entries restricted to defined owner.
      val newPrestations =
        for { prestation <- prestations.filter { prestation => prestation.PARTENAIRE.get.PROPRIETAIRE.get.NOM.get == owner } } yield {

          val newObjectifBuildingPart = prestation.IDENTIFIANT.get.OBJECTIF.get.split(SPLIT_REGEXP)(0)
          val newObjectifIndexPart = (prestationMutableIndexesPerBuilding getOrElse (newObjectifBuildingPart, 98)) + 1

          // WARNING: prestationMutableIndexesPerBuilding is a mutable data structure. Former logic updating its state, that is 
          // incrementing objectif index part by one, from within `futureElfinWithId.map` call lead to non deterministic behaviour
          // as prestationMutableIndexesPerBuilding could be queried for current objectifIndexPart while not yet updated from 
          // future call execution leading to wrong results! Now all mutable structure logic is applied before call to a dedicated
          // function `createPrestation` whose parameters are guaranteed immutable.

          // Update mutable Map value for key `newObjectifBuildingPart`
          prestationMutableIndexesPerBuilding(newObjectifBuildingPart) = newObjectifIndexPart

          // Build new objectif string such as "195.30"
          val newObjectif = Option(s"""$newObjectifBuildingPart.$newObjectifIndexPart""")

          // Create immutable IDENTIFIANT using new objectif and year.
          val identifiantWithUpdatedYearAndObjectif = IDENTIFIANT(
            AUT = prestation.IDENTIFIANT.get.AUT,
            GER = prestation.IDENTIFIANT.get.GER,
            RES = prestation.IDENTIFIANT.get.RES,
            NOM = prestation.IDENTIFIANT.get.NOM,
            ALIAS = prestation.IDENTIFIANT.get.ALIAS,
            ORIGINE = prestation.IDENTIFIANT.get.ORIGINE,
            OBJECTIF = newObjectif,
            QUALITE = prestation.IDENTIFIANT.get.QUALITE,
            COMPTE = prestation.IDENTIFIANT.get.COMPTE,
            DE = Option(createYear),
            A = prestation.IDENTIFIANT.get.A,
            PAR = prestation.IDENTIFIANT.get.PAR,
            VALEUR_A_NEUF = prestation.IDENTIFIANT.get.VALEUR_A_NEUF,
            VALEUR = prestation.IDENTIFIANT.get.VALEUR)

          // Perform asynchronous (Future) prestation creation in isolation.   
          createPrestation(newIdentifiant = identifiantWithUpdatedYearAndObjectif, prestation = prestation)

        }
    }
  }

  /**
   * Performs creation of a new ELFIN object expected of ELFIN.CLASSE `PRESTATION`.
   */
  private def createPrestation(newIdentifiant: IDENTIFIANT, prestation: ELFIN): Unit = {

    val futureElfinWithId: Future[ELFIN] = ElfinDAO.getNewFromCatalogue(prestation.CLASSE)

    futureElfinWithId.map { elfin =>

      val newPrestationWithoutMutations = ELFIN(None, prestation.GEOSELECTION, Option(newIdentifiant), prestation.FILIATION, prestation.CARACTERISTIQUE,
        prestation.PARTENAIRE, prestation.ACTIVITE, prestation.FORME, prestation.ANNEXE, prestation.DIVERS, elfin.Id,
        elfin.ID_G, elfin.CLASSE, prestation.GROUPE, prestation.TYPE, prestation.NATURE, prestation.SOURCE)

      val newPrestation = ElfinUtil.replaceElfinMutationsHead(newPrestationWithoutMutations, ElfinUtil.createMutationForUserId("PRE"))
      Logger.debug(s"newPrestation.Id = ${newPrestation.Id}")
      Logger.debug(s"newPrestation.IDENTIFIANT = ${newPrestation.IDENTIFIANT}")

      // TODO: Evaluate possible improvement making database entry creation conditional to 
      // no existing entry with identical {IDENTIFIANT/0BJECTIF building part, year, groupe, remark}

      // Create in database
      ElfinDAO.create(newPrestation)
    }
      .recover {
        case e: WithClasseEditRightException =>
          val errorMsg = s"Failed to obtain Elfin with CLASSE: ${prestation.CLASSE} from catalogue: ${e}"
          Logger.error(errorMsg)
        case resNotFound: ResultNotFoundException => {
          val errorMsg = s"Failed to obtain new ELFIN from catalogue for classeName: ${prestation.CLASSE}: ${resNotFound}"
          Logger.error(errorMsg)
        }
        case connectException: ConnectException => {
          val errorMsg = s"No database connection could be established."
          Logger.error(errorMsg)
        }
        case e: Throwable => {
          val errorMsg = s"Failed to obtain new ELFIN from catalogue for classeName: ${prestation.CLASSE}: ${e}"
          Logger.error(errorMsg)
        }
      }

  }

}