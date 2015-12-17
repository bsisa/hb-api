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

//import ch.bsisa.hyperbird.security.social.WithRole
//import ch.bsisa.hyperbird.security.social.WithClasseEditRight
import ch.bsisa.hyperbird.security.social.WithClasseEditRightException
//import ch.bsisa.hyperbird.security.social.WithManagerEditRight
//import ch.bsisa.hyperbird.security.social.WithManagerEditRightException


object YearlyPrestationsCreation {

  val SPLIT_REGEXP = "\\."
  
  def createPrestations(referenceYear: String, createYear: String) : Unit = {

    val xqueryFileName = "get_PRESTATION_list_for_year.xq"
    
    // TODO: add xqueryParameter for reference year as 
    val xqueryParameters = Some(s"?ANNEE=${referenceYear}")
    val refPrestations = XQueryWSHelper.runXQueryFile(xqueryFileName, None).map { response =>
      Logger.debug(s"response.ahcResponse.getContentType() = ${response.ahcResponse.getContentType()}")
      val melfinWrappedBody = "<MELFIN>" + response.body.mkString + "</MELFIN>"
      ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))
    }

    refPrestations.map { prestations =>

      val prestationMutableIndexesPerBuilding : scala.collection.mutable.HashMap[String,Int] = scala.collection.mutable.HashMap()
      
      for (prestation <- prestations ) {
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
      
      // Second pass - Make use of the prestations indexes Map and indent it while creating new prestation entries.
      val newPrestations = 
        for { prestation <- prestations } 
        yield { 
         
          val futureElfinWithId: Future[ELFIN] = ElfinDAO.getNewFromCatalogue(prestation.CLASSE)

          // Send cloned catalogue elfin in JSON format 
          futureElfinWithId.map { elfin =>
            val newObjectifBuildingPart = prestation.IDENTIFIANT.get.OBJECTIF.get.split(SPLIT_REGEXP)(0)
            // TODO: Must update MUTABLE Map... counter
            val newObjectifIndexPart = (prestationMutableIndexesPerBuilding getOrElse (newObjectifBuildingPart, 98)) + 1
            // Update Map value for key `newObjectifBuildingPart`
            prestationMutableIndexesPerBuilding(newObjectifBuildingPart) = newObjectifIndexPart
            val newObjectif = Option(s"""$newObjectifBuildingPart.$newObjectifIndexPart""")
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
            
            
            val newPrestationWithoutMutations = ELFIN(None, prestation.GEOSELECTION, Option(identifiantWithUpdatedYearAndObjectif), prestation.CARACTERISTIQUE,
            prestation.PARTENAIRE, prestation.ACTIVITE, prestation.FORME, prestation.ANNEXE, prestation.DIVERS, elfin.Id,
            elfin.ID_G, elfin.CLASSE, prestation.GROUPE, prestation.TYPE, prestation.NATURE, prestation.SOURCE) 
          
            val newPrestation = ElfinUtil.replaceElfinMutationsHead(newPrestationWithoutMutations,ElfinUtil.createMutationForUserId("PRE"))
            Logger.debug(s"newPrestation.Id = ${newPrestation.Id}")
            Logger.debug(s"newPrestation.Id = ${newPrestation.IDENTIFIANT}")

            // Create in database
            // TODO: add check for existing entry based upon building sai nb. + year + groupe + remark ?
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
  }
}