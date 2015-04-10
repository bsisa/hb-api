package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import play.api.libs.concurrent.Execution.Implicits._
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.dao.ElfinDAO
import scala.concurrent.Future
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.patman.simulations.Constants
import ch.bsisa.hyperbird.patman.simulations.model.Bed
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.model.IDENTIFIANT
import ch.bsisa.hyperbird.util.DateUtil
import java.util.Date

class SimulatedHospitalStateReportActor(simulationId: String) extends Actor with ActorLogging {

  def receive = {

    case SimulatedHospitalState(hospitalState) =>
      log.info(s"SimulatedHospitalState notified for ${hospitalState.code}  at ${hospitalState.schedule}")

      val futureHospitalStateElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("HOSPITAL_STATE")
      futureHospitalStateElfin.map { elfinHospitalStateTemplate =>

        try {

          val elfinHospitalStateFuture = buildHospitalStateElfin(
            elfinHospitalStateTemplate = elfinHospitalStateTemplate,
            simulationId = simulationId,
            hospitalState = hospitalState)
          // Update database 
          elfinHospitalStateFuture.map { elfinHospitalState =>
            ElfinDAO.create(elfinHospitalState)
          }

        } catch {
          case e: Throwable => log.error(s"SimulatedHospitalStateReportActor complaining: ${e}")
        }

      }

    case DataSetEmpty =>
      sender ! WorkCompleted("TransferReportActor")

  }

  /**
   * Builds `ELFIN` of CLASSE='HOSPITAL_STATE' for `Hospital`
   */
  def buildHospitalStateElfin(elfinHospitalStateTemplate: ELFIN, simulationId: String, hospitalState: Hospital): Future[ELFIN] = {

    val ELFIN_SIMULATION_NATURE = "simulation"
    val ELFIN_SIMULATION_ID_G = "G20150114160000006"
    
    val elfinHospitalStateWithIdFuture: Future[ELFIN] = ElfinUtil.assignElfinId(elfinHospitalStateTemplate)
    
    val elfinHospitalStateWithBedsFuture = elfinHospitalStateWithIdFuture.map { elfinHospitalStateWithId =>
      // Assign ID_G: G20150114160000006 to have ELFIN_SIMULATION_NATURE store in a collection distinct 
      // from end users recorded HOSPITAL_STATE ELFINs.
      val elfinHospitalStateWithUpdatedID_G = ElfinUtil.replaceElfinID_G(elfinHospitalStateWithId, ELFIN_SIMULATION_ID_G)
      
      val elfinHospitalStateWithNewNatureGroupeSource = ElfinUtil.replaceElfinNatureGroupeSource(elfin = elfinHospitalStateWithUpdatedID_G, newNature = ELFIN_SIMULATION_NATURE, newGroupe = elfinHospitalStateWithUpdatedID_G.GROUPE, newSource = Some(simulationId))
      val bedsHospitalWrapperElfin = HospitalHelper.toElfin(hospitalState)
      val identifiantHospitalState = IDENTIFIANT(AUT = Some("FluxPatients - Simulator"), NOM = None, ORIGINE = None, OBJECTIF = None, DE = Option(DateUtil.getIsoDateFormatterWithoutTz.format(hospitalState.schedule)))
      val elfinHospitalStateWithIdentifiant = ElfinUtil.replaceElfinIdentifiant(elfinHospitalStateWithNewNatureGroupeSource, identifiantHospitalState)
      // TODO: we need L[0] to match Hospital meta-data unlike TRANSFER 
      val elfinHospitalStateWithBeds = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfinHospitalStateWithIdentifiant, bedsHospitalWrapperElfin.CARACTERISTIQUE.get.FRACTION.get.L)
      elfinHospitalStateWithBeds
    }

    elfinHospitalStateWithBedsFuture
  }

}