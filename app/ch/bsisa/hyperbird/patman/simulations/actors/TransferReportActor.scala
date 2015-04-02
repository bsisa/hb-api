package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import play.api.libs.concurrent.Execution.Implicits._
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.dao.ElfinDAO
import scala.concurrent.Future
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.model.IDENTIFIANT
import ch.bsisa.hyperbird.util.DateUtil

class TransferReportActor extends Actor with ActorLogging {

  def receive = {

    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferReportActor notified of transfer from ${fromHospitalCode} to ${toHospitalCode} at ${fromSchedule}")
      //      val transferEvenElfin = ELFIN
      //      ElfinDAO.create(elfin)
      val futureTransferElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("TRANSFER")
      futureTransferElfin.map { elfinTransferTemplate =>

        try {

          log.info(s">>>> OBTAINED TRANSFER FROM CATALOGUE:\n ${elfinTransferTemplate}")

          val elfinTransfer = ElfinUtil.assignElfinId(elfinTransferTemplate)
          val incomingAndtypeScToSiBeds = incomingSiBeds ++ typeScToSiBeds

          val incomingAndTypeScToSiHospitalWrapper = Hospital(code = fromHospitalCode, schedule = fromSchedule, beds = incomingAndtypeScToSiBeds)
          val incomingAndTypeScToSiHospitalWrapperElfin = HospitalHelper.toElfin(incomingAndTypeScToSiHospitalWrapper)

          /*
      <IDENTIFIANT>
            <!-- Auteur de la saisie -->
            <AUT/>
            <!-- Id de la SIMULATION -->
            <NOM/>
            <!-- Code hopital FROM (Stable) -->
            <ORIGINE/>
            <!-- Code hopital TO (Stable) -->
            <OBJECTIF/>
            <!-- Date de transfer (correspond à l'horaire qui déclanche le transfert) expected values are at 08:00, 16:00 and 22:00 every day -->
            <DE/>
            <!-- Date de saisie effective YYYY-MM-dd HH:mm -->
            <A/>
           */
          // NATURE={add, remove}
          val identifiantTransfer = IDENTIFIANT(AUT = Some("FluxPatients - Simulator"), NOM = Option(id), ORIGINE = Option(fromHospitalCode), OBJECTIF = Option(toHospitalCode),DE = Option(DateUtil.getIsoDateFormatterWithoutTz.format(fromSchedule) ) )
          val elfinTransferWithIdentifiant = ElfinUtil.replaceElfinIdentifiant(elfinTransfer, identifiantTransfer)
          val elfinTransferWithBeds = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfinTransferWithIdentifiant, incomingAndTypeScToSiHospitalWrapperElfin.CARACTERISTIQUE.get.FRACTION.get.L)

          // Update database with new elfin
          ElfinDAO.create(elfinTransferWithBeds)
        } catch {
          case e: Throwable => log.error(s"TransferReportActor complaining: ${e}")
          case z: Any => log.error(s"TransferReportActor complaining with Any caught: ${z}")
        }

      }

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferResponse id ${id} from ${fromHospitalCode} to ${toHospitalCode} status = ${status}")
    // We could write to database if status is not successful...

    case DataSetEmpty =>
      sender ! WorkCompleted("TransferReportActor")

  }

}