package ch.bsisa.hyperbird.patman.simulations.model

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.ElfinDAO

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.util.ElfinUtil

import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper

import java.util.Date
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Logger
import scala.concurrent.Future
import ch.bsisa.hyperbird.patman.simulations.Constants
import ch.bsisa.hyperbird.model.IDENTIFIANT

/**
 *  Helper to go from ELFIN to Hospital and reverse.
 *
 * <L POS="5">
 *   <C POS="1">503B</C>
 *   <C POS="2">4195904</C>
 *   <C POS="3">soins continus</C>
 *   <C POS="4">médicalisé</C>
 *   <C POS="5">terminé</C>
 *   <C POS="6">occupé</C>
 *   <C POS="7"/>
 * </L>
 *
 */
object HospitalHelper {

  val logger = Logger("ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper")

  /**
   * Converts generic ELFIN type for specific CLASSE='HOSPITAL_STATE' to semantic type Hospital
   */
  def toHospital(elfinHospitalState: ELFIN): Hospital = {
    val hospitalCode = getMixedContent(elfinHospitalState.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed)
    val scheduleStr = elfinHospitalState.IDENTIFIANT.get.DE.get
    //play.api.Logger.info("scheduleStr = " + scheduleStr)
    val schedule = DateUtil.getIsoDateFormatterWithoutTz.parse(scheduleStr)
    val beds = for {
      (l, index) <- (elfinHospitalState.CARACTERISTIQUE.get.FRACTION.get.L.toList zipWithIndex) if (index > 0) // First L does not contain bed information
    } yield {
      val c = l.C
      val bedId = getMixedContent(c(0).mixed)
      val bedFree = (getMixedContent(c(5).mixed) == BED_FREE_CODE)
      val patientNb = getMixedContent(c(1).mixed)
      val patientType = getMixedContent(c(2).mixed)
      val transferType = getMixedContent(c(3).mixed)
      val bed = Bed(bedId, bedFree, patientNb, patientType, transferType)
      bed
    }

    Hospital(hospitalCode, schedule, beds)
  }

  /**
   * Converts `Hospital` instance to ELFIN generic GeoXML representation.
   * We consider beds data built programmatically as always having input state 
   * as completed {`terminé`} not pending {`en cours`}. This helps reporting
   * queries reuse.  
   * 
   */
  def toElfin(hospital: Hospital, bedsPosStartIndex: Int = 1): ELFIN = {

    val bedsXmlElem = for ((bed, i) <- hospital.beds zipWithIndex) yield {

      <!-- Identification lit -->
      val bedXmlElem = <L POS={ (i + bedsPosStartIndex).toString }>
                         <!-- Numéro lit -->
                         <C POS="1">{ bed.id }</C>
                         <C POS="2">{ bed.patientNb }</C>
                         <C POS="3">{ bed.patientType }</C>
                         <C POS="4">{ bed.transferType }</C>
                         <C POS="5">{ Constants.BED_COMPLETED_INPUT }</C>
                         <C POS="6">{ if (bed.free) "libre" else "occupé" }</C>
                         <C POS="7">{ bed.reasonForTransfer.getOrElse("") }</C>
                       </L>
      bedXmlElem
    }

    val hospitalElfinTemplateXmlElem = <ELFIN Id="N/A" ID_G="N/A" CLASSE="HOSPITAL" GROUPE="" TYPE="BIEN" NATURE="">
                                         <IDENTIFIANT>
                                           <AUT/>
                                           <NOM>N/A</NOM>
                                           <ALIAS>{ hospital.code }</ALIAS>
                                           <DE>{ DateUtil.getIsoDateFormatterWithoutTz.format(hospital.schedule) }</DE>
                                         </IDENTIFIANT>
                                         <CARACTERISTIQUE>
                                           <!-- Liste des lits de l'hopital -->
                                           <FRACTION>
                                             { bedsXmlElem }
                                           </FRACTION>
                                         </CARACTERISTIQUE>
                                         <DIVERS>
                                           <REMARQUE/>
                                         </DIVERS>
                                       </ELFIN>

    ElfinFormat.fromXml(hospitalElfinTemplateXmlElem)
  }

  /**
   * Shortcut function for repetitive Bed.patientType matches Constants.PATIENT_TYPE_SI test
   */
  def isBedPatientTypeSi(bed: Bed): Boolean = (bed.patientType == PATIENT_TYPE_SI)

  /**
   * Sets `reasonForTransfer` to all beds
   */
  def setReasonForTransfer(beds: List[Bed], reasonForTransfer: String): List[Bed] = {
    val bedsWithReasonForTransfer = for (bed <- beds) yield { Bed(bed.id, bed.free, bed.patientNb, bed.patientType, bed.transferType, Some(reasonForTransfer)) }
    bedsWithReasonForTransfer
  }

  /**
   * Returns a pair of Seq[Bed]. The first one contains incoming SI patients while the second contains incoming SC patients.
   */
  def getBedsWithIncomingPatient(previousCdfStateOption: Option[Hospital], previousSimulatedPrtHospitalStateOption: Option[Hospital], currentStateOption: Option[Hospital]): (List[Bed], List[Bed]) = {

    previousCdfStateOption match {
      case Some(previousCdfState) =>

        previousSimulatedPrtHospitalStateOption match {
          case Some(previousSimulatedPrtHospitalState) =>

            currentStateOption match {
              // previous CDF, previous sim PRT and current CDF available
              case Some(currentCdfState) => {

                // Incoming patients 
                val bedsWithIncomingPatients = currentCdfState.beds.filter { currentCdfStateBed =>
                  if (!currentCdfStateBed.free) {
                    // Check if the current patient was already there at CDF
                    val existingBedAtCdf = previousCdfState.beds.find(previousStateBed => currentCdfStateBed.patientNb == previousStateBed.patientNb)
                    // Check if the current patient was already there at PRT
                    val existingBedAtPrt = previousSimulatedPrtHospitalState.beds.find(previousSimulatedPrtStateBed => currentCdfStateBed.patientNb == previousSimulatedPrtStateBed.patientNb)
                    // Return true if was not previously there
                    (existingBedAtCdf == None && existingBedAtPrt == None)
                  } else {
                    // Skip empty bed
                    false
                  }
                }
                val bedsWithIncomingPatientTypeSi = bedsWithIncomingPatients.filter(isBedPatientTypeSi)
                val bedsWithIncomingPatientTypeSc = bedsWithIncomingPatients.filterNot(isBedPatientTypeSi)
                val bedsWithIncomingPatientTypeSiAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSi, Constants.BED_REASON_FOR_TRANSFER_SI)
                val bedsWithIncomingPatientTypeScAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSc, Constants.BED_REASON_FOR_TRANSFER_SC)
                (bedsWithIncomingPatientTypeSiAndReasonForTransfer, bedsWithIncomingPatientTypeScAndReasonForTransfer)
              }
              // previous available but no current: Nothing is incoming.          
              case None => (List(), List())
            }

          case None =>
            currentStateOption match {
              // previous and current available
              case Some(currentState) => {
                // Incoming patients 
                val bedsWithIncomingPatients = currentState.beds.filter { currentStateBed =>
                  if (!currentStateBed.free) {
                    // Check if the current patient was already there
                    val existingBed = previousCdfState.beds.find(previousStateBed => currentStateBed.patientNb == previousStateBed.patientNb)
                    // Return true if was not previously there
                    (existingBed == None)
                  } else {
                    // Skip empty bed
                    false
                  }
                }
                val bedsWithIncomingPatientTypeSi = bedsWithIncomingPatients.filter(isBedPatientTypeSi)
                val bedsWithIncomingPatientTypeSc = bedsWithIncomingPatients.filterNot(isBedPatientTypeSi)
                //(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc)
                val bedsWithIncomingPatientTypeSiAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSi, Constants.BED_REASON_FOR_TRANSFER_SI)
                val bedsWithIncomingPatientTypeScAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSc, Constants.BED_REASON_FOR_TRANSFER_SC)
                (bedsWithIncomingPatientTypeSiAndReasonForTransfer, bedsWithIncomingPatientTypeScAndReasonForTransfer)
              }
              // previous available but no current: Nothing is incoming.          
              case None => (List(), List())
            }
        }

      case None =>
        previousSimulatedPrtHospitalStateOption match {
          case Some(previousSimulatedPrtHospitalState) =>
            currentStateOption match {
              // previous simulated PRT and current available
              case Some(currentState) => {
                // Incoming patients 
                val bedsWithIncomingPatients = currentState.beds.filter { currentStateBed =>
                  if (!currentStateBed.free) {
                    // Check if the current patient was already there
                    val existingBed = previousSimulatedPrtHospitalState.beds.find(previousStateBed => currentStateBed.patientNb == previousStateBed.patientNb)
                    // Return true if was not previously there
                    (existingBed == None)
                  } else {
                    // Skip empty bed
                    false
                  }
                }
                val bedsWithIncomingPatientTypeSi = bedsWithIncomingPatients.filter(isBedPatientTypeSi)
                val bedsWithIncomingPatientTypeSc = bedsWithIncomingPatients.filterNot(isBedPatientTypeSi)
                //(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc)
                val bedsWithIncomingPatientTypeSiAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSi, Constants.BED_REASON_FOR_TRANSFER_SI)
                val bedsWithIncomingPatientTypeScAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSc, Constants.BED_REASON_FOR_TRANSFER_SC)
                (bedsWithIncomingPatientTypeSiAndReasonForTransfer, bedsWithIncomingPatientTypeScAndReasonForTransfer)
              }
              // previous available but no current: Nothing is incoming.          
              case None => (List(), List())
            }
          case None =>
            currentStateOption match {
              // current available but no previous: Everything is incoming
              case Some(currentState) =>
                val currentNonEmptyBeds = currentState.beds.filter(bed => !bed.free)
                val bedsWithIncomingPatientTypeSi = currentNonEmptyBeds.filter(isBedPatientTypeSi)
                val bedsWithIncomingPatientTypeSc = currentNonEmptyBeds.filterNot(isBedPatientTypeSi)
                //(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc)
                val bedsWithIncomingPatientTypeSiAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSi, Constants.BED_REASON_FOR_TRANSFER_SI)
                val bedsWithIncomingPatientTypeScAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSc, Constants.BED_REASON_FOR_TRANSFER_SC)
                (bedsWithIncomingPatientTypeSiAndReasonForTransfer, bedsWithIncomingPatientTypeScAndReasonForTransfer)
              // no previous nor current state available
              case None => (List(), List())
            }
        }
    }
  }

  /**
   * Returns a pair of Seq[Bed]. The first one contains incoming SI patients while the second contains incoming SC patients.
   */
  def getBedsWithIncomingPatient(previousStateOption: Option[Hospital], currentStateOption: Option[Hospital]): (List[Bed], List[Bed]) = {

    previousStateOption match {
      case Some(previousState) =>
        currentStateOption match {
          // previous and current available
          case Some(currentState) => {
            // Incoming patients 
            val bedsWithIncomingPatients = currentState.beds.filter { currentStateBed =>
              if (!currentStateBed.free) {
                // Check if the current patient was already there
                val existingBed = previousState.beds.find(previousStateBed => currentStateBed.patientNb == previousStateBed.patientNb)
                // Return true if was not previously there
                (existingBed == None)
              } else {
                // Skip empty bed
                false
              }
            }
            val bedsWithIncomingPatientTypeSi = bedsWithIncomingPatients.filter(isBedPatientTypeSi)
            val bedsWithIncomingPatientTypeSc = bedsWithIncomingPatients.filterNot(isBedPatientTypeSi)
            //(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc)
            val bedsWithIncomingPatientTypeSiAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSi, Constants.BED_REASON_FOR_TRANSFER_SI)
            val bedsWithIncomingPatientTypeScAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSc, Constants.BED_REASON_FOR_TRANSFER_SC)
            (bedsWithIncomingPatientTypeSiAndReasonForTransfer, bedsWithIncomingPatientTypeScAndReasonForTransfer)
          }
          // previous available but no current: Nothing is incoming.          
          case None => (List(), List())
        }
      case None =>
        currentStateOption match {
          // current available but no previous: Everything is incoming
          case Some(currentState) =>
            val currentNonEmptyBeds = currentState.beds.filter(bed => !bed.free)
            val bedsWithIncomingPatientTypeSi = currentNonEmptyBeds.filter(isBedPatientTypeSi)
            val bedsWithIncomingPatientTypeSc = currentNonEmptyBeds.filterNot(isBedPatientTypeSi)
            //(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc)
            val bedsWithIncomingPatientTypeSiAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSi, Constants.BED_REASON_FOR_TRANSFER_SI)
            val bedsWithIncomingPatientTypeScAndReasonForTransfer = setReasonForTransfer(bedsWithIncomingPatientTypeSc, Constants.BED_REASON_FOR_TRANSFER_SC)
            (bedsWithIncomingPatientTypeSiAndReasonForTransfer, bedsWithIncomingPatientTypeScAndReasonForTransfer)
          // no previous nor current state available
          case None => (List(), List())
        }
    }
  }

  /**
   * Return a pair of Seq[Bed]. The first one contains outgoing SI patients while the second contains outgoing SC patients.
   */
  def getBedsWithOutgoingPatient(previousStateOption: Option[Hospital], currentStateOption: Option[Hospital]): (List[Bed], List[Bed]) = {

    previousStateOption match {
      case Some(previousState) =>
        currentStateOption match {
          // previous and current available
          case Some(currentState) => {
            // Outgoing patients
            val bedsWithOutgoingPatient = previousState.beds.filter { previousStateBed =>
              if (!previousStateBed.free) {
                // Check if the previous patient is still there
                val existingBed = currentState.beds.find(currentStateBed => currentStateBed.patientNb == previousStateBed.patientNb)
                // Return true if no more there
                (existingBed == None)
              } else {
                // Skip empty bed
                false
              }
            }
            // Split outgoing in SI, SC.
            val bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatient.filter(isBedPatientTypeSi)
            val bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatient.filterNot(isBedPatientTypeSi)
            (bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc)
          }
          // Previous available but nothing anymore in current: Everything is outgoing
          case None =>
            val previousNonEmptyBeds = previousState.beds.filter(bed => !bed.free)
            val bedsWithOutgoingPatientTypeSi = previousNonEmptyBeds.filter(isBedPatientTypeSi)
            val bedsWithOutgoingPatientTypeSc = previousNonEmptyBeds.filterNot(isBedPatientTypeSi)
            (bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc)
        }
      case None =>
        currentStateOption match {
          // current available but no previous: Nothing outgoing
          case Some(currentState) => (List(), List())
          // no previous nor current state available
          case None => (List(), List())
        }
    }
  }

  /**
   * Return a pair of Seq[Bed]. The first one contains patients patientType SC to SI change while the second contains patients patientType SI to SC change
   */
  def getBedsWithPatientTypeChange(previousStateOption: Option[Hospital], currentStateOption: Option[Hospital]): (List[Bed], List[Bed]) = {

    previousStateOption match {
      case Some(previousState) =>
        currentStateOption match {
          // previous and current available
          case Some(currentState) => {
            // From SC to SI 
            val bedsWithPatientTypeChangeFromScToSi = currentState.beds.filter { currentStateBed =>
              if (!currentStateBed.free) {
                // Check if the current patient was already there
                val bedWithPatientTypeChange = previousState.beds.find(previousStateBed => currentStateBed.patientNb == previousStateBed.patientNb) match {
                  case Some(previousStateBed) =>
                    (currentStateBed.patientType != previousStateBed.patientType) &&
                      (previousStateBed.patientType == PATIENT_TYPE_SC) &&
                      (currentStateBed.patientType == PATIENT_TYPE_SI)
                  case None => false
                }
                bedWithPatientTypeChange
              } else {
                // Skip empty bed
                false
              }
            }

            // From SI to SC 
            val bedsWithPatientTypeChangeFromSiToSc = currentState.beds.filter { currentStateBed =>
              if (!currentStateBed.free) {
                // Check if the current patient was already there
                val bedWithPatientTypeChange = previousState.beds.find(previousStateBed => currentStateBed.patientNb == previousStateBed.patientNb) match {
                  case Some(previousStateBed) =>
                    (currentStateBed.patientType != previousStateBed.patientType) &&
                      (previousStateBed.patientType == PATIENT_TYPE_SI) &&
                      (currentStateBed.patientType == PATIENT_TYPE_SC)
                  case None => false
                }
                bedWithPatientTypeChange
              } else {
                // Skip empty bed
                false
              }
            }
            //(bedsWithPatientTypeChangeFromScToSi, bedsWithPatientTypeChangeFromSiToSc)
            val bedsWithPatientTypeChangeFromScToSiAndReasonForTransfer = setReasonForTransfer(bedsWithPatientTypeChangeFromScToSi, Constants.BED_REASON_FOR_TRANSFER_SC_TO_SI)
            val bedsWithPatientTypeChangeFromSiToScAndReasonForTransfer = setReasonForTransfer(bedsWithPatientTypeChangeFromSiToSc, Constants.BED_REASON_FOR_TRANSFER_SI_TO_SC)
            (bedsWithPatientTypeChangeFromScToSiAndReasonForTransfer, bedsWithPatientTypeChangeFromSiToScAndReasonForTransfer)
          }
          // previous available but no current: No existing bed change tracking.          
          case None => (List(), List())
        }
      // No previous available: No existing bed change tracking.
      case None => (List(), List())
    }
  }

  /**
   * Return (bedsWithTransferTypeOnlyChangePatientTypeSi:List[Bed],bedsWithTransferTypeOnlyChangePatientTypeSc:List[Bed]) for which only TransferType changed. It excludes those already included in patientType change.
   */
  def getBedsWithTransfertTypeChangeOnly(previousStateOption: Option[Hospital], currentStateOption: Option[Hospital]): (List[Bed], List[Bed]) = {

    previousStateOption match {
      case Some(previousState) =>
        currentStateOption match {
          // previous and current available
          case Some(currentState) => {
            // Beds with transfer type only change 
            val bedsWithTransferTypeOnlyChange = currentState.beds.filter { currentStateBed =>
              if (!currentStateBed.free) {
                // Check if the current patient was already there
                val isBedWithTransferTypeOnlyChange = previousState.beds.find(previousStateBed => currentStateBed.patientNb == previousStateBed.patientNb) match {
                  case Some(previousStateBed) =>
                    // We exclude patient type change beds already included in getBedsWithPatientTypeChange check
                    (currentStateBed.patientType == previousStateBed.patientType) &&
                      // We check transfer type changed (any change)
                      (previousStateBed.transferType != currentStateBed.transferType)
                  case None => false
                }
                isBedWithTransferTypeOnlyChange
              } else {
                // Skip empty bed
                false
              }
            }
            val bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChange.filter(isBedPatientTypeSi)
            val bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChange.filterNot(isBedPatientTypeSi)
            //(bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc)
            val bedsWithTransferTypeOnlyChangePatientTypeSiAndReasonForTransfer = setReasonForTransfer(bedsWithTransferTypeOnlyChangePatientTypeSi, Constants.BED_REASON_FOR_TRANSFER_TRANSFER_TYPE_CHANGE_FOR_SI)
            val bedsWithTransferTypeOnlyChangePatientTypeScAndReasonForTransfer = setReasonForTransfer(bedsWithTransferTypeOnlyChangePatientTypeSc, Constants.BED_REASON_FOR_TRANSFER_TRANSFER_TYPE_CHANGE_FOR_SC)
            (bedsWithTransferTypeOnlyChangePatientTypeSiAndReasonForTransfer, bedsWithTransferTypeOnlyChangePatientTypeScAndReasonForTransfer)
          }
          // previous available but no current: No existing bed change tracking.          
          case None => (List(), List())
        }
      // No previous available: No existing bed change tracking.
      case None => (List(), List())
    }
  }

  /**
   * Encloses several beds updates function calls to provide a single tuple of 7th results as a time.
   *
   * List[Bed] results are:
   *
   * `(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
   *   bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
   *   patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
   *   tranferTypeOnlyChange)`
   *
   */
  def getBedsUpdates(previousHospitalState: Option[Hospital], currentHospitalState: Option[Hospital]): (List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed]) = {
    val incoming = HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState)
    val outgoing = HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState)
    val patientTypeChange = HospitalHelper.getBedsWithPatientTypeChange(previousHospitalState, currentHospitalState)
    val tranferTypeOnlyChange = HospitalHelper.getBedsWithTransfertTypeChangeOnly(previousHospitalState, currentHospitalState)
    (incoming._1, incoming._2, outgoing._1, outgoing._2, patientTypeChange._1, patientTypeChange._2, tranferTypeOnlyChange._1, tranferTypeOnlyChange._2)
  }

  /**
   * TODO: CURRENT DEV...
   */
  def getCdfBedsUpdates(previousCdfHospitalState: Option[Hospital], currentCdfHospitalState: Option[Hospital], previousSimulatedPrtHospitalState: Option[Hospital]): (List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed]) = {
    // DONE
    val incoming = HospitalHelper.getBedsWithIncomingPatient(previousCdfHospitalState, previousSimulatedPrtHospitalState, currentCdfHospitalState)
    // TODO
    val outgoing = HospitalHelper.getBedsWithOutgoingPatient(previousCdfHospitalState, currentCdfHospitalState)
    // TODO
    val patientTypeChange = HospitalHelper.getBedsWithPatientTypeChange(previousCdfHospitalState, currentCdfHospitalState)
    // TODO
    val tranferTypeOnlyChange = HospitalHelper.getBedsWithTransfertTypeChangeOnly(previousCdfHospitalState, currentCdfHospitalState)
    (incoming._1, incoming._2, outgoing._1, outgoing._2, patientTypeChange._1, patientTypeChange._2, tranferTypeOnlyChange._1, tranferTypeOnlyChange._2)
  }

  /**
   * CURRENT: Full implementation review needed
   * =====================================================================================================================================
   * ====                                      UPDATE ALGO CDF                                                                        ====
   * =====================================================================================================================================
   * These events are determined on static HOSPITAL_STATEs NOT on simulated hospital state.
   *
   *  - bedsWithIncomingPatientTypeSi must trigger TRANSFER NATURE="add"       => be transferred to PRT
   *  - patientTypeChangeFromScToSi   must trigger TRANSFER NATURE="add"       => be transferred to PRT
   *
   *  - bedsWithOutgoingPatientTypeSi must trigger TRANSFER NATURE="remove"    => notify PRT these transferred SI patients are going out
   *
   *  - patientTypeChangeFromSiToSc   must trigger TRANSFER NATURE="update"    => notify PRT patients have had their patient type changed
   *
   *  - tranferTypeOnlyChange:
   *       if SI patient type         must trigger TRANSFER NATURE="update"    => notify PRT patients have had transfer type changed
   *                                                                              (replace their previous bed values with new updated ones)
   *       if SC patient type         must update CDF `simulatedHospitalState` => replace their previous bed values with new updated ones
   *
   *  - bedsWithIncomingPatientTypeSc must update CDF `simulatedHospitalState` => stay at CDF
   *  - bedsWithOutgoingPatientTypeSc must update CDF `simulatedHospitalState` => out of CDF
   *
   * Returns a new copy of `currentSimulatedHospitalStateOption` updated with all provided information.
   */
  def updateSimulatedHospitalStateForCdf(
    currentSimulatedHospitalStateOption: Option[Hospital], newStaticHospitalStateOption: Option[Hospital], bedsWithIncomingPatientTypeSi: List[Bed], bedsWithIncomingPatientTypeSc: List[Bed],
    bedsWithOutgoingPatientTypeSi: List[Bed], bedsWithOutgoingPatientTypeSc: List[Bed], patientTypeChangeFromScToSi: List[Bed],
    patientTypeChangeFromSiToSc: List[Bed], transferTypeOnlyChangePatientTypeSi: List[Bed], transferTypeOnlyChangePatientTypeSc: List[Bed]): Option[Hospital] = {

    currentSimulatedHospitalStateOption match {
      case Some(currentSimulatedHospitalState) =>
        newStaticHospitalStateOption match {
          case Some(newStaticHospitalState) =>
            // bedsWithIncomingPatientTypeSi - DO NOTHING - These are transferred to PRT
            // bedsWithIncomingPatientTypeSc - TO ADD - new CDF incoming beds
            val currentWithIncomingSc = currentSimulatedHospitalState.beds ++ bedsWithIncomingPatientTypeSc
            // bedsWithOutgoingPatientTypeSi - DO NOTHING - These changes are forwarded to PRT where the SI beds have been transferred
            // bedsWithOutgoingPatientTypeSc - TO REMOVE - outgoing SC beds at CDF
            val currentWithIncomingScMinusOutgoingSc = currentWithIncomingSc diff bedsWithOutgoingPatientTypeSc
            // patientTypeChangeFromScToSi - TO REMOVE - beds we transfer to PRT following patient type change 
            val currentWithIncomingScMinusOutgoingScMinusScToSi = currentWithIncomingScMinusOutgoingSc diff patientTypeChangeFromScToSi
            // patientTypeChangeFromSiToSc - DO NOTHING - These changes are forwarded to PRT where the SI beds have been transferred. (They could be transferred back from PRT but this would not be dealt with here anyway.)
            // transferTypeOnlyChangePatientTypeSi - DO NOTHING - SI beds with new updated transfer type information are managed at PRT side
            // transferTypeOnlyChangePatientTypeSc - TO REPLACE - SC beds with new updated transfer type information 
            val currentWithIncomingScMinusOutgoingScMinusScToSiWithUpdatedTransferType =
              (currentWithIncomingScMinusOutgoingScMinusScToSi diff transferTypeOnlyChangePatientTypeSc) ++ transferTypeOnlyChangePatientTypeSc
            Some(Hospital(newStaticHospitalState.code, newStaticHospitalState.schedule, currentWithIncomingScMinusOutgoingScMinusScToSiWithUpdatedTransferType))
          case None => 
            logger.error("updateSimulatedHospitalStateForCdf received None for newStaticHospitalStateOption !?")
            // Nothing new provided keep the current simulated state unchanged. We do not expect such call.
            Some(currentSimulatedHospitalState)
        }

      case None =>
        newStaticHospitalStateOption match {
          case Some(newStaticHospitalState) =>
            // bedsWithIncomingPatientTypeSi - DO NOTHING - These are transferred to PRT
            // bedsWithIncomingPatientTypeSc - TO ADD - new CDF incoming beds
            // bedsWithOutgoingPatientTypeSi - DO NOTHING - No current state: must be empty
            // bedsWithOutgoingPatientTypeSc - DO NOTHING - No current state: must be empty
            // patientTypeChangeFromScToSi - DO NOTHING - No current state: must be empty
            // patientTypeChangeFromSiToSc - DO NOTHING - No current state: must be empty
            // transferTypeOnlyChangePatientTypeSi - DO NOTHING - No current state: must be empty
            // transferTypeOnlyChangePatientTypeSc - DO NOTHING - No current state: must be empty
            Some(Hospital(newStaticHospitalState.code, newStaticHospitalState.schedule, bedsWithIncomingPatientTypeSc))
          case None => None
        }
    }

  }

  /**
   * Returns a new copy of `currentSimulatedHospitalStateOption` updated with all provided information.
   */
  def updateSimulatedHospitalStateForPrt(
    currentSimulatedHospitalStateOption: Option[Hospital], newStaticHospitalStateOption: Option[Hospital], bedsWithIncomingPatientTypeSi: List[Bed], bedsWithIncomingPatientTypeSc: List[Bed],
    bedsWithOutgoingPatientTypeSi: List[Bed], bedsWithOutgoingPatientTypeSc: List[Bed], patientTypeChangeFromScToSi: List[Bed],
    patientTypeChangeFromSiToSc: List[Bed], bedsWithTransferTypeOnlyChangePatientTypeSi: List[Bed], bedsWithTransferTypeOnlyChangePatientTypeSc: List[Bed]): Option[Hospital] = {

    currentSimulatedHospitalStateOption match {

      case Some(currentSimulatedHospitalState) =>

        newStaticHospitalStateOption match {

          // Compute delta from current to new hospital state 
          case Some(newStaticHospitalState) => applyDeltas(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc, Some(currentSimulatedHospitalState), newStaticHospitalState)
          // No new hospital state return current state unchanged
          case None => Some(currentSimulatedHospitalState)
        }

      case None =>

        newStaticHospitalStateOption match {
          // No current state only newStatic, same logic 
          case Some(newStaticHospitalState) => applyDeltas(bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc, None, newStaticHospitalState)

          case None => None
        }
    }

  }

  /**
   * TODO: review
   */
  private def applyDeltas(
    bedsWithIncomingPatientTypeSi: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    bedsWithIncomingPatientTypeSc: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    bedsWithOutgoingPatientTypeSi: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    bedsWithOutgoingPatientTypeSc: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    patientTypeChangeFromScToSi: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    patientTypeChangeFromSiToSc: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    bedsWithTransferTypeOnlyChangePatientTypeSi: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    bedsWithTransferTypeOnlyChangePatientTypeSc: List[ch.bsisa.hyperbird.patman.simulations.model.Bed],
    currentSimulatedHospitalStateOption: Option[ch.bsisa.hyperbird.patman.simulations.model.Hospital],
    newStaticHospitalState: ch.bsisa.hyperbird.patman.simulations.model.Hospital): Some[ch.bsisa.hyperbird.patman.simulations.model.Hospital] = {
    // bedsWithOutgoingPatientTypeSi - TO REMOVE: they may either directly go out from PRT or are simulated in PRT and signaled out from CDF
    // bedsWithOutgoingPatientTypeSc - TO REMOVE: should only be from PRT
    //val currentMinusOutgoingBeds = (currentSimulatedHospitalState.beds diff bedsWithOutgoingPatientTypeSi) diff bedsWithOutgoingPatientTypeSc
    val currentMinusOutgoingBeds = currentSimulatedHospitalStateOption match {
      case Some(currentSimulatedHospitalState) => (currentSimulatedHospitalState.beds diff bedsWithOutgoingPatientTypeSi) diff bedsWithOutgoingPatientTypeSc
      case None => (newStaticHospitalState.beds diff bedsWithOutgoingPatientTypeSi) diff bedsWithOutgoingPatientTypeSc
    }

    // bedsWithIncomingPatientTypeSi - TO ADD: they may either directly come from PRT or be transferred from CDF
    // bedsWithIncomingPatientTypeSc - TO ADD: they come from PRT only
    val currentLeftPlusIncomingBeds = (currentMinusOutgoingBeds ++ bedsWithIncomingPatientTypeSc) ++ bedsWithIncomingPatientTypeSi
    // patientTypeChangeFromScToSi -   TO REPLACE: in order to have latest patient type up to date
    // patientTypeChangeFromSiToSc -   TO REPLACE: in order to have latest patient type up to date
    // transfertTypeChangeOnly -       TO REPLACE: in order to have latest transfer type up to date
    // TODO: updated to compile only. Logic must be reviewed
    //val currentDeltaMinusUpdated = ((currentLeftPlusIncomingBeds diff patientTypeChangeFromScToSi) diff patientTypeChangeFromSiToSc) diff transfertTypeChangeOnly
    val currentDeltaMinusUpdated = ((currentLeftPlusIncomingBeds diff patientTypeChangeFromScToSi) diff patientTypeChangeFromSiToSc) diff bedsWithTransferTypeOnlyChangePatientTypeSi
    //val currentDeltaWithUpdated = currentDeltaMinusUpdated ++ patientTypeChangeFromScToSi ++ patientTypeChangeFromSiToSc ++ transfertTypeChangeOnly
    val currentDeltaWithUpdated = currentDeltaMinusUpdated ++ patientTypeChangeFromScToSi ++ patientTypeChangeFromSiToSc ++ bedsWithTransferTypeOnlyChangePatientTypeSi

    Some(Hospital(newStaticHospitalState.code, newStaticHospitalState.schedule, currentDeltaWithUpdated))
  }

  /**
   * Builds `ELFIN` of CLASSE='HOSPITAL_STATE' for `elfinHospitalStateTemplate: ELFIN`, `simulationId: String`, ``, `hospitalState: Hospital`
   */
  def buildHospitalStateElfin(elfinHospitalStateTemplate: ELFIN, simulationId: String, hospitalState: Hospital): Future[ELFIN] = {

    val elfinHospitalStateWithIdFuture: Future[ELFIN] = ElfinUtil.assignElfinId(elfinHospitalStateTemplate)

    val elfinHospitalStateWithBedsFuture = elfinHospitalStateWithIdFuture.map { elfinHospitalStateWithId =>
      // Assign ID_G: G20150114160000006 to have ELFIN_SIMULATION_NATURE store in a collection distinct 
      // from end users recorded HOSPITAL_STATE ELFINs.
      val elfinHospitalStateWithUpdatedID_G = ElfinUtil.replaceElfinID_G(elfinHospitalStateWithId, Constants.ELFIN_HOSPITAL_STATE_SIMULATION_COLLECTION_ID)

      val elfinHospitalStateWithNewNatureGroupeSource = ElfinUtil.replaceElfinNatureGroupeSource(elfin = elfinHospitalStateWithUpdatedID_G, newNature = Constants.ELFIN_HOSPITAL_STATE_SIMULATION_NATURE, newGroupe = elfinHospitalStateWithUpdatedID_G.GROUPE, newSource = Some(simulationId))
      // bedsPosStartIndex starts at 2 as we manually add lMeta with POS=1 at the head of lSeq
      val bedsHospitalWrapperElfin = HospitalHelper.toElfin(hospital = hospitalState, bedsPosStartIndex = 2)
      val identifiantHospitalState = IDENTIFIANT(AUT = Some("FluxPatients - Simulator"), NOM = None, ORIGINE = None, OBJECTIF = None, DE = Option(DateUtil.getIsoDateFormatterWithoutTz.format(hospitalState.schedule)))
      val elfinHospitalStateWithIdentifiant = ElfinUtil.replaceElfinIdentifiant(elfinHospitalStateWithNewNatureGroupeSource, identifiantHospitalState)

      val lSeq: Seq[ch.bsisa.hyperbird.model.L] = bedsHospitalWrapperElfin.CARACTERISTIQUE.get.FRACTION.get.L

      // Meta-data identifying hospital
      val lMetaXmlElem = <L POS="1">
                           <!-- Code ALIAS (Stable) -->
                           <C POS="1">{ hospitalState.code }</C>
                           <!-- Nom NOM (full name) -->
                           <C POS="2">N/A</C>
                           <!-- Id -->
                           <C POS="3">N/A</C>
                           <!-- ID_G -->
                           <C POS="4">N/A</C>
                         </L>
      val lMeta = ElfinFormat.lFromXml(lMetaXmlElem)
      val lSeqWithMeta: Seq[ch.bsisa.hyperbird.model.L] = lMeta +: lSeq

      // TODO: we need L[0] to match Hospital meta-data unlike TRANSFER 
      //val elfinHospitalStateWithBeds = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfinHospitalStateWithIdentifiant, bedsHospitalWrapperElfin.CARACTERISTIQUE.get.FRACTION.get.L)
      val elfinHospitalStateWithBeds = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfinHospitalStateWithIdentifiant, lSeqWithMeta)

      elfinHospitalStateWithBeds
    }

    elfinHospitalStateWithBedsFuture
  }

  /**
   * Builds `ELFIN` of CLASSE='TRANSFER' given provided parameters.
   */
  def buildTransferElfin(elfinTransferTemplate: ELFIN, simulationId: String, nature: String, fromHospitalCode: String, toHospitalCode: String, schedule: Date, beds: List[Bed]): Future[ELFIN] = {

    val elfinTransferWithIdFuture: Future[ELFIN] = ElfinUtil.assignElfinId(elfinTransferTemplate)

    val elfinTransferWithBedsFuture = elfinTransferWithIdFuture.map { elfinTransferWithId =>
      val elfinTransferWithNewNatureGroupeSource = ElfinUtil.replaceElfinNatureGroupeSource(elfin = elfinTransferWithId, newNature = nature, newGroupe = elfinTransferWithId.GROUPE, newSource = Some(simulationId))
      val bedsHospitalWrapper = Hospital(code = fromHospitalCode, schedule = schedule, beds = beds)
      val bedsHospitalWrapperElfin = HospitalHelper.toElfin(bedsHospitalWrapper)

      val identifiantTransfer = IDENTIFIANT(AUT = Some("FluxPatients - Simulator"), NOM = None, ORIGINE = Option(fromHospitalCode), OBJECTIF = Option(toHospitalCode), DE = Option(DateUtil.getIsoDateFormatterWithoutTz.format(schedule)))
      val elfinTransferWithIdentifiant = ElfinUtil.replaceElfinIdentifiant(elfinTransferWithNewNatureGroupeSource, identifiantTransfer)
      val elfinTransferWithBeds = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfinTransferWithIdentifiant, bedsHospitalWrapperElfin.CARACTERISTIQUE.get.FRACTION.get.L)
      elfinTransferWithBeds
    }

    elfinTransferWithBedsFuture
  }

  /**
   * Create SIMULATION database entry and return a the corresponding ELFIN.Id
   */
  def createSimulationDatabaseEntry(author: Option[String] = None, dateFrom: String, dateTo: String): Future[String] = {

    val simulationIdFutureFuture = ElfinDAO.getNewFromCatalogue("SIMULATION").flatMap { simulationElfinTemplate =>
      val simulationIdFuture = buildSimulationElfin(simulationElfinTemplate, author, dateFrom, dateTo).map { simulationElfin =>
        ElfinDAO.create(simulationElfin)
        simulationElfin.Id
      }
      simulationIdFuture
    }
    simulationIdFutureFuture
  }

  /**
   * Update SIMULATION database entry with HospitalSimulationSummary
   */
  def updateSimulationDatabaseEntry(simulationId: String, hssList: List[HospitalSimulationSummary]): Unit = {

    logger.info(s">>>> updateSimulationDatabaseEntry(simulationId = ${simulationId})")

    val simulationElfinFuture = XQueryWSHelper.find(WSQueries.elfinQuery(Constants.ELFIN_SIMULATION_COLLECTION_ID, elfinId = simulationId))

    simulationElfinFuture.map { simulationElfin =>

      val currLSeq = simulationElfin.CARACTERISTIQUE.get.FRACTION.get.L

      val newLXmlElements = for ((hss, i) <- hssList zipWithIndex) yield {
        val newLXmlElement = <L POS={ (i + currLSeq.size + 1).toString }>
                               <!-- Code ALIAS (Stable) -->
                               <C POS="1">{ hss.hospitalCode }</C>
                               <!-- Total incoming SI -->
                               <C POS="2">{ hss.totalIncomingSiPatient }</C>
                               <!-- Total incoming SC -->
                               <C POS="3">{ hss.totalIncomingScPatient }</C>
                               <!-- Total outgoing SI -->
                               <C POS="4">{ hss.totalOutgoingSiPatient }</C>
                               <!-- Total outgoing SC -->
                               <C POS="5">{ hss.totalOutgoingScPatient }</C>
                             </L>
        newLXmlElement
      }

      val newL = for (newLXmlElement <- newLXmlElements) yield { ElfinFormat.lFromXml(newLXmlElement) }
      val newLSeq = currLSeq ++ newL
      val updatedSimulationElfin = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfin = simulationElfin, newLSeq = newLSeq)
      ElfinDAO.update(updatedSimulationElfin)
    }

  }

  /**
   * Builds `ELFIN` of CLASSE='SIMULATION' given provided parameters.
   */
  def buildSimulationElfin(elfinSimulationTemplate: ELFIN, author: Option[String] = None, dateFrom: String, dateTo: String): Future[ELFIN] = {

    val simulationElfinWithIdFuture: Future[ELFIN] = ElfinUtil.assignElfinId(elfinSimulationTemplate)

    val simulationElfinWithCaracteristiqueFuture: Future[ELFIN] = simulationElfinWithIdFuture.map { simulationElfinWithId =>

      val identifiantSimulation = IDENTIFIANT(AUT = author, DE = Option(DateUtil.getIsoDateFormatterWithoutTz.format(new Date())))
      val simulationElfinWithIdentifiant = ElfinUtil.replaceElfinIdentifiant(simulationElfinWithId, identifiantSimulation)

      val characteristicsXmlElem =
        <CARACTERISTIQUE>
          <FRACTION>
            <!-- Simulation parameters -->
            <L POS="1">
              <!-- Simulation parameter date from -->
              <C POS="1">{ DateUtil.getIsoDateFormatterWithoutTz.format(DateUtil.hbDateFormat.parse(dateFrom)) }</C>
              <!-- Simulation parameter date to -->
              <C POS="2">{ DateUtil.getIsoDateFormatterWithoutTz.format(DateUtil.hbDateFormat.parse(dateTo)) }</C>
            </L>
          </FRACTION>
        </CARACTERISTIQUE>

      val caracteristique = ElfinFormat.caracteristiqueFromXml(characteristicsXmlElem)
      val simulationElfinWithCaracteristique = ElfinUtil.replaceElfinCaracteristique(simulationElfinWithIdentifiant, caracteristique)
      simulationElfinWithCaracteristique
    }

    simulationElfinWithCaracteristiqueFuture
  }

  /**
   * HospitalSimulationSummary update helper
   */
  def updateHospitalSimulationSummary(hospitalCode: String, currentHss: Option[HospitalSimulationSummary], bedsWithIncomingPatientTypeSi: List[Bed], bedsWithIncomingPatientTypeSc: List[Bed], bedsWithOutgoingPatientTypeSi: List[Bed], bedsWithOutgoingPatientTypeSc: List[Bed]): HospitalSimulationSummary = {

    val newHss = currentHss match {

      case Some(hss) =>
        HospitalSimulationSummary(hospitalCode = hss.hospitalCode,
          totalIncomingSiPatient = hss.totalIncomingSiPatient + bedsWithIncomingPatientTypeSi.size,
          totalIncomingScPatient = hss.totalIncomingScPatient + bedsWithIncomingPatientTypeSc.size,
          totalOutgoingSiPatient = hss.totalOutgoingSiPatient + bedsWithOutgoingPatientTypeSi.size,
          totalOutgoingScPatient = hss.totalOutgoingScPatient + bedsWithOutgoingPatientTypeSc.size)

      case None =>
        HospitalSimulationSummary(hospitalCode = hospitalCode,
          totalIncomingSiPatient = bedsWithIncomingPatientTypeSi.size,
          totalIncomingScPatient = bedsWithIncomingPatientTypeSc.size,
          totalOutgoingSiPatient = bedsWithOutgoingPatientTypeSi.size,
          totalOutgoingScPatient = bedsWithOutgoingPatientTypeSc.size)
    }
    newHss
  }

}
