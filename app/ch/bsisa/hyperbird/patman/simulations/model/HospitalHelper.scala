package ch.bsisa.hyperbird.patman.simulations.model

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.util.DateUtil
import java.util.Date

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
   */
  def toElfin(hospital: Hospital): ELFIN = {

    val bedsXmlElem = for ((bed, i) <- hospital.beds zipWithIndex) yield {

      <!-- Identification lit -->
      val bedXmlElem = <L POS={ (i + 1).toString }>
                         <!-- Numéro lit -->
                         <C POS="1">{ bed.id }</C>
                         <C POS="2">{ bed.patientNb }</C>
                         <C POS="3">{ bed.patientType }</C>
                         <C POS="4">{ bed.transferType }</C>
                         <C POS="5">n/a</C>
                         <C POS="6">{ if (bed.free) "libre" else "occupé" }</C>
                         <C POS="7"/>
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
            (bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc)
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
            (bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc)
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
            (bedsWithPatientTypeChangeFromScToSi, bedsWithPatientTypeChangeFromSiToSc)
          }
          // previous available but no current: No existing bed change tracking.          
          case None => (List(), List())
        }
      // No previous available: No existing bed change tracking.
      case None => (List(), List())
    }
  }

  /**
   * Return List[Bed] for which only TransferType changed. It excludes those already included in patientType change.
   */
  def getBedsWithTransfertTypeChangeOnly(previousStateOption: Option[Hospital], currentStateOption: Option[Hospital]): List[Bed] = {

    previousStateOption match {
      case Some(previousState) =>
        currentStateOption match {
          // previous and current available
          case Some(currentState) => {
            // Beds with transfer type only change 
            val bedsWithTransferTypeOnlyChange = currentState.beds.filter { currentStateBed =>
              if (!currentStateBed.free) {
                // Check if the current patient was already there
                val bedWithTransferTypeOnlyChange = previousState.beds.find(previousStateBed => currentStateBed.patientNb == previousStateBed.patientNb) match {
                  case Some(previousStateBed) =>
                    // We exclude patient type change beds already included in getBedsWithPatientTypeChange check
                    (currentStateBed.patientType == previousStateBed.patientType) &&
                      // We check transfer type changed (any change)
                      (previousStateBed.transferType != currentStateBed.transferType)
                  case None => false
                }
                bedWithTransferTypeOnlyChange
              } else {
                // Skip empty bed
                false
              }
            }
            bedsWithTransferTypeOnlyChange
          }
          // previous available but no current: No existing bed change tracking.          
          case None => List()
        }
      // No previous available: No existing bed change tracking.
      case None => List()
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
  def getBedsUpdates(previousHospitalState: Option[Hospital], currentHospitalState: Option[Hospital]): (List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed], List[Bed]) = {
    val incoming = HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState)
    val outgoing = HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState)
    val patientTypeChange = HospitalHelper.getBedsWithPatientTypeChange(previousHospitalState, currentHospitalState)
    val tranferTypeOnlyChange = HospitalHelper.getBedsWithTransfertTypeChangeOnly(previousHospitalState, currentHospitalState)
    (incoming._1, incoming._2, outgoing._1, outgoing._2, patientTypeChange._1, patientTypeChange._2, tranferTypeOnlyChange)
  }

  /**
   * Returns a new copy of `currentSimulatedHospitalStateOption` updated with all provided information.
   */
  def updateSimulatedHospitalStateForCdf(
    currentSimulatedHospitalStateOption: Option[Hospital], newStaticHospitalStateOption: Option[Hospital], bedsWithIncomingPatientTypeSi: List[Bed], bedsWithIncomingPatientTypeSc: List[Bed],
    bedsWithOutgoingPatientTypeSi: List[Bed], bedsWithOutgoingPatientTypeSc: List[Bed], patientTypeChangeFromScToSi: List[Bed],
    patientTypeChangeFromSiToSc: List[Bed], transfertTypeChangeOnly: List[Bed]): Option[Hospital] = {

    currentSimulatedHospitalStateOption match {
      case Some(currentSimulatedHospitalState) =>
        newStaticHospitalStateOption match {
          case Some(newStaticHospitalState) =>
            // bedsWithIncomingPatientTypeSi - we do not care about : they did not exist before and are directly transferred.
            // bedsWithIncomingPatientTypeSc - new incoming beds:  TO ADD
            // bedsWithOutgoingPatientTypeSi - must be empty as we do not keep any SI bed
            // bedsWithOutgoingPatientTypeSc - outgoing beds: TO REMOVE
            // patientTypeChangeFromScToSi - beds we transfer: TO REMOVE
            // patientTypeChangeFromSiToSc - must be empty as we do not keep any SI bed
            // transfertTypeChangeOnly - beds with new updated information: TO REPLACE
            val currentWithIncomingSc = currentSimulatedHospitalState.beds ++ bedsWithIncomingPatientTypeSc
            val currentWithIncomingScMinusOutgoingSc = currentWithIncomingSc diff bedsWithOutgoingPatientTypeSc
            val currentWithIncomingScMinusOutgoingScMinusScToSi = currentWithIncomingScMinusOutgoingSc diff patientTypeChangeFromScToSi
            val currentWithIncomingScMinusOutgoingScMinusScToSiWithUpdatedTransferType =
              (currentWithIncomingScMinusOutgoingScMinusScToSi diff transfertTypeChangeOnly) ++ transfertTypeChangeOnly
            Some(Hospital(newStaticHospitalState.code, newStaticHospitalState.schedule, currentWithIncomingScMinusOutgoingScMinusScToSiWithUpdatedTransferType))
          case None => Some(currentSimulatedHospitalState)
        }

      case None =>
        newStaticHospitalStateOption match {
          case Some(newStaticHospitalState) =>
            // bedsWithIncomingPatientTypeSi - we do not care about : they are transferred.
            // bedsWithIncomingPatientTypeSc - these are new beds we keep.
            // bedsWithOutgoingPatientTypeSi - must be empty
            // bedsWithOutgoingPatientTypeSc - must be empty
            // patientTypeChangeFromScToSi - must be empty
            // patientTypeChangeFromSiToSc - must be empty
            // transfertTypeChangeOnly - must be empty
            Some(Hospital(newStaticHospitalState.code, newStaticHospitalState.schedule, bedsWithIncomingPatientTypeSc))
          case None => None
        }
    }

  }

}
