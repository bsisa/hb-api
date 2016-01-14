package ch.bsisa.hyperbird.orders

import play.api.Logger

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.model.format.Implicits._

/**
 * Object grouping simple functions dealing with Orders.
 *
 * @author Patrick Refondini
 */
object OrderUtil {

  val MANUAL_AMOUNT = "MANUAL_AMOUNT"
  val REDUCTION_RATE = "REDUCTION_RATE"
  val DISCOUNT_RATE = "DISCOUNT_RATE"
  val ROUNDING_AMOUNT = "ROUNDING_AMOUNT"
  val VAT_RATE = "VAT_RATE"
  val NET_AMOUNT_TOTAL = "TOTAL_NET"
  val GROSS_AMOUNT_TOTAL = "TOTAL_GROSS"

  def getTotalGrossLine(total: Double): L = {

    val totalGrossXml = <L POS="2">
                          <C POS="1">TOTAL_GROSS</C>
                          <C POS="2">Total brut</C>
                          <C POS="3"/>
                          <C POS="4"/>
                          <C POS="5">{ f"$total%2.2f" }</C>
                          <C POS="6">false</C>
                        </L>

    ElfinFormat.lFromXml(totalGrossXml)

  }

  /**
   * Compute the total gross amount from entered MANUAL_AMOUNT lines
   * if any. Returns None otherwise. 
   */
  def getTotalGrossAmount(fraction: MATRICETypable): Option[Double] = {

    println(s"fraction => \n${fraction}")

    // Keep only user entries
    val userEnteredOrderLines = fraction.L.filter { l =>
      l.C.filter { c =>
        println(s"c.mixed.mkString = " + getMixedContent(c.mixed))
        (c.POS == 1 && getMixedContent(c.mixed) == MANUAL_AMOUNT)
      }.seq.size == 1
    }
    Logger.warn(s"userEnteredOrderLines => \n${userEnteredOrderLines}")
    println(s"userEnteredOrderLines => \n${userEnteredOrderLines}")
    // Compute total for user entries
    val grossTotal = userEnteredOrderLines.map { l =>
      // Get cell containing amount (Position 5)
      val amountCellSeq = l.C.filter { c => c.POS == 5 }
      // Make sure we have a single match
      if (amountCellSeq.size == 1) {
        val amountCellString = getMixedContent(amountCellSeq(0).mixed)
        val amountCellDoubleValue = amountCellString.toDouble
        Option(amountCellDoubleValue)
        //amountCellDoubleValue
      } else {
        None
      }
    }
    //}.sum

    if (grossTotal.exists { x => x match { case Some(y) => true } }) {
      val grossTotalOpt = grossTotal.foldLeft(0d) { (acc, el) =>
        el match {
          case Some(nb) => acc + nb
          case None     => acc
        }
      }
      Some(grossTotalOpt)
    } else {
      None
    }
  }

  /**
   * Computes percentages, gross and net amounts given provided order details.
   */
  def computeOrderFigures(carP: CARACTERISTIQUE): CARACTERISTIQUE = {

    
    // 1. Compute gross amount 
    // 2. If gross amount obtained replace gross amount line 
    // 3. Find and update REDUCTION_RATE
    // 4. Find and update DISCOUNT_RATE 
    // 5. Find and do nothing ROUNDING_AMOUNT
    // 6. Find and update VAT_RATE
    // 7. Compute and replace NET_AMOUNT_TOTAL

    
    // Extract FRACTION if available create a new FRACTION creating, updating, copying original FRACTION data as necessary.
    val updatedFractionOpt = carP.FRACTION.map { fract =>

      val grossTotalAmntOpt = getTotalGrossAmount(fract)
      val newComputedGrossTotalLine = grossTotalAmntOpt match {
        case Some(grossTotalAmnt) =>
          // Create gross total entry using computed total
          val newGrossTotalComputedLine = getTotalGrossLine(grossTotalAmnt)
          Option(newGrossTotalComputedLine)
        case None =>
          // Make sure there was a total gross line with user entered amount and keept it
          val grossAmtLines = fract.L.filter { l => l.C.exists { c => c.POS == 1 && getMixedContent(c.mixed) == GROSS_AMOUNT_TOTAL } }
          if (grossAmtLines.size == 1) {
            // Ok - do nothing
          } else if (grossAmtLines.size > 1) {
            // Only one GROSS_AMOUNT_TOTAL line allowed: Data integrity problem - TODO: provide some end-user feedback            
          } else {
            // No gross amount line is unexpected: Data integrity problem - TODO: provide some end-user feedback  
          }
          //fract.L.exists { l => l.C.exists { c => c.POS == 1 && getMixedContent(c.mixed) == GROSS_AMOUNT_TOTAL } }
          None
      }

      newComputedGrossTotalLine match {
        case Some(l) =>
          // Append newGrossTotalComputedLine to original FRACTION lines
          val updatedSeqL = fract.L :+ l
          // Create a new FRACTION 
          val updatedFraction = MATRICEType(updatedSeqL: _*)
          updatedFraction
        case None =>
          // Preserve fraction
          fract
      }

    }

    // Create new CARACTERISTIQUE 
    val carUpdated = CARACTERISTIQUE(carP.CAR1, carP.CAR2, carP.CAR3, carP.CAR4, carP.CAR5, carP.CAR6, carP.CARSET, carP.ETAT, carP.CALCUL, updatedFractionOpt)

    carUpdated

  }

}