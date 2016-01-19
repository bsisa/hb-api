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

  def getTotalNetLine(total: Double): L = {

    val totalNetXml = <L POS="2">
                        <C POS="1">TOTAL_NET</C>
                        <C POS="2">Total net</C>
                        <C POS="3"/>
                        <C POS="4"/>
                        <C POS="5">{ f"$total%2.2f" }</C>
                        <C POS="6">false</C>
                      </L>

    ElfinFormat.lFromXml(totalNetXml)
  }

  /**
   * Return the amount found at cell C position 5 for a given line L
   * if available.
   * Remark: An empty string is considered as value 0.0
   */
  def getLineAmount(l: L): Option[Double] = {

    // Get cell containing amount (Position 5)
    val amountCellSeq = l.C.filter { c => c.POS == 5 }
    // Make sure we have a single match
    if (amountCellSeq.size == 1) {
      val amountCellString = getMixedContent(amountCellSeq(0).mixed)
      val amountCellDoubleValue = amountCellString match {
        case "" => 0d
        case s  => s.toDouble
      }
      Option(amountCellDoubleValue)
    } else {
      None
    }
  }

  /**
   * Compute the total gross amount from entered MANUAL_AMOUNT lines
   * if any. Returns None otherwise.
   */
  def getTotalGrossAmount(fraction: MATRICETypable): Option[Double] = {

    //println(s"fraction => \n${fraction}")

    // Keep only user entries
    val userEnteredOrderLines = fraction.L.filter { l =>
      l.C.filter { c =>
        //println(s"c.mixed.mkString = " + getMixedContent(c.mixed))
        (c.POS == 1 && getMixedContent(c.mixed) == MANUAL_AMOUNT)
      }.seq.size == 1
    }
    //println(s"userEnteredOrderLines => \n${userEnteredOrderLines}")

    // Build a sequence of Double values to compute total from
    val grossTotal = userEnteredOrderLines.map { l =>
      // Get cell containing amount (Position 5)
      getLineAmount(l)
      //      val amountCellSeq = l.C.filter { c => c.POS == 5 }
      //      // Make sure we have a single match
      //      if (amountCellSeq.size == 1) {
      //        val amountCellString = getMixedContent(amountCellSeq(0).mixed)
      //        val amountCellDoubleValue = amountCellString match {
      //          case "" => 0d
      //          case s => s.toDouble
      //        }
      //        Option(amountCellDoubleValue)
      //      } else {
      //        None
      //      }
    }

    // Compute total for Some double value return None otherwise.
    if (grossTotal.exists { x =>
      x match {
        case Some(y) => true
        case None    => false
      }
    }) {
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
   * Update gross amount total line
   */
  def updateGrossAmountLine(fract: MATRICETypable): (MATRICETypable, Option[Double]) = {
    // 1. Compute gross amount
    val grossTotalAmntOpt = getTotalGrossAmount(fract)
    println(s"1. grossTotalAmntOpt = ${grossTotalAmntOpt}")
    // 2. Prepare gross amount line if some gross amount returned 
    val newComputedGrossTotalLineOpt = grossTotalAmntOpt match {
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
    println(s"2. newComputedGrossTotalLineOpt = ${newComputedGrossTotalLineOpt}")

    // 3. Replace gross amount line if available
    val fractionUpdatedWithGrossTotal = newComputedGrossTotalLineOpt match {
      case Some(newComputedGrossTotalLine) =>
        // Find current gross total line
        val grossTotalLineIndex = fract.L.indexWhere { l =>
          l.C.exists { c => (c.POS == 1 && getMixedContent(c.mixed) == GROSS_AMOUNT_TOTAL) }
        }
        println(s"3. grossTotalLineIndex = ${grossTotalLineIndex}")
        // Remove existing gross total line
        val linesWithoutGrossTotal = fract.L.filterNot { l =>
          l.C.exists { c => (c.POS == 1 && getMixedContent(c.mixed) == GROSS_AMOUNT_TOTAL) }
        }

        println(s"3. linesWithoutGrossTotal.size = ${linesWithoutGrossTotal.size}")

        // Insert newGrossTotalComputedLine to original FRACTION lines
        val newLines = replaceAt(newComputedGrossTotalLine, linesWithoutGrossTotal, grossTotalLineIndex)
        println(s"3. newLines.size = ${newLines.size}")

        val updatedFraction = MATRICEType(newLines: _*)
        // Insert newGrossTotalComputedLine to original FRACTION lines
        //val updatedSeqL = fract.L :+ newComputedGrossTotalLine
        // Create a new FRACTION 
        //val updatedFraction = MATRICEType(updatedSeqL: _*)
        println(s"3. updatedFraction = ${updatedFraction}")
        updatedFraction
      case None =>
        // Preserve fraction
        println(s"3. Preserved fraction = ${fract}")
        fract
    }
    (fractionUpdatedWithGrossTotal, grossTotalAmntOpt)
  }

  /**
   * Update rate lines
   */
  def updateRateLines(grossValue: Double, fract: MATRICETypable): MATRICETypable = {

    // Get rate lines
    val rateLinesWithIndex = fract.L.zipWithIndex.filter {
      case (l, i) => l.C.exists {
        c =>
          (c.POS == 1 && (
            getMixedContent(c.mixed) == REDUCTION_RATE ||
            getMixedContent(c.mixed) == DISCOUNT_RATE ||
            getMixedContent(c.mixed) == VAT_RATE))
      }
    }

    //foldLeft(List[L]())((acc,l) => l :: acc )
    val updatedRateLinesWithIndex =
      for { (rateLine, i) <- rateLinesWithIndex } yield {

        val rateValue = getMixedContent(rateLine.C.find { c => c.POS == 3 }.get.mixed) match {
          case "" => 0d
          case s  => s.toDouble / 100d
        }
        val rateAmount = BigDecimal(rateValue * grossValue).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
        val updateCPos5 = C(setMixedContent(f"$rateAmount%2.2f"), POS = 5)

        val updatedCSeq = Seq(
          rateLine.C.find { c => c.POS == 1 }.get,
          rateLine.C.find { c => c.POS == 2 }.get,
          rateLine.C.find { c => c.POS == 3 }.get,
          rateLine.C.find { c => c.POS == 4 }.get,
          updateCPos5);
        // Return a new line L with computed C position 5 and leaving rest unchanged
        (L(C = updatedCSeq, POS = rateLine.POS), i)
      }

    val updatedFractionL = replaceAt(linesToUpdateAtIndex = updatedRateLinesWithIndex, sequenceToUpdate = fract.L)

    val updatedFraction = MATRICEType(updatedFractionL: _*)
    updatedFraction

  }

  /**
   * Update net total line
   */
  def updateNetTotalLine(fract: MATRICETypable): MATRICETypable = {

    // Get all lines amount to sum
    val amountLines = fract.L.filterNot { l =>
      l.C.exists {
        c =>
          (c.POS == 1 && (
            getMixedContent(c.mixed) == MANUAL_AMOUNT ||
            getMixedContent(c.mixed) == NET_AMOUNT_TOTAL))
      }
    }

    val netAmountSum = amountLines.foldLeft(0d) {
      (acc, l) =>
        getLineAmount(l) match {
          case Some(amount) => acc + amount
          case None         => acc
        }
    }

    val updatedNetAmountLine = getTotalNetLine(netAmountSum)
    val currentNetAmountLineIndex = fract.L.indexWhere { l => l.C.exists { c => (c.POS == 1 && getMixedContent(c.mixed) == NET_AMOUNT_TOTAL) } }
    val updatedOrderLines = replaceAt(elementToInsert = updatedNetAmountLine, sequenceToUpdate = fract.L, index = currentNetAmountLineIndex)
    val updatedFraction = MATRICEType(updatedOrderLines: _*)
    updatedFraction

  }

  /**
   * Generic insertion of line element `L` in a sequence of lines `Seq[L]` at `index` position.
   */
  def replaceAt(elementToInsert: L, sequenceToUpdate: Seq[L], index: Int): Seq[L] = {
    val resSeq = for ((el, i) <- sequenceToUpdate.zipWithIndex) yield {
      if (i != index) Seq(el) else Seq(elementToInsert, el)
    }
    resSeq.flatten
  }

  /**
   * Generic update of lines element `L` ain a sequence of lines `Seq[L]` at `index` position.
   */
  def replaceAt(linesToUpdateAtIndex: Seq[(L, Int)], sequenceToUpdate: Seq[L]): Seq[L] = {
    val resSeq = for ((currentLine, i) <- sequenceToUpdate.zipWithIndex) yield {
      val elToUpdateOpt = linesToUpdateAtIndex.find { case (l, j) => j == i }
      elToUpdateOpt match {
        case Some(elToUpdateWithIdx) => elToUpdateWithIdx._1
        case None                    => currentLine
      }
    }
    resSeq
  }

  /**
   * Computes percentages, gross and net amounts given provided order details.
   */
  def computeOrderFigures(carP: CARACTERISTIQUE): CARACTERISTIQUE = {

    // Extract FRACTION if available create a new FRACTION creating, updating, copying original FRACTION data as necessary.
    val updatedFractionOpt = carP.FRACTION.map { fract =>

      // 1. Proceed with MANUAL_AMOUNT / GROSS_TOTAL
      val (fractionWithGrossTotalUpdated, grossTotalOpt) = updateGrossAmountLine(fract)

      // 4. Find and update REDUCTION_RATE
      val fractionUpdated = grossTotalOpt match {
        case Some(grossTotal) =>
          val fractionWithRatesUpdated = updateRateLines(grossTotal, fractionWithGrossTotalUpdated)
          fractionWithRatesUpdated
        case None => fractionWithGrossTotalUpdated
      }

      // 8. Compute and replace NET_AMOUNT_TOTAL      

      fractionUpdated
    }

    // Create new CARACTERISTIQUE 
    val carUpdated = CARACTERISTIQUE(carP.CAR1, carP.CAR2, carP.CAR3, carP.CAR4, carP.CAR5, carP.CAR6, carP.CARSET, carP.ETAT, carP.CALCUL, updatedFractionOpt)

    carUpdated

  }

}