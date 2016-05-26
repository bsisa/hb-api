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

  val ROUNDING_PRECISION = BigDecimal(0.05)
  
  val APPLIED_RATE = "APPLIED_RATE"
  val APPLIED_AMOUNT = "APPLIED_AMOUNT"
  val GROSS_AMOUNT_TOTAL = "TOTAL_GROSS"
  val MANUAL_AMOUNT = "MANUAL_AMOUNT"
  val NET_AMOUNT_TOTAL = "TOTAL_NET"
  val NET_AMOUNT_TOTAL_INCL_TAX = "TOTAL_NET_INCL_TAX"
  val TAX_RATE = "TAX_RATE"

  /**
   * Rounding helper useful for currency rounding
   * 
   * Examples:  
   * assert(round(BigDecimal(1000.1367478), BigDecimal(0.05)) ==  BigDecimal(1000.15))
   * assert(round(BigDecimal(1000.16), BigDecimal(0.05)) ==  BigDecimal(1000.15))
   * assert(round(BigDecimal(1000.1749), BigDecimal(0.05)) ==  BigDecimal(1000.15))
   * assert(round(BigDecimal(1000.175), BigDecimal(0.05)) ==  BigDecimal(1000.20))
   * assert(round(BigDecimal(1000.19), BigDecimal(0.05)) ==  BigDecimal(1000.20))
   */
  def round(number: BigDecimal, precision: BigDecimal): BigDecimal = {
    val roundedNumber = number.setScale(precision.scale, BigDecimal.RoundingMode.HALF_UP)
    val division = (roundedNumber / precision)
    val truncated = division.setScale(0, BigDecimal.RoundingMode.HALF_UP)
    val result = truncated * precision
    result
  }

  def getTotalNetInclTaxLine(total: Double): L = {

    val totalNetInclTaxXml = <L POS="2">
                               <C POS="1">{ s"$NET_AMOUNT_TOTAL_INCL_TAX" }</C>
                               <C POS="2">Total brut</C>
                               <C POS="3"/>
                               <C POS="4"/>
                               <C POS="5">{ f"$total%2.2f" }</C>
                               <C POS="6">false</C>
                             </L>

    ElfinFormat.lFromXml(totalNetInclTaxXml)
  }

  def getTotalGrossLine(total: Double): L = {

    val totalGrossXml = <L POS="2">
                          <C POS="1">{ s"$GROSS_AMOUNT_TOTAL" }</C>
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
                        <C POS="1">{ s"$NET_AMOUNT_TOTAL" }</C>
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
        case ""  => 0d // Consider empty string as zero
        case "." => 0d // Consider a single point a zero
        case s   => s.toDouble
      }
      Option(amountCellDoubleValue)
    } else {
      None
    }
  }

  /**
   * Return the rate found at cell C position 3 for a given line L
   * if available.
   * Remark: An empty string is considered as value 0.0
   */
  def getLineRate(l: L): Option[Double] = {

    // Get cell containing rate (Position 3)
    val rateCellSeq = l.C.filter { c => c.POS == 3 }
    // Make sure we have a single match
    if (rateCellSeq.size == 1) {
      val rateCellString = getMixedContent(rateCellSeq(0).mixed)
      val rateCellDoubleValue = rateCellString match {
        case ""  => 0d // Consider empty string as zero
        case "." => 0d // Consider a single point a zero
        case s   => s.toDouble / 100d
      }
      Option(rateCellDoubleValue)
    } else {
      None
    }
  }

  /**
   * Returns the line type found at cell position 1.
   * If no or several cells are found with POS attribute 1 None is return.
   */
  def getLineType(l: L): Option[String] = {
    val lineTypeCellSeq = l.C.filter { c => c.POS == 1 }
    // Make sure we have a single match
    if (lineTypeCellSeq.size == 1) {
      val lineType = getMixedContent(lineTypeCellSeq(0).mixed)
      Option(lineType)
    } else {
      None
    }
  }

  /**
   * Compute amount from: `rateValue` * `amountTarget` with defined rounding rule.
   */
  def computeAmount(rateValue: Double, amountTarget: Double): Double = BigDecimal(rateValue * amountTarget).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  /**
   * Return a new line entry only updating original line entry amount cell with value `amount`
   */
  def updateLineAmount(line: L, amount: Double): L = {

    val newCPos5 = C(setMixedContent(f"$amount%2.2f"), POS = 5)

    val newCSeq = Seq(
      line.C.find { c => c.POS == 1 }.get,
      line.C.find { c => c.POS == 2 }.get,
      line.C.find { c => c.POS == 3 }.get,
      line.C.find { c => c.POS == 4 }.get,
      newCPos5);

    // Return a new line L with updated C position 5 and leaving other cells unchanged
    L(C = newCSeq, POS = line.POS)
  }

  /**
   * Return single gross amount line amount value as Some(Double).
   * If None is returned it can be considered a data integrity problem.
   */
  def getGrossAmount(fract: MATRICETypable): Option[Double] = {
    // Filter fraction preserving only gross amount line
    val grossAmtLines = fract.L.filter { l => l.C.exists { c => c.POS == 1 && getMixedContent(c.mixed) == GROSS_AMOUNT_TOTAL } }
    if (grossAmtLines.size == 1) {
      getLineAmount(grossAmtLines(0))
    } else if (grossAmtLines.size > 1) {
      // Only one GROSS_AMOUNT_TOTAL line allowed: Data integrity problem
      None
    } else {
      // The MUST be one gross amount line: Data integrity problem
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
    val grossTotal = userEnteredOrderLines.map { l => getLineAmount(l) }

    // Compute total for Some double value return None otherwise.
    if (grossTotal.exists { x => x.isDefined }) {
      val grossTotalOpt = grossTotal.foldLeft(0d) { (acc, el) =>
        el match {
          case Some(nb) => acc + nb
          case None     => acc
        }
      }
      Some(grossTotalOpt)
    } else {
      // Check there is a single 
      None
    }
  }

  /**
   *  Replace first line matching `line` type in `fract.L` matrix with `line`
   *  If no matching is found returns `fract` unchanged.
   */
  def replaceFirstLineMatchingLineType(fract: MATRICETypable, line: L): MATRICETypable = {

    val lineTypeOpt = getLineType(line);

    lineTypeOpt match {
      case Some(lineType) =>
        // Find line index for lineType
        val lineIndex = fract.L.indexWhere { l =>
          l.C.exists { c => (c.POS == 1 && getMixedContent(c.mixed) == lineType) }
        }
        // Remove existing line at lineIndex
        //val linesWithoutFirstLineOfLineType = fract.L.slice(0, lineIndex) ++ fract.L.slice(lineIndex + 1, fract.L.size)

        // Insert line at lineIndex
        //val linesWithNewLineAtLineIndex = insertAt(line, linesWithoutFirstLineOfLineType, lineIndex)

        // Replace line at lineIndex        
        val linesWithNewLineAtLineIndex = replaceAt(line, fract.L, lineIndex)

        val updatedFraction = MATRICEType(linesWithNewLineAtLineIndex: _*)
        updatedFraction
      case None => fract
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
          // Ok
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
    //val fractionUpdatedWithGrossTotal = newComputedGrossTotalLineOpt match {
    val fractionAndGrossTotal = newComputedGrossTotalLineOpt match {
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
        val newLines = insertAt(newComputedGrossTotalLine, linesWithoutGrossTotal, grossTotalLineIndex)
        println(s"3. newLines.size = ${newLines.size}")

        val updatedFraction = MATRICEType(newLines: _*)
        println(s"3. updatedFraction = ${updatedFraction}")
        (updatedFraction, grossTotalAmntOpt)
      case None =>
        // Preserve fraction
        println(s"3. Preserved fraction = ${fract}")
        (fract, getGrossAmount(fract))
    }
    //(fractionUpdatedWithGrossTotal, grossTotalAmntOpt)
    fractionAndGrossTotal
  }

  /**
   * Update rate lines
   */
  def updateRateLines(grossValue: Double, fract: MATRICETypable): MATRICETypable = {

    // Get rate lines
    val rateLinesWithIndex = fract.L.zipWithIndex.filter {
      case (l, i) => l.C.exists {
        c =>
          (c.POS == 1 && getMixedContent(c.mixed) == APPLIED_RATE)
      }
    }

    val updatedRateLinesWithIndex =
      for { (rateLine, i) <- rateLinesWithIndex } yield {

        // TODO: refactor using getLineRate
        val rateValue = getMixedContent(rateLine.C.find { c => c.POS == 3 }.get.mixed) match {
          case "" => 0d
          case s  => s.toDouble / 100d
        }
        // TODO: refactor using computeAmount
        val rateAmount = BigDecimal(rateValue * grossValue).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

        // TODO: refactor using updateLineAmount

        val updateCPos5 = C(setMixedContent(f"$rateAmount%2.2f"), POS = 5)

        val updatedCSeq = Seq(
          rateLine.C.find { c => c.POS == 1 }.get,
          rateLine.C.find { c => c.POS == 2 }.get,
          rateLine.C.find { c => c.POS == 3 }.get,
          rateLine.C.find { c => c.POS == 4 }.get,
          updateCPos5);
        // Return a new line L with computed C position 5 and leaving other cells unchanged
        (L(C = updatedCSeq, POS = rateLine.POS), i)
      }

    val updatedFractionL = replaceAt(linesToUpdateAtIndex = updatedRateLinesWithIndex, sequenceToUpdate = fract.L)

    val updatedFraction = MATRICEType(updatedFractionL: _*)
    updatedFraction

  }

  /**
   * Takes a sequence of L and computes the sum of each L amount.
   * If no amount are available returns 0.
   */
  def computeSumForAmountLines(amountLines: Seq[L]): Double = {
    val sum = amountLines.foldLeft(0d) {
      (acc, l) =>
        getLineAmount(l) match {
          case Some(amount) => acc + amount
          case None         => acc
        }
    }
    sum
  }

  /**
   * Computes tax amount from NET_AMOUNT_TOTAL and TAX_RATE lines.
   * We expect a single NET_AMOUNT_TOTAL line.
   * We expect a single TAX_RATE line.
   * If the above constraints are not matched the update is cancelled
   * but no notification, exception or message are currently triggered,
   * Only an error log will be output.
   */
  def updateTaxRateLine(fract: MATRICETypable): MATRICETypable = {

    // We expect a single net amount line but support several
    val netAmountLines = fract.L.filter { l =>
      l.C.exists {
        c =>
          (c.POS == 1 && (getMixedContent(c.mixed) == NET_AMOUNT_TOTAL))
      }
    }

    val taxRateLines = fract.L.filter { l =>
      l.C.exists {
        c =>
          (c.POS == 1 && (getMixedContent(c.mixed) == TAX_RATE))
      }
    }

    if (netAmountLines.size == 1 && taxRateLines.size == 1) {
      // Get lines
      val netAmountLine = netAmountLines(0)
      val taxRateLine = taxRateLines(0)
      // Get amount and rate
      val netAmount = getLineAmount(netAmountLine)
      val taxRate = getLineRate(taxRateLine)
      // Compute tax amount
      val taxAmount = computeAmount(rateValue = taxRate.getOrElse(0d), amountTarget = netAmount.getOrElse(0d))
      // Update matrix
      val newTaxRateLine = updateLineAmount(taxRateLine, taxAmount)

      val updatedFraction = replaceFirstLineMatchingLineType(fract, newTaxRateLine)
      updatedFraction

    } else {
      fract
    }
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
            getMixedContent(c.mixed) == NET_AMOUNT_TOTAL ||
            getMixedContent(c.mixed) == TAX_RATE ||
            getMixedContent(c.mixed) == NET_AMOUNT_TOTAL_INCL_TAX))
      }
    }

    println(s">>>> amountLines: \n${amountLines}")

    val netAmountSum = computeSumForAmountLines(amountLines)

    val updatedNetAmountLine = getTotalNetLine(netAmountSum)

    // TODO: Check refactoring with replaceFirstLineMatchingLineType
    //val currentNetAmountLineIndex = fract.L.indexWhere { l => l.C.exists { c => (c.POS == 1 && getMixedContent(c.mixed) == NET_AMOUNT_TOTAL) } }
    //val updatedOrderLines = replaceAt(elementToInsert = updatedNetAmountLine, sequenceToUpdate = fract.L, index = currentNetAmountLineIndex)
    //val updatedFraction = MATRICEType(updatedOrderLines: _*)

    val updatedFraction = replaceFirstLineMatchingLineType(fract, updatedNetAmountLine)
    updatedFraction
  }

  def updateNetInclTaxTotalLine(fract: MATRICETypable): MATRICETypable = {

    // We expect a single net amount line but support several
    val netAmountLines = fract.L.filter { l =>
      l.C.exists {
        c =>
          (c.POS == 1 && (getMixedContent(c.mixed) == NET_AMOUNT_TOTAL))
      }
    }

    val taxRateLines = fract.L.filter { l =>
      l.C.exists {
        c =>
          (c.POS == 1 && (getMixedContent(c.mixed) == TAX_RATE))
      }
    }

    if (netAmountLines.size == 1 && taxRateLines.size == 1) {
      // Get lines
      val netAmountLine = netAmountLines(0)
      val taxRateLine = taxRateLines(0)
      // Get amount and rate
      val netAmount = getLineAmount(netAmountLine)
      val taxAmount = getLineAmount(taxRateLine)

      val netAmountInclTax = netAmount.getOrElse(0d) + taxAmount.getOrElse(0d);
      val netAmountInclTaxLine = getTotalNetInclTaxLine(netAmountInclTax)

      val updatedFraction = replaceFirstLineMatchingLineType(fract, netAmountInclTaxLine)
      updatedFraction

    } else {
      fract
    }
  }

  /**
   * Generic insertion of line element `L` in a sequence of lines `Seq[L]` at `index` position.
   */
  def insertAt(elementToInsert: L, sequenceToUpdate: Seq[L], index: Int): Seq[L] = {
    // TODO: test view usage. sequenceToUpdate.view.zipWithIndex
    val resSeq = for ((el, i) <- sequenceToUpdate.zipWithIndex) yield {
      if (i != index) Seq(el) else Seq(elementToInsert, el)
    }
    resSeq.flatten
  }

  /**
   * Generic replacement of line element `L` in a sequence of lines `Seq[L]` at `index` position.
   */
  def replaceAt(elementToInsert: L, sequenceToUpdate: Seq[L], index: Int): Seq[L] = {
    // TODO: test view usage. sequenceToUpdate.view.zipWithIndex
    val resSeq = for ((el, i) <- sequenceToUpdate.zipWithIndex) yield {
      if (i != index) el else elementToInsert
    }
    resSeq
  }

  /**
   * Generic update of lines element `L` ain a sequence of lines `Seq[L]` at `index` position.
   */
  def replaceAt(linesToUpdateAtIndex: Seq[(L, Int)], sequenceToUpdate: Seq[L]): Seq[L] = {
    // TODO: test view usage. sequenceToUpdate.view.zipWithIndex
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

      // 1. Proceed with MANUAL_AMOUNT GROSS_TOTAL computation
      val (fractionWithGrossTotalUpdated, grossTotalOpt) = updateGrossAmountLine(fract)

      // 2. Find and update APPLIED_RATE
      val fractionWithRates = grossTotalOpt match {
        case Some(grossTotal) =>
          val fractionWithRatesUpdated = updateRateLines(grossTotal, fractionWithGrossTotalUpdated)
          fractionWithRatesUpdated
        case None => fractionWithGrossTotalUpdated
      }

      // 3. Compute net total (NET_AMOUNT_TOTAL)
      val fractionWithNetTotal = updateNetTotalLine(fractionWithRates)

      // 4. Compute tax amount (NET_AMOUNT_TOTAL * TAX_RATE)
      val fractionWithTaxAmount = updateTaxRateLine(fractionWithNetTotal)

      // 5. Compute net total including tax (NET_AMOUNT_TOTAL + tax amount)
      val fractionWithNetInclTaxAmount = updateNetInclTaxTotalLine(fractionWithTaxAmount)

      fractionWithNetInclTaxAmount
    }

    // Create new CARACTERISTIQUE 
    val carUpdated = CARACTERISTIQUE(carP.CAR1, carP.CAR2, carP.CAR3, carP.CAR4, carP.CAR5, carP.CAR6, carP.CARSET, carP.ETAT, carP.CALCUL, updatedFractionOpt)

    carUpdated

  }

}