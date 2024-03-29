package ch.bsisa.hyperbird.orders

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.model.format.Implicits._

import scala.math.BigDecimal

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
  def computeAmount(rateValue: Double, amountTarget: Double): Double = {
    val amount = BigDecimal(rateValue * amountTarget)
    round(amount, ROUNDING_PRECISION).toDouble
  }

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
      // There MUST be one gross amount line: Data integrity problem
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
   * Returns FRACTION with updated gross amount total line and total gross
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

        val updatedFraction = MATRICEType(newLines)
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
   * Return a tuple2 with updated `currTotal` and `currList` for `line`
   *  
   * Remark: computeOrderFigures utility
   */
  def updateTotalAndRateLine(line: L, currTotal: BigDecimal, currList: List[L]): (BigDecimal, List[L]) = {
    val rate = getLineRate(line)
    val lineAmount = computeAmount(rateValue = rate.getOrElse(0d), amountTarget = currTotal.toDouble)
    val updatedLine = updateLineAmount(line, lineAmount)
    val newUpdatedList = updatedLine :: currList
    // 2) update current sub total adding this line rate amount
    val newTotal = currTotal + lineAmount
    (newTotal, newUpdatedList)
  }

  /**
   * Return a tuple2 with updated `currTotal` and `currList` for `line`
   *  
   * Remark: computeOrderFigures utility
   */
  def updateTotalFromAmountLine(line: L, currTotal: BigDecimal, currList: List[L]): (BigDecimal, List[L]) = {
    val lineAmount = getLineAmount(line).getOrElse(0d)
    val newTotal = currTotal + lineAmount
    val newUpdatedList = line :: currList
    (newTotal, newUpdatedList)
  }

  /**
   * Computes percentages, gross and net amounts given provided order details.
   */
  def computeOrderFigures(carP: CARACTERISTIQUE): CARACTERISTIQUE = {

    /*
     *  Extracts Option FRACTION.
     *  Perform transformation contained in map function if some FRACTION is available,
     *  builds a new FRACTION creating, updating, copying original FRACTION data as necessary.
     */
    val updatedFractionOpt = carP.FRACTION.map { fract =>

      // 1. Proceed with MANUAL_AMOUNT GROSS_TOTAL computation
      val (fractionWithGrossTotalUpdated, grossTotalOpt) = updateGrossAmountLine(fract)

      // 2. Proceed line by line computing intermediary total at each computation step (line)
      // Lines ordering is vital and must be preserved.
      val updatedTotalAndOrderLines = fractionWithGrossTotalUpdated.L.foldLeft((BigDecimal(0), List[L]())) { (currTotalAndUpdatedList, line) =>

        val (currTotal, currUpdatedList) = currTotalAndUpdatedList

        val nextTotalAndUpdatedList: (BigDecimal, List[L]) = getLineType(line) match {
          case Some(APPLIED_RATE) =>
            updateTotalAndRateLine(line, currTotal, currUpdatedList)
          case Some(APPLIED_AMOUNT) =>
            updateTotalFromAmountLine(line, currTotal, currUpdatedList)
          case Some(GROSS_AMOUNT_TOTAL) =>
            updateTotalFromAmountLine(line, currTotal, currUpdatedList)
          case Some(MANUAL_AMOUNT) =>
            // Preserve line `as is`. Manual amount are included in mandatory GROSS AMOUNT TOTAL. 
            (currTotal, line :: currUpdatedList)
          case Some(NET_AMOUNT_TOTAL) =>
            // Update NET AMOUNT TOTAL line amount with current sub total
            val updatedLine = updateLineAmount(line, currTotal.toDouble)
            // Keep current sub total unchanged.
            (currTotal, updatedLine :: currUpdatedList)
          case Some(NET_AMOUNT_TOTAL_INCL_TAX) =>
            // 1) Update NET AMOUNT TOTAL INCL TAX line with current sub total
            val updatedLine = updateLineAmount(line, currTotal.toDouble)
            // 2) Keep current sub total unchanged.
            (currTotal, updatedLine :: currUpdatedList)
          case Some(TAX_RATE) =>
            updateTotalAndRateLine(line, currTotal, currUpdatedList)
          case Some(unexpectedType) =>
            // TODO: Currently provide no feedback for unexpected line type. 
            println("WARNING: >>>> computeOrderFigures unexpectedType = " + unexpectedType)
            // Preserve line `as is`
            (currTotal, line :: currUpdatedList)
          case None =>
            // TODO: Should notify of a data integrity problem 
            println("WARNING: >>>> computeOrderFigures data integrity problem with line type.")
            // Preserve line `as is`
            (currTotal, line :: currUpdatedList)
        }
        nextTotalAndUpdatedList
      }

      // Build new List need to be reversed to preserve original List ordering.
      val updatedFraction = MATRICEType(updatedTotalAndOrderLines._2.reverse)
      updatedFraction
    }

    // Create new CARACTERISTIQUE 
    val carUpdated = CARACTERISTIQUE(carP.CAR1, carP.CAR2, carP.CAR3, carP.CAR4, carP.CAR5, carP.CAR6, carP.CARSET, carP.ETAT, carP.CALCUL, updatedFractionOpt)

    carUpdated

  }

}