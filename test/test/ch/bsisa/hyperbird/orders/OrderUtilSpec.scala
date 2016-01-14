package test.ch.bsisa.hyperbird.orders

import test.ch.bsisa.hyperbird.util.BaseSerialisationSpec
import ch.bsisa.hyperbird.orders.OrderUtil
import ch.bsisa.hyperbird.model.format.ElfinFormat
import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._

/**
 * Tests ch.bsisa.hyperbird.io.OrderUtilSpec.computeOrderFigures(carP : CARACTERISTIQUE) function
 * 
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.orders.OrderUtilSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class OrderUtilSpec extends BaseSerialisationSpec with PlaySpecification {

  val carInXml =
<CARACTERISTIQUE>
  <CAR1 NOM="Surface au sol" UNITE="Ligne" VALEUR="100"/>
  <CAR2 NOM="Surface au sol" UNITE="Ligne" VALEUR="100"/>
  <CAR3 NOM="Surface au sol" UNITE="Taux TVA" VALEUR="100"/>
  <CARSET>
    <CAR NOM="" UNITE="" VALEUR="" POS="1"/>
    <CAR NOM="" UNITE="" VALEUR="" POS="2"/>
  </CARSET>
  <FRACTION>
    <L POS="1">
      <C POS="1">MANUAL_AMOUNT</C>
      <C POS="2">Item1</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">1050</C>
      <C POS="6">false</C>
    </L>
    <L POS="2">
      <C POS="1">MANUAL_AMOUNT</C>
      <C POS="2">Item2</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">94.75</C>
      <C POS="6">false</C>
    </L>
    <L POS="2">
      <C POS="1">TOTAL_GROSS</C>
      <C POS="2">Total brut</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">9999.00</C>
      <C POS="6">false</C>
    </L>
    <L POS="3">
      <C POS="1">REDUCTION_RATE</C>
      <C POS="2">Rabais</C>
      <C POS="3">1.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="4">
      <C POS="1">DISCOUNT_RATE</C>
      <C POS="2">Escompte</C>
      <C POS="3">2.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="5">
      <C POS="1">ROUNDING_AMOUNT</C>
      <C POS="2">Arrondi</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">5.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="6">
      <C POS="1">VAT_RATE</C>
      <C POS="2">TVA</C>
      <C POS="3">8.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="7">
      <C POS="1">TOTAL_NET</C>
      <C POS="2">Total net</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">9999.00</C>
      <C POS="6">false</C>
    </L>
  </FRACTION>
</CARACTERISTIQUE>
  
  val carIn = ElfinFormat.caracteristiqueFromXml(carInXml)
  val carOut = OrderUtil.computeOrderFigures(carP = carIn)


  s"The number of carIn.FRACTION.L " should {
    s"equals 7" in {
     val nbLinesOpt = carIn.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     val success = nbLinesOpt match {
       case Some(nbLines) => nbLines == 7 
       case None => false
     }
     success
    }
  }
  
  s"The number of carOut.FRACTION.L " should {
    s"equals 8" in {
     val nbLinesOpt = carOut.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     val success = nbLinesOpt match {
       case Some(nbLines) => nbLines == 8 
       case None => false
     }
     success
    }
  }  

}