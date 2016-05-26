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
 * test-only test.ch.bsisa.hyperbird.orders.OrderUtilWithNoAmountLinesSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class OrderUtilWithNoAmountLinesSpec extends BaseSerialisationSpec with PlaySpecification {

  
    val carInNoManualAmountXml =
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
      <C POS="1">TOTAL_GROSS</C>
      <C POS="2">Total brut</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">1000.00</C>
      <C POS="6">false</C>
    </L>
    <L POS="2">
      <C POS="1">APPLIED_RATE</C>
      <C POS="2">Rabais</C>
      <C POS="3">1.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="3">
      <C POS="1">APPLIED_RATE</C>
      <C POS="2">Escompte</C>
      <C POS="3">2.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="4">
      <C POS="1">TOTAL_NET</C>
      <C POS="2">Total net</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">9999.00</C>
      <C POS="6">false</C>
    </L>
    <L POS="5">
      <C POS="1">APPLIED_AMOUNT</C>
      <C POS="2">Arrondi</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="6">
      <C POS="1">APPLIED_RATE</C>
      <C POS="2">TVA</C>
      <C POS="3">8.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="7">
      <C POS="1">TOTAL_NET_INCL_TAX</C>
      <C POS="2">Total net TTC</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">9999.00</C>
      <C POS="6">false</C>
    </L>
  </FRACTION>
</CARACTERISTIQUE>
      
  val carInNoManualAmount = ElfinFormat caracteristiqueFromXml carInNoManualAmountXml
  val carOutNoManualAmount = OrderUtil computeOrderFigures carInNoManualAmount      

  s"The number of carInNoManualAmount.FRACTION.L " should {
    s"equal 7" in {
     val nbLinesOpt = carInNoManualAmount.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     nbLinesOpt.get mustEqual 7
    }
  }
  
  s"The number of carOutNoManualAmount.FRACTION.L " should {
    s"equal 7" in {
     val nbLinesOpt = carOutNoManualAmount.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     nbLinesOpt.get mustEqual 7
    }
  }  
  
  s"Amounts of carInNoManualAmount.FRACTION.L " should {
    "equal 1000 for L POS='1' manual single total gross record " in {
      val lPos1 = carInNoManualAmount.FRACTION.map ( f => f.L(0)).get
      OrderUtil getLineAmount lPos1  mustEqual Some(1000)
    }
    "equal 0.0 for L POS='2' not yet computed reduction amount " in {
      val lPos2 = carInNoManualAmount.FRACTION.map ( f => f.L(1)).get
      OrderUtil getLineAmount lPos2  mustEqual Some(0.0)
    }
    "equal 0.0 for L POS='3' not yet computed discount amount " in {
      val lPos3 = carInNoManualAmount.FRACTION.map ( f => f.L(2)).get
      OrderUtil getLineAmount lPos3  mustEqual Some(0.0)
    }
    "equal 9999.0 for L POS='4' not yet computed net amount " in {
      val lPos4 = carInNoManualAmount.FRACTION.map ( f => f.L(3)).get
      OrderUtil getLineAmount lPos4  mustEqual Some(9999.0)
    }
    "equal 0.0 for L POS='5' rounding amount " in {
      val lPos5 = carInNoManualAmount.FRACTION.map ( f => f.L(4)).get
      OrderUtil getLineAmount lPos5  mustEqual Some(0.0)
    }
    "equal 0.0 for L POS='6' not yet computed VAT amount " in {
      val lPos6 = carInNoManualAmount.FRACTION.map ( f => f.L(5)).get
      OrderUtil getLineAmount lPos6  mustEqual Some(0.0)
    }
  }
  
  
  s"Amounts of carOutNoManualAmount.FRACTION.L " should {
    "equal 1000 for L POS='1' manual single total gross record " in {
      val lPos1 = carOutNoManualAmount.FRACTION.map ( f => f.L(0)).get
      OrderUtil getLineAmount lPos1  mustEqual Some(1000)
    }
    "equal 2% (10.0) for L POS='2' computed reduction rate " in {
      val lPos2 = carOutNoManualAmount.FRACTION.map ( f => f.L(1)).get
      OrderUtil getLineAmount lPos2  mustEqual Some(10.0)
    }
    "equal 1% (20.20) for L POS='3' computed discount rate " in {
      val lPos3 = carOutNoManualAmount.FRACTION.map ( f => f.L(2)).get
      OrderUtil getLineAmount lPos3  mustEqual Some(20.20)
    }
    "equal 1030.2 for L POS='4' computed net amount " in {
      val lPos4 = carOutNoManualAmount.FRACTION.map ( f => f.L(3)).get
      OrderUtil getLineAmount lPos4  mustEqual Some(1030.20)
    }
    "equal 0.00 for L POS='5' rounding amount " in {
      val lPos5 = carOutNoManualAmount.FRACTION.map ( f => f.L(4)).get
      OrderUtil getLineAmount lPos5  mustEqual Some(0.0)
    }
    "equal 82.40 (8.0% rounded 82.416) for L POS='6' computed VAT amount " in {
      val lPos6 = carOutNoManualAmount.FRACTION.map ( f => f.L(5)).get
      OrderUtil getLineAmount lPos6  mustEqual Some(82.40)
    }
    "equal 1112.60 for L POS='7' net amount incl. tax " in {
      val lPos6 = carOutNoManualAmount.FRACTION.map ( f => f.L(6)).get
      OrderUtil getLineAmount lPos6  mustEqual Some(1112.6)
    }
  }      
      
}