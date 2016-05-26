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
 * test-only test.ch.bsisa.hyperbird.orders.OrderUtilRoundingSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class OrderUtilRoundingSpec extends BaseSerialisationSpec with PlaySpecification {

  /**
   * Test Use Case provided by end-user.
   *
   * TOTAL_GROSS = 8726
   *
   * TOTAL_NET = TOTAL_GROSS + ( TOTAL_GROSS * -0.01 ) + ( TOTAL_GROSS * -0.02 ) + 5 = 1144.75 - 11.45 - 22.90 + 5 = 1115.40
   * TAX_RATE computed amount =  TOTAL_NET * TAX_RATE = 1115.40 * 0.08 = 89.23
   * TOTAL_NET_INCL_TAX = TOTAL_NET + ( TOTAL_NET * TAX_RATE ) = 1115.40 + 89.23 = 1204.63
   */

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
          <C POS="1">TOTAL_GROSS</C>
          <C POS="2">Total brut</C>
          <C POS="3"/>
          <C POS="4"/>
          <C POS="5">8726</C>
          <C POS="6">false</C>
        </L>
        <L POS="2">
          <C POS="1">APPLIED_RATE</C>
          <C POS="2">Rabais</C>
					<C POS="3">-10.00</C>
          <C POS="4">%</C>
          <C POS="5">0.00</C>
          <C POS="6">true</C>
        </L>
        <L POS="3">
          <C POS="1">APPLIED_RATE</C>
          <C POS="2">Escompte</C>
          <C POS="3">-2.0</C>
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
          <C POS="1">ROUNDING_AMOUNT</C>
          <C POS="2">Arrondi</C>
          <C POS="3"/>
          <C POS="4"/>
          <C POS="5">-0.35</C>
          <C POS="6">true</C>
        </L>
        <L POS="6">
          <C POS="1">TAX_RATE</C>
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

  val carIn = ElfinFormat caracteristiqueFromXml carInXml
  val carOut = OrderUtil computeOrderFigures carIn

  println(">>>>> \n" + ElfinFormat.caracteristiqueToJson(carOut) + "\n")

  // ==========================================================================
  //     Test input 
  // ==========================================================================  
  
  s"The number of carIn.FRACTION.L " should {
    s"equal 7" in {
      val nbLinesOpt = carIn.FRACTION.map { fractionMat =>
        fractionMat.L.foldLeft(0)((acc, l) => acc + 1)
      }
      nbLinesOpt.get mustEqual 7
    }
  }

  s"Amounts of carIn.FRACTION.L " should {
    "equal 8726 for L POS='1' manual record (TOTAL_GROSS) " in {
      val lPos1 = carIn.FRACTION.map(f => f.L(0)).get
      OrderUtil getLineAmount lPos1 mustEqual Some(8726)
    }
    "equal -10% for L POS='2' submitted reduction rate " in {
      val lPos2 = carIn.FRACTION.map(f => f.L(1)).get
      OrderUtil getLineRate lPos2 mustEqual Some(-10d/100d)
    }    
    "equal 0.0 for L POS='2' not yet computed reduction amount " in {
      val lPos2 = carIn.FRACTION.map(f => f.L(1)).get
      OrderUtil getLineAmount lPos2 mustEqual Some(0.0)
    }
    "equal -2% for L POS='3' submitted discount rate " in {
      val lPos3 = carIn.FRACTION.map(f => f.L(2)).get
      OrderUtil getLineRate lPos3 mustEqual Some(-2d/100d)
    }
    "equal 0.0 for L POS='3' not yet computed discount amount " in {
      val lPos3 = carIn.FRACTION.map(f => f.L(2)).get
      OrderUtil getLineAmount lPos3 mustEqual Some(0.0)
    }
    "equal 9999.0 for L POS='4' not yet computed net total amount " in {
      val lPos4 = carIn.FRACTION.map(f => f.L(3)).get
      OrderUtil getLineAmount lPos4 mustEqual Some(9999.0)
    }
    "equal -0.35 for L POS='5' rounding amount " in {
      val lPos5 = carIn.FRACTION.map(f => f.L(4)).get
      OrderUtil getLineAmount lPos5 mustEqual Some(-0.35)
    }
    "equal 8% for L POS='6' submitted VAT rate " in {
      val lPos6 = carIn.FRACTION.map(f => f.L(5)).get
      OrderUtil getLineRate lPos6 mustEqual Some(8.0d/100d)
    }    
    "equal 0.0 for L POS='6' not yet computed VAT amount " in {
      val lPos6 = carIn.FRACTION.map(f => f.L(5)).get
      OrderUtil getLineAmount lPos6 mustEqual Some(0.0)
    }
    "equal 9999.0 for L POS='9' not yet computed Tax incl. net total " in {
      val lPos7 = carIn.FRACTION.map(f => f.L(6)).get
      OrderUtil getLineAmount lPos7 mustEqual Some(9999.00)
    }
  }

  // ==========================================================================
  //     Test output 
  // ==========================================================================  
  
  s"The number of carOut.FRACTION.L " should {
    s"equal 7" in {
      val nbLinesOpt = carOut.FRACTION.map { fractionMat =>
        fractionMat.L.foldLeft(0)((acc, l) => acc + 1)
      }
      nbLinesOpt.get mustEqual 7
    }
  }  
  
  s"Amounts of carOut.FRACTION.L " should {
    "equal 8726 for L POS='1' manual record (TOTAL_GROSS) " in {
      val lPos1 = carOut.FRACTION.map(f => f.L(0)).get
      OrderUtil getLineAmount lPos1 mustEqual Some(8726)
    }
    "equal -10% for L POS='2' submitted reduction rate " in {
      val lPos2 = carOut.FRACTION.map(f => f.L(1)).get
      OrderUtil getLineRate lPos2 mustEqual Some(-10d/100d)
    }    
    "equal -872.60 for L POS='2' computed reduction amount " in {
      val lPos2 = carOut.FRACTION.map(f => f.L(1)).get
      OrderUtil getLineAmount lPos2 mustEqual Some(-872.60)
    }
    "equal -2% for L POS='3' submitted discount rate " in {
      val lPos3 = carOut.FRACTION.map(f => f.L(2)).get
      OrderUtil getLineRate lPos3 mustEqual Some(-2d/100d)
    }
    "equal (7853.4 * -0.02) −157.068 => −157.05 for L POS='3' computed discount amount " in {
      val lPos3 = carOut.FRACTION.map(f => f.L(2)).get
      OrderUtil getLineAmount lPos3 mustEqual Some(-157.05)
    }
    "equal 7696.35 for L POS='4' computed net total amount " in {
      val lPos4 = carOut.FRACTION.map(f => f.L(3)).get
      OrderUtil getLineAmount lPos4 mustEqual Some(7696.35)
    }
    "equal -0.35 for L POS='5' rounding amount " in {
      val lPos5 = carOut.FRACTION.map(f => f.L(4)).get
      OrderUtil getLineAmount lPos5 mustEqual Some(-0.35)
    }
    "equal 8% for L POS='6' submitted VAT rate " in {
      val lPos6 = carOut.FRACTION.map(f => f.L(5)).get
      OrderUtil getLineRate lPos6 mustEqual Some(8.0d/100d)
    }    
    "equal (7696 * 0.08 = 615.68) => 615.70 for L POS='6' computed VAT amount " in {
      val lPos6 = carOut.FRACTION.map(f => f.L(5)).get
      OrderUtil getLineAmount lPos6 mustEqual Some(615.70)
    }
    "equal 8311.70 = 7696.35 - 0.35 + 615.70 for L POS='9' computed Tax incl. net total " in {
      val lPos7 = carOut.FRACTION.map(f => f.L(6)).get
      OrderUtil getLineAmount lPos7 mustEqual Some(8311.70)
    }
  }  

}