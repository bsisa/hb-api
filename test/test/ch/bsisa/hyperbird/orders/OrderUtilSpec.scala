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
    <L POS="3">
      <C POS="1">TOTAL_GROSS</C>
      <C POS="2">Total brut</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">9999.00</C>
      <C POS="6">false</C>
    </L>
    <L POS="4">
      <C POS="1">REDUCTION_RATE</C>
      <C POS="2">Rabais</C>
      <C POS="3">1.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="5">
      <C POS="1">DISCOUNT_RATE</C>
      <C POS="2">Escompte</C>
      <C POS="3">2.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="6">
      <C POS="1">ROUNDING_AMOUNT</C>
      <C POS="2">Arrondi</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">5.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="7">
      <C POS="1">VAT_RATE</C>
      <C POS="2">TVA</C>
      <C POS="3">8.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="8">
      <C POS="1">TOTAL_NET</C>
      <C POS="2">Total net</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">9999.00</C>
      <C POS="6">false</C>
    </L>
  </FRACTION>
</CARACTERISTIQUE>
  
  val carIn = ElfinFormat caracteristiqueFromXml carInXml
  val carOut = OrderUtil computeOrderFigures carIn


  s"The number of carIn.FRACTION.L " should {
    s"equal 8" in {
     val nbLinesOpt = carIn.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     nbLinesOpt.get mustEqual 8
    }
  }
  
  s"The number of carOut.FRACTION.L " should {
    s"equal 8" in {
     val nbLinesOpt = carOut.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     nbLinesOpt.get mustEqual 8
    }
  }  
  
  s"Amounts of carIn.FRACTION.L " should {
    "equal 1050 for L POS='1' manual record " in {
      val lPos1 = carIn.FRACTION.map ( f => f.L(0)).get
      OrderUtil getLineAmount lPos1  mustEqual Some(1050)
    }
    "equal 94.75 for L POS='2' manual record " in {
      val lPos2 = carIn.FRACTION.map ( f => f.L(1)).get
      OrderUtil getLineAmount lPos2  mustEqual Some(94.75)
    }
    "equal 9999.00 for L POS='3' not yet computed gross total " in {
      val lPos3 = carIn.FRACTION.map ( f => f.L(2)).get
      OrderUtil getLineAmount lPos3  mustEqual Some(9999.00)
    }
    "equal 0.0 for L POS='4' not yet computed reduction amount " in {
      val lPos4 = carIn.FRACTION.map ( f => f.L(3)).get
      OrderUtil getLineAmount lPos4  mustEqual Some(0.0)
    }
    "equal 0.0 for L POS='5' not yet computed discount amount " in {
      val lPos5 = carIn.FRACTION.map ( f => f.L(4)).get
      OrderUtil getLineAmount lPos5  mustEqual Some(0.0)
    }
    "equal 5.00 for L POS='6' rounding amount " in {
      val lPos6 = carIn.FRACTION.map ( f => f.L(5)).get
      OrderUtil getLineAmount lPos6  mustEqual Some(5.00)
    }
    "equal 0.0 for L POS='7' not yet computed VAT amount " in {
      val lPos7 = carIn.FRACTION.map ( f => f.L(6)).get
      OrderUtil getLineAmount lPos7  mustEqual Some(0.0)
    }
    "equal 9999.0 for L POS='8' not yet computed net total " in {
      val lPos8 = carIn.FRACTION.map ( f => f.L(7)).get
      OrderUtil getLineAmount lPos8  mustEqual Some(9999.00)
    }
  }
  
  
  s"Amounts of carOut.FRACTION.L " should {
    "equal 1050 for L POS='1' manual record " in {
      val lPos1 = carOut.FRACTION.map ( f => f.L(0)).get
      OrderUtil getLineAmount lPos1  mustEqual Some(1050)
    }
    "equal 94.75 for L POS='2' manual record " in {
      val lPos2 = carOut.FRACTION.map ( f => f.L(1)).get
      OrderUtil getLineAmount lPos2  mustEqual Some(94.75)
    }
    "equal 1144.75 for L POS='3' computed gross total " in {
      val lPos3 = carOut.FRACTION.map ( f => f.L(2)).get
      OrderUtil getLineAmount lPos3  mustEqual Some(1144.75)
    }
    "equal 11.45 (1.0%) for L POS='4' computed reduction amount " in {
      val lPos4 = carOut.FRACTION.map ( f => f.L(3)).get
      OrderUtil getLineAmount lPos4  mustEqual Some(11.45)
    }
    "equal 22.90 (2.0%) for L POS='5' computed discount amount " in {
      val lPos5 = carOut.FRACTION.map ( f => f.L(4)).get
      OrderUtil getLineAmount lPos5  mustEqual Some(22.90)
    }
    "equal 5.00 for L POS='6' rounding amount " in {
      val lPos6 = carOut.FRACTION.map ( f => f.L(5)).get
      OrderUtil getLineAmount lPos6  mustEqual Some(5.00)
    }
    "equal 91.58 (8.0%) for L POS='7' computed VAT amount " in {
      val lPos7 = carOut.FRACTION.map ( f => f.L(6)).get
      OrderUtil getLineAmount lPos7  mustEqual Some(91.58)
    }
    "equal 1275.68 (rounded 1267.6725) for L POS='8' computed net total " in {
      val lPos8 = carOut.FRACTION.map ( f => f.L(7)).get
      OrderUtil getLineAmount lPos8  mustEqual Some(1275.68)
    }
  }  
  
  
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
      <C POS="1">REDUCTION_RATE</C>
      <C POS="2">Rabais</C>
      <C POS="3">1.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="3">
      <C POS="1">DISCOUNT_RATE</C>
      <C POS="2">Escompte</C>
      <C POS="3">2.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="4">
      <C POS="1">ROUNDING_AMOUNT</C>
      <C POS="2">Arrondi</C>
      <C POS="3"/>
      <C POS="4"/>
      <C POS="5">5.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="5">
      <C POS="1">VAT_RATE</C>
      <C POS="2">TVA</C>
      <C POS="3">8.0</C>
      <C POS="4">%</C>
      <C POS="5">0.00</C>
      <C POS="6">true</C>
    </L>
    <L POS="6">
      <C POS="1">TOTAL_NET</C>
      <C POS="2">Total net</C>
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
    s"equal 6" in {
     val nbLinesOpt = carInNoManualAmount.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     nbLinesOpt.get mustEqual 6
    }
  }
  
  s"The number of carOutNoManualAmount.FRACTION.L " should {
    s"equal 6" in {
     val nbLinesOpt = carOutNoManualAmount.FRACTION.map { fractionMat => 
       fractionMat.L.foldLeft(0)( (acc, l) => acc + 1) 
     } 
     nbLinesOpt.get mustEqual 6
    }
  }  
  
  s"Amounts of carInNoManualAmount.FRACTION.L " should {
    "equal 1050 for L POS='1' manual record " in {
      val lPos1 = carInNoManualAmount.FRACTION.map ( f => f.L(0)).get
      OrderUtil getLineAmount lPos1  mustEqual Some(1000)
    }
    "equal 94.75 for L POS='2' manual record " in {
      val lPos2 = carInNoManualAmount.FRACTION.map ( f => f.L(1)).get
      OrderUtil getLineAmount lPos2  mustEqual Some(0.0)
    }
    "equal 9999.00 for L POS='3' not yet computed gross total " in {
      val lPos3 = carInNoManualAmount.FRACTION.map ( f => f.L(2)).get
      OrderUtil getLineAmount lPos3  mustEqual Some(0.0)
    }
    "equal 0.0 for L POS='4' not yet computed reduction amount " in {
      val lPos4 = carInNoManualAmount.FRACTION.map ( f => f.L(3)).get
      OrderUtil getLineAmount lPos4  mustEqual Some(5.0)
    }
    "equal 0.0 for L POS='5' not yet computed discount amount " in {
      val lPos5 = carInNoManualAmount.FRACTION.map ( f => f.L(4)).get
      OrderUtil getLineAmount lPos5  mustEqual Some(0.0)
    }
    "equal 5.00 for L POS='6' rounding amount " in {
      val lPos6 = carInNoManualAmount.FRACTION.map ( f => f.L(5)).get
      OrderUtil getLineAmount lPos6  mustEqual Some(9999.0)
    }
  }
  
  
  s"Amounts of carOutNoManualAmount.FRACTION.L " should {
    "equal 1050 for L POS='1' manual record " in {
      val lPos1 = carOutNoManualAmount.FRACTION.map ( f => f.L(0)).get
      OrderUtil getLineAmount lPos1  mustEqual Some(1000)
    }
    "equal 94.75 for L POS='2' manual record " in {
      val lPos2 = carOutNoManualAmount.FRACTION.map ( f => f.L(1)).get
      OrderUtil getLineAmount lPos2  mustEqual Some(10.0)
    }
    "equal 1144.75 for L POS='3' computed gross total " in {
      val lPos3 = carOutNoManualAmount.FRACTION.map ( f => f.L(2)).get
      OrderUtil getLineAmount lPos3  mustEqual Some(20.0)
    }
    "equal 11.45 (1.0%) for L POS='4' computed reduction amount " in {
      val lPos4 = carOutNoManualAmount.FRACTION.map ( f => f.L(3)).get
      OrderUtil getLineAmount lPos4  mustEqual Some(5d)
    }
    "equal 22.90 (2.0%) for L POS='5' computed discount amount " in {
      val lPos5 = carOutNoManualAmount.FRACTION.map ( f => f.L(4)).get
      OrderUtil getLineAmount lPos5  mustEqual Some(80d)
    }
    "equal 5.00 for L POS='6' rounding amount " in {
      val lPos6 = carOutNoManualAmount.FRACTION.map ( f => f.L(5)).get
      OrderUtil getLineAmount lPos6  mustEqual Some(1115.0)
    }
  }      
      
}