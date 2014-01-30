package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import org.specs2.mutable._
import scala.xml.XML
import play.api.libs.json.Json
import play.api.libs.json._

/**
 * Tests JSON ELFIN.ANNEXE.RENVOI serialisation deserialisation
 *
 * Tip: from play console use commmand `test-only test.ch.bsisa.hyperbird.util.RENVOISerialisation`
 * to run this test only.
 *
 * @author Patrick Refondini
 */
class RENVOISerialisation extends Specification {

  val TestResourcesDir = "./test/resources/"
  val TestResultsDir = "./test/results/"
  val RENVOIJsonFilePath = TestResourcesDir + "RENVOI.json"

  val renvoiJsonInput = JsonXmlConverter.loadJsonFromFile(RENVOIJsonFilePath)
  val renvoiFromFile = renvoiJsonInput.as[RENVOI]

  s"The RENVOIJsonFilePath at ${RENVOIJsonFilePath} converted to ch.bsisa.hyperbird.model.RENVOI" should {
    s"have POS attribute equal to 1" in {
      renvoiFromFile.POS must be equalTo (1)
    }
    s"""have LIEN attribute equal to "../../HB-Objets/G20040930101030005/annexes/G20040931234567890/Ecole_Le_Corbusier_Contrat.pdf"""" in {
      renvoiFromFile.LIEN.toString must be equalTo ("../../HB-Objets/G20040930101030005/annexes/G20040931234567890/Ecole_Le_Corbusier_Contrat.pdf")
    }
    s"""have MIXED-CONTENT attribute equal to "Ecole_Le_Corbusier_Contrat.pdf"""" in {
      getMixedContent(renvoiFromFile.mixed) must be equalTo ("Ecole_Le_Corbusier_Contrat.pdf")
    }

  }

}