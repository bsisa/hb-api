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
 * Tests JSON ELFIN serialisation deserialisation
 *
 * Tip: from sbt play console run: 
 * {{{
 * test-only test.ch.bsisa.hyperbird.util.ELFINSerialisation
 * }}}
 * to have only the current test run.  
 * 
 * @author Patrick Refondini
 */
class ELFINSerialisation extends BaseSerialisationSpec {

  val ELFINJsonFilePath = TestResourcesDir + "ELFIN.json"

  val expectedELFIN_Id = "G20040931234567890"
  val expectedELFIN_ID_G = "G20040930101030005"

  val elfinJsonInput = JsonXmlConverter.loadJsonFromFile(ELFINJsonFilePath)
  val elfinFromFile = elfinJsonInput.as[ELFIN]

  s"The ELFINJsonFilePath at ${ELFINJsonFilePath} converted to ch.bsisa.hyperbird.model.ELFIN " should {
    s"have ELFIN.Id equal to ${expectedELFIN_Id}" in {
      elfinFromFile.Id must be equalTo (expectedELFIN_Id)
    }
    s"have ELFIN.ID_G equal to ${expectedELFIN_ID_G}" in {
      elfinFromFile.ID_G must be equalTo (expectedELFIN_ID_G)
    }
  }

}