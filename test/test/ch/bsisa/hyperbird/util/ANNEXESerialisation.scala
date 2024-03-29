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
 * Tests JSON ELFIN.ANNEXE serialisation deserialisation
 *
 * Tip: from sbt play console run: 
 * {{{
 * test-only test.ch.bsisa.hyperbird.util.ANNEXESerialisation
 * }}}
 * to have only the current test run. 
 *
 * @author Patrick Refondini
 */
class ANNEXESerialisation extends BaseSerialisationSpec {

  val ANNEXEJsonFilePath = TestResourcesDir + "ANNEXE.json"

  val annexeJsonInput = JsonXmlConverter.loadJsonFromFile(ANNEXEJsonFilePath)
  val annexeFromFile = annexeJsonInput.as[ANNEXE]

  s"The ANNEXEJsonFilePath at ${ANNEXEJsonFilePath} " should {
    s"convert to ch.bsisa.hyperbird.model.ANNEXE" in {
      annexeFromFile.RENVOI(0).POS must be equalTo (1)
      annexeFromFile.RENVOI(1).POS must be equalTo (2)
    }
  }

}