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
 * test-only test.ch.bsisa.hyperbird.util.GEOSELECTIONSerialisation
 * }}}
 * to have only the current test run.
 * 
 * @author Patrick Refondini
 */
class GEOSELECTIONSerialisation extends BaseSerialisationSpec {

  val GEOSELECTIONJsonFilePath = TestResourcesDir + "GEOSELECTION.json"

  val geoselectionJsonInput = JsonXmlConverter.loadJsonFromFile(GEOSELECTIONJsonFilePath)
  val geoselectionFromFile = geoselectionJsonInput.as[GEOSELECTION]

  s"The GEOSELECTIONJsonFilePath at ${GEOSELECTIONJsonFilePath} " should {
    s"convert to ch.bsisa.hyperbird.model.GEOSELECTION" in {
      geoselectionFromFile.CENTROIDE(0).TYPE.toString must be equalTo ("GEOGRAPHIE")
      geoselectionFromFile.CENTROIDE(1).TYPE.toString must be equalTo ("SCHEMATIQUE")
    }
  }

}