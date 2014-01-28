package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.Elfin

import org.specs2.mutable._

import play.api.libs.json.Json

/**
 * Tests serialisation deserialisation of Elfin object to and from JSON format.
 *
 * @author Patrick Refondini
 */
class ElfinJsonSerialisation extends Specification {

  val jsonString = """
{
"ELFIN":{
    "NATURE":"Patrimoine",
    "TYPE":"BIEN",
    "GROUPE":"",
    "CLASSE":"IMMEUBLE",
    "ID_G":"G20040930101030005",
    "Id":"G20040931234567890",
    "MUTATIONS":{
      "UTILISATEUR":"",
      "MOT_DE_PASSE":"pbourquin",
      "ROLE":"",
      "DATE":"20081106105749992"
    }
  }
}"""

  val jsonValue = Json.parse(jsonString)

  "The jsonString " should {
    "convert to Elfin object" in {
      val elfinJsValue = jsonValue \ "ELFIN"
      val elfin = elfinJsValue.as[Elfin]
      elfin.Id must equalTo("G20040931234567890")
      elfin.CLASSE must equalTo("IMMEUBLE")
      elfin.ID_G must equalTo("G20040930101030005")
    }

  }

}