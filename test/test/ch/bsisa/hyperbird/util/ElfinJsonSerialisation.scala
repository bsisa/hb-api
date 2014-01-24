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
    "Id":"G20040203114894000",
    "MUTATIONS":{
      "UTILISATEUR":"",
      "MOT_DE_PASSE":"pbourquin",
      "ROLE":"",
      "DATE":"20081106105749992"
    }
  }
}"""

    
    val jsonValue = Json.parse(jsonString)
    
    import play.api.libs.json._
  import play.api.libs.functional.syntax._

    "The jsonString " should {
    "convert to Elfin object" in {
      val elfinJsValue = jsonValue \ "ELFIN"
      val elfin = elfinJsValue.as[Elfin]
      elfin.id must equalTo("G20040203114894000")
      elfin.classe must equalTo("IMMEUBLE")
      elfin.group must equalTo("G20040930101030005")
    }

  }
  
  
  
}