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
 *
 * @author Patrick Refondini
 */
class CARACTERISTIQUESerialisation extends Specification {

  val TestResourcesDir = "./test/resources/"
  //val TestResultsDir = "./test/results/"
  val CARACTERISTIQUEJsonFilePath = TestResourcesDir + "CARACTERISTIQUE.json"

  val caracteristiqueJsonInput = JsonXmlConverter.loadJsonFromFile(CARACTERISTIQUEJsonFilePath)
  val caracteristiqueFromFile = caracteristiqueJsonInput.as[CARACTERISTIQUE]

  s"The CARACTERISTIQUEJsonFilePath at ${CARACTERISTIQUEJsonFilePath} " should {
    s"convert to ch.bsisa.hyperbird.model.CARACTERISTIQUE" in {
      caracteristiqueFromFile.CAR1.get.NOM.get must be equalTo ("Surface au sol")
      caracteristiqueFromFile.CAR1.get.UNITE.get must be equalTo ("m2")
      caracteristiqueFromFile.CAR1.get.VALEUR.get must be equalTo ("617.2340174963703")

      caracteristiqueFromFile.ETAT.get.ETAT1.get.B.get must be equalTo ("bEtat1Test")
      caracteristiqueFromFile.ETAT.get.ETAT1.get.C.get must be equalTo ("cEtat1Test")
      getMixedContent(caracteristiqueFromFile.ETAT.get.ETAT1.get.mixed) must be equalTo (
        "Etat1 TEst 1, chemin des|Tests 7,\n\t\t\t\tchemin\n\t\t\t\tdes|Tests 9, chemin des...\n\t\t\t")
    }
  }

}