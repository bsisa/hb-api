package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import org.specs2.mutable._
import scala.xml.XML
import play.api.libs.json.Json
import play.api.libs.json._

import java.io.File
import scala.io.Source

/**
 *
 * Important:
 *
 * CFC codes cannot be padded with 0. These are not numbers.
 *
 * For instance:
 * 0, 00, 000 are three valid and different codes
 * 9, 009 are two valid and completely different codes...
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.util.CfcConvert
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class CfcConvert extends BaseSerialisationSpec {

  val cfcFilePath = TestResourcesDir + "cfc.csv"

  var i = 0;

  val csvCfcInputFile = new File(cfcFilePath)
  val XmlOutputFilePrefix = "CODE"
  val XmlOutputFileSuffix = ".xml"

  Source.fromFile(csvCfcInputFile, "UTF8").getLines.foreach { line =>

    i = i + 1

    val tokens: Array[String] = line.split(" ")

    val code: String = tokens(0)
    val formattedIndex = "%03d".format(i)
    val id: String = "G20150915030000" + formattedIndex
    val comment: String = tokens.drop(1).toList.mkString(" ")

    val cfcCodeElfin = <ELFIN ID_G="G20150910130000002" Id={ id } CLASSE="CODE" GROUPE="CFC" NATURE="Classification" TYPE="ACTIVITE" SOURCE="">
                         <IDENTIFIANT>
                           <AUT>PRE</AUT><!-- Input by (automatic) -->
                           <NOM>{ code }</NOM><!-- Alphanumeric code -->
                         </IDENTIFIANT>
                         <CARACTERISTIQUE><CAR1 NOM="Position de tri" VALEUR={ formattedIndex }/></CARACTERISTIQUE>
                         <PARTENAIRE/>
                         <ACTIVITE/>
                         <DIVERS>
                           <REMARQUE>{ comment }</REMARQUE><!-- Description -->
                         </DIVERS>
                       </ELFIN>

    val xmlCfcOutputFilePath = TestResultsDir + XmlOutputFilePrefix + id + XmlOutputFileSuffix

    //println(cfcCodeElfin)
    XML.save(xmlCfcOutputFilePath, cfcCodeElfin, "UTF8", false)
  }

  s"The conversion of CSV input file ${csvCfcInputFile.getCanonicalFile} " should {
    s"create 572 XML files" in {
      val xmlFiles = new File(TestResultsDir)
      val matchingXmlFiles = xmlFiles.listFiles.filter(file => (!file.isDirectory && file.getName.startsWith(XmlOutputFilePrefix) && file.getName.endsWith(XmlOutputFileSuffix)))
      matchingXmlFiles.size must equalTo(572)
    }
  }

}