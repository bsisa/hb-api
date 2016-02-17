package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.report.ReportConfig

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import org.specs2.mutable._
import scala.xml.XML


/**
 * Tests ElfinUtilSpec
 *
 *  Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.util.ElfinUtilSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class ElfinUtilSpec extends BaseSerialisationSpec {

  val EXPECTED_HEADER_MESSAGE_VALUE = "TENANT COPY"
  val EXPECTED_CAR_VALUE_PAGE_ORIENTATION = ReportConfig.CAR_VALUE_PAGE_ORIENTATION_LANDSCAPE
  val EXPECTED_CAR_VALUE_PDF_INCLUDE_FIRST = """G20050101000012345/FLYING_SAUCER/20160216174700999"""
  val EXPECTED_CAR_VALUE_PDF_INCLUDE_LAST = """G20050101000012345/DOCUMENT/20160216174700999"""
  val EXPECTED_CAR_NOT_FOUND = "I DONT EXIST"
  
  // ================================================================== 
  // XML file => scala.xml.Elem
  // ==================================================================    
  val elfinReportTest001Xml = XML.loadFile(TestResourcesDir + "elfin-RAPPORT-001.xml")
  
  // ================================================================== 
  // scala.xml.Elem => ch.bsisa.hyperbird.model.ELFIN
  // ==================================================================
  val elfin = scalaxb.fromXML[ELFIN](elfinReportTest001Xml)
  
    
  s"ElfinUtil.getElfinCarByName(elfin, name)" should {
    s"return Some('${EXPECTED_HEADER_MESSAGE_VALUE}') for name : ReportConfig.CAR_NAME_HEADER_MESSAGE = ${ReportConfig.CAR_NAME_HEADER_MESSAGE}" in {
      ElfinUtil.getElfinCarByName(elfin,ReportConfig.CAR_NAME_HEADER_MESSAGE).flatMap(_.VALEUR) mustEqual Some(EXPECTED_HEADER_MESSAGE_VALUE)
    }
    s"return Some('${EXPECTED_CAR_VALUE_PAGE_ORIENTATION}') for name : ReportConfig.CAR_NAME_PAGE_ORIENTATION = ${ReportConfig.CAR_NAME_PAGE_ORIENTATION}" in {
      ElfinUtil.getElfinCarByName(elfin,ReportConfig.CAR_NAME_PAGE_ORIENTATION).flatMap(_.VALEUR) mustEqual Some(EXPECTED_CAR_VALUE_PAGE_ORIENTATION)
    }
    s"return Some('${EXPECTED_CAR_VALUE_PDF_INCLUDE_FIRST}') for name : ReportConfig.CAR_NAME_PDF_INCLUDE_FIRST = ${ReportConfig.CAR_NAME_PDF_INCLUDE_FIRST}" in {
      ElfinUtil.getElfinCarByName(elfin,ReportConfig.CAR_NAME_PDF_INCLUDE_FIRST).flatMap(_.VALEUR) mustEqual Some(EXPECTED_CAR_VALUE_PDF_INCLUDE_FIRST)
    }
    s"return Some('${EXPECTED_CAR_VALUE_PDF_INCLUDE_LAST}') for  name : ReportConfig.CAR_NAME_PDF_INCLUDE_LAST = ${ReportConfig.CAR_NAME_PDF_INCLUDE_LAST}" in {
      ElfinUtil.getElfinCarByName(elfin,ReportConfig.CAR_NAME_PDF_INCLUDE_LAST).flatMap(_.VALEUR) mustEqual Some(EXPECTED_CAR_VALUE_PDF_INCLUDE_LAST)
    }
    s"return None for name : EXPECTED_CAR_NOT_FOUND = ${EXPECTED_CAR_NOT_FOUND}" in {
      ElfinUtil.getElfinCarByName(elfin,EXPECTED_CAR_NOT_FOUND).flatMap(_.VALEUR) mustEqual None
    }
    
  }

}