package ch.bsisa.hyperbird.documents

import java.io.StringReader

import org.apache.poi.xwpf.usermodel._

import scala.tools.nsc.interpreter.InputStream
import scala.xml.{Elem, XML}

object WordDocumentBuilder {

  def getDocument(stream: InputStream): XWPFDocument = new XWPFDocument(stream)

  def getXQueryFileName(doc: XWPFDocument): String =
    doc.getProperties.getCustomProperties.getProperty("GespatriQuery").getLpwstr

  def mergeData(doc: XWPFDocument, data: String): XWPFDocument = {
    val xmlData = XML.load(new StringReader(data))

    val dataMap = Map(xmlData.child.collect { case n: Elem => n }.map { e => e.label -> e.text }: _*)

    for ((key, value) <- dataMap) {

      val property = doc.getProperties.getCustomProperties.getProperty(s"Gespatri_$key")
      if (property != null) {
        property.setLpwstr(value)
      }
    }

    doc.enforceUpdateFields()

    doc
  }
}