package ch.bsisa.hyperbird.db.evolution

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import scala.concurrent.Future
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.dao.ElfinDAO

object Version4To5 {

  def fontainesStructUpdate() = {

    val fontainesCollectionId = "G20040930101030004"
    val xpath = "//ELFIN[@CLASSE='FONTAINE']"

    val query = WSQueries.filteredCollectionQuery(fontainesCollectionId, xpath)

    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url(query).get()

    // asynchronous call
    responseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      // Parse XML (Need to wrap the list of XML elements received to obtain valid XML.)
      val melfinElem = scala.xml.XML.loadString("<MELFIN>" + resp.body.mkString + "</MELFIN>")
      // elfinsFromXml unwraps ELFINS from the MELFIN element to return a Seq[ELFIN]
      // Unwrap wrap tag (should be MELFIN)
      val elfinNodeSeq = melfinElem \\ "ELFIN"

      // TODO: Perform conversion: copy all enforcing top elements order
      val elfins = for { elfinNode <- elfinNodeSeq } yield {
        /*
                <xs:element ref="MUTATIONS" minOccurs="0" maxOccurs="1"></xs:element>
                <xs:element ref="GEOSELECTION" minOccurs="0" maxOccurs="1"/>
                <xs:element ref="IDENTIFIANT" minOccurs="0" maxOccurs="1"/>
                <xs:element ref="CARACTERISTIQUE" minOccurs="0" maxOccurs="1"/>
                <xs:element ref="PARTENAIRE" minOccurs="0" maxOccurs="1"/>
                <xs:element ref="ACTIVITE" minOccurs="0" maxOccurs="1"/>
                <xs:element ref="FORME" minOccurs="0" maxOccurs="1"/>
                <xs:element ref="ANNEXE" minOccurs="0" maxOccurs="1"/>
                <xs:element ref="DIVERS" minOccurs="0" maxOccurs="1"/>
         */
        val newElfinNode =
          <ELFIN>
            { elfinNode \ "MUTATIONS" }
            { elfinNode \ "GEOSELECTION" }
            { elfinNode \ "IDENTIFIANT" }
            { elfinNode \ "CARACTERISTIQUE" }
            { elfinNode \ "PARTENAIRE" }
            { elfinNode \ "ACTIVITE" }
            { elfinNode \ "FORME" }
            { elfinNode \ "ANNEXE" }
            { elfinNode \ "DIVERS" }
          </ELFIN>.%(elfinNode.attributes)
        //val newElfinNode = <ELFIN Id={elfinNode \ "@Id"} ID_G={elfinNode \ "@ID_G"} CLASSE={elfinNode \ "@CLASSE"}></ELFIN>
        newElfinNode

      }

      Logger.debug(s"Found ${elfins.size} FONTAINES...")
      for (elfin <- elfins) {
        ElfinDAO.update(elfin)
        Logger.debug(s"elfin: ${elfin}")
      }
    }

  }

}