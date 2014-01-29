package ch.bsisa.hyperbird.model.format

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.model.SCHEMATIQUE
import ch.bsisa.hyperbird.model.GEOGRAPHIE
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scalaxb._
import java.io.StringWriter

/**
 * Exposes play.api.libs.json.Format (Reads and Writes) utilities to allow
 * {{{
 * ch.bsisa.hyperbird.model.MELFIN
 * ch.bsisa.hyperbird.model.ELFIN 
 * }}}
 * and subtree structures case classes seamless serialisation and
 * deserialisation to JSON format.
 *
 - First use ScalaJsonInceptions for shortest formatter definition
 - Second use ScalaJsonCombinators if simple customisation is needed
 - Fallback to Play plain Json API to deal with varargs constructors parameter
 *
 * 
 * @see [[http://www.playframework.com/documentation/2.2.x/ScalaJsonInception]]
 * @see [[http://www.playframework.com/documentation/2.2.x/ScalaJsonCombinators]]
 * @see [[http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.Reads$]]
 * @see [[http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.Writes$]]
 * @see [[http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.package]]
 *  
 *
 * @author Patrick Refondini
 */
object Implicits {

  // See: http://www.playframework.com/documentation/2.2.x/ScalaJsonInception
  // TODO: suppres Elfin entity together with corresponding test once ELFIN entity implemented.
  implicit val elfinReads: Reads[Elfin] = Json.reads[Elfin]

  // ==================================================================
  // ch.bsisa.hyperbird.model.MUTATIONS and sub tree
  // ==================================================================
  implicit val MUTATIONFormat: Format[MUTATION] = Json.format[MUTATION]

  implicit object MUTATIONSFormat extends Format[MUTATIONS] {
    def reads(json: JsValue): JsResult[MUTATIONS] =
      JsSuccess(MUTATIONS((json \ "MUTATION").as[List[MUTATION]]: _*))
    def writes(ms: MUTATIONS): JsValue = JsObject(
      for (m <- ms.MUTATION) yield "MUTATION" -> Json.toJson(m))
  }

  // ==================================================================
  // ch.bsisa.hyperbird.model.GEOSELECTION and sub tree
  // ==================================================================
  implicit object TYPETypeFormat extends Format[TYPEType] {

    def reads(json: JsValue): JsResult[TYPEType] =
      (json \ "TYPE") match {
        case JsString(value) => value match {
          case "SCHEMATIQUE" => JsSuccess(SCHEMATIQUE)
          case "GEOGRAPHIE" => JsSuccess(GEOGRAPHIE)
          case invalid => JsError(s"Invalid string value ${invalid} found for TYPE. Valid values are {SCHEMATIQUE,GEOGRAPHIE}")
        }
        case _ => JsError(s"Invalid JsValue type received for TYPE. Expecting JsString only.")
      }

    def writes(t: TYPEType): JsValue = t.toString match {
      case "SCHEMATIQUE" => JsString("SCHEMATIQUE")
      case "GEOGRAPHIE" => JsString("GEOGRAPHIE")
    }
  }

  implicit val CENTROIDEFormat: Format[CENTROIDE] = Json.format[CENTROIDE]

  implicit object GEOSELECTIONFormat extends Format[GEOSELECTION] {
    def reads(json: JsValue): JsResult[GEOSELECTION] =
      JsSuccess(GEOSELECTION((json \ "CENTROIDE").as[List[CENTROIDE]]: _*))
    def writes(gs: GEOSELECTION): JsValue = JsObject(
      for (c <- gs.CENTROIDE) yield "CENTROIDE" -> Json.toJson(c))
  }

  // ==================================================================
  // ch.bsisa.hyperbird.model.IDENTIFIANT and sub tree
  // ==================================================================  
  implicit val IDENTIFIANTFormat: Format[IDENTIFIANT] = Json.format[IDENTIFIANT]

  // ==================================================================
  // ch.bsisa.hyperbird.model.CARACTERISTIQUE and sub tree
  // ==================================================================  

  implicit object CARTypableFormat extends Format[CARTypable] {

    def reads(json: JsValue): JsResult[CARType] = {
      val nomUniteValeur: JsResult[(String, String, String)] = for {
        nom <- (json \ "NOM").validate[String]
        unite <- (json \ "UNITE").validate[String]
        valeur <- (json \ "VALEUR").validate[String]
      } yield (nom, unite, valeur)

      nomUniteValeur match {
        case JsSuccess((nom, unite, valeur), path) => JsSuccess(CARType(Option(nom), Option(unite), Option(valeur)))
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(c: CARTypable): JsValue = Json.obj(
      "NOM" -> c.NOM,
      "UNITE" -> c.UNITE,
      "VALEUR" -> c.VALEUR)
  }

  implicit object CARSET_CARTypeFormat extends Format[CARSET_CARType] {

    def reads(json: JsValue): JsResult[CARSET_CARType] = {

      val nomUniteValeurPos: JsResult[(String, String, String, Long)] = for {
        nom <- (json \ "NOM").validate[String]
        unite <- (json \ "UNITE").validate[String]
        valeur <- (json \ "VALEUR").validate[String]
        pos <- (json \ "POS").validate[Long] // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
      } yield (nom, unite, valeur, pos)

      nomUniteValeurPos match {
        case JsSuccess((nom, unite, valeur, pos), path) => JsSuccess(CARSET_CARType(Option(nom), Option(unite), Option(valeur), pos))
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(c: CARSET_CARType): JsValue = Json.obj(
      "NOM" -> c.NOM,
      "UNITE" -> c.UNITE,
      "VALEUR" -> c.VALEUR,
      "POS" -> c.POS.toLong // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
      )
  }

  implicit object CARSETFormat extends Format[CARSET] {
    def reads(json: JsValue): JsResult[CARSET] =
      JsSuccess(CARSET((json \ "CAR").as[List[CARSET_CARType]]: _*))
    def writes(cs: CARSET): JsValue = JsObject(
      for (c <- cs.CAR) yield "CAR" -> Json.toJson(c)(CARTypableFormat)) // Note: Force CARTypableFormat between it and CARSET_CARTypeFormat
  }

  // TODO: Review MIXED-CONTENT serialisation
  implicit object STATETypeFormat extends Format[STATEType] {

    def reads(json: JsValue): JsResult[STATEType] = {
      val bAndCAndContent: JsResult[(String, String, String)] = for {
        b <- (json \ "B").validate[String]
        c <- (json \ "C").validate[String]
        content <- (json \ "MIXED-CONTENT").validate[String]
      } yield (b, c, content)

      bAndCAndContent match {
        case JsSuccess((b, c, content), path) =>
          //val dataRecord = scalaxb.DataRecord[STATEType](None,None,None,None,content) //  TODO: to review
          val node = JsonXmlConverter.xmlStringToNodeSeq(content)
          val dataRecord = DataRecord.fromAny(node)
          val recordsSeq = Seq(dataRecord)
          val stateType = STATEType(recordsSeq, Option(b), Option(c))
          JsSuccess(stateType)
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(st: STATEType): JsValue = Json.obj(
      "B" -> st.B,
      "C" -> st.C,
      "MIXED-CONTENT" -> getMixedContent(st.mixed)) //TODO: GET MIXED CONTENT
  }

  //ETAT
  implicit val ETATFormat: Format[ETAT] = Json.format[ETAT]

  /**
   * case class C(mixed: Seq[scalaxb.DataRecord[Any]] = Nil,
   * POS: BigInt)
   */

  // TODO: Review MIXED-CONTENT serialisation
  implicit object CFormat extends Format[C] {

    def reads(json: JsValue): JsResult[C] = {
      val posContent: JsResult[(Long, String)] = for {
        pos <- (json \ "POS").validate[Long] // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
        content <- (json \ "MIXED-CONTENT").validate[String]
      } yield (pos, content)

      posContent match {
        case JsSuccess((pos, content), path) =>
          val node = JsonXmlConverter.xmlStringToNodeSeq(content)
          val dataRecord = DataRecord.fromAny(node)
          val recordsSeq = Seq(dataRecord)
          val column = C(recordsSeq, pos)
          JsSuccess(column)
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(c: C): JsValue = Json.obj(
      "POS" -> c.POS.toLong, // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
      "MIXED-CONTENT" -> getMixedContent(c.mixed)) //TODO: GET MIXED CONTENT
  }

  /**
   *   case class L(C: Seq[ch.bsisa.hyperbird.model.C] = Nil,
   * POS: BigInt)
   */
  //  implicit val LFormat: Reads[L] = (
  //    (__ \ "POS").read[Long] and
  //    (__ \ "C").read[C])(L)
  //

  implicit object LFormat extends Format[L] {

    def reads(json: JsValue): JsResult[L] = {
      val posCList: JsResult[(Long, Seq[C])] = for {
        pos <- (json \ "POS").validate[Long] // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
        cList <- (json \ "C").validate[Seq[C]]
      } yield (pos, cList)

      posCList match {
        case JsSuccess((pos, cList), path) => JsSuccess(L(cList, pos))
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(l: L): JsValue = {
      JsObject(
        Seq(
          ("POS" -> JsNumber(l.POS.toLong)), // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
          ("C" -> JsArray(
            for {
              c <- l.C
            } yield Json.toJson[C](c)))))
    }
  }

  // CALCUL => CALCULType (ok) => DIMENSION (ok) => NOM (ok) => TYPEType (ok)

  implicit object NOMFormat extends Format[NOM] {

    def reads(json: JsValue): JsResult[NOM] =
      (json \ "NOM") match {
        case JsString(value) => value match {
          case "LONGUEUR" => JsSuccess(LONGUEUR)
          case "SURFACE" => JsSuccess(SURFACE)
          case "RAYON" => JsSuccess(RAYON)
          case invalid => JsError(s"Invalid string value ${invalid} found for NOM. Valid values are {LONGUEUR,SURFACE,RAYON}")
        }
        case _ => JsError(s"Invalid JsValue type received for NOM. Expecting JsString only.")
      }

    def writes(n: NOM): JsValue = n.toString match {
      case "LONGUEUR" => JsString("LONGUEUR")
      case "SURFACE" => JsString("SURFACE")
      case "RAYON" => JsString("RAYON")
    }
  }

  // TODO: Review MIXED-CONTENT serialisation
  implicit object DIMENSIONFormat extends Format[DIMENSION] {

    def reads(json: JsValue): JsResult[DIMENSION] = {
      val nomTypeContent: JsResult[(NOM, TYPEType, String)] = for {
        nom <- (json \ "NOM").validate[NOM] // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
        typet <- (json \ "TYPE").validate[TYPEType]
        content <- (json \ "MIXED-CONTENT").validate[String]
      } yield (nom, typet, content)

      nomTypeContent match {
        case JsSuccess((nom, typet, content), path) =>
          val node = JsonXmlConverter.xmlStringToNodeSeq(content)
          val dataRecord = DataRecord.fromAny(node)
          val recordsSeq = Seq(dataRecord)
          val dimension = DIMENSION(recordsSeq, nom, typet)
          JsSuccess(dimension)
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(d: DIMENSION): JsValue = Json.obj(
      "NOM" -> Json.toJson[NOM](d.NOM),
      "TYPE" -> Json.toJson[TYPEType](d.TYPE),
      "MIXED-CONTENT" -> getMixedContent(d.mixed)) //TODO: GET MIXED CONTENT
  }

  // CALCULType
  implicit val CALCULTypeFormat: Format[CALCULType] = Json.format[CALCULType]

  // FRACTION => MATRICETypable (abtract trait) => MATRICEType (ok)
  implicit object MATRICETypableFormat extends Format[MATRICETypable] {
    def reads(json: JsValue): JsResult[MATRICEType] = {
      val lList = (json \ "L").as[List[L]]
      val mType = MATRICEType(lList: _*)
      JsSuccess(mType)
    }
    def writes(ls: MATRICETypable): JsValue = JsObject(
      for (l <- ls.L) yield "L" -> Json.toJson(l))
  }

  //    CARACTERISTIQUE
  implicit val CARACTERISTIQUEFormat: Format[CARACTERISTIQUE] = Json.format[CARACTERISTIQUE]


  /**
   * Extracts mixed content text nodes as String assuming there is no "real" mix content.
   *
   * TODO: validate the current assumption that geoXML.xsd
   * mixed content never really contains mix of text and
   * XML nodes element but only contains text nodes.
   * Then change the geoXML.xsd accordingly.
   * Regenerate Scalaxb model from new XSD.
   * Then make sure the current function calls can/must be suppressed.
   */
  def getMixedContent(mixed: Seq[scalaxb.DataRecord[Any]]): String = {

    // Get mixed content as as sequence of scala.xml.NodeSeq
    val nodeSeqSeq = for {
      record <- mixed
    } yield scalaxb.toXML(record, "MIXED-CONTENT", ch.bsisa.hyperbird.model.proto.defaultScope)

    // Output sequence of scala.xml.NodeSeq to String
    val sw = new StringWriter()
    import scala.xml.XML
    for (nodeSeq <- nodeSeqSeq) {
      for (node <- nodeSeq) {
        //XML.write(sw, node, "", false, null)
        sw.write(node.text)
      }
    }
    sw.toString
  }

  ///////////////////////////////////////////////////////////////////////////

}