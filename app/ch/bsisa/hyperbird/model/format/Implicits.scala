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
 * - First use ScalaJsonInceptions for shortest formatter definition
 * - Second use ScalaJsonCombinators if simple customisation is needed
 * - Fallback to Play plain Json API to deal with varargs constructors parameter
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

  /**
   * Extracts mixed content text nodes as String assuming there is no "real" mix content,
   * that is no mix of XML elements and text nodes but only text nodes.
   *
   * TODO: validate the current assumption that geoXML.xsd
   * mixed content never really contains mix of text and
   * XML nodes element but only contains text nodes.
   * Then change the geoXML.xsd accordingly.
   * Regenerate Scalaxb model from new XSD.
   * Then make sure the current function calls can/must be suppressed.
   */
  def getMixedContent(mixed: Seq[scalaxb.DataRecord[Any]]): String = {

    // Get mixed content as as Seq of scala.xml.NodeSeq
    val nodeSeqSeq = for {
      record <- mixed
    } yield scalaxb.toXML(record, "TEMP-ELEMENT", ch.bsisa.hyperbird.model.proto.defaultScope)

    // Extract text content from the sequence of node sequence.
    val textSeq = for {
      nodeSeq <- nodeSeqSeq
      node <- nodeSeq
    } yield node.text

    //Return the content as a String
    textSeq.mkString
  }

  /**
   * Generic BigInt formatter. Considers that no BigInt will exceed Long as used in geoXml.xsd
   *
   * WARNING: Assumes BigInt conversion only applies to POS attributes which is currently true for geoXml.xsd
   * @todo: Review geoXml.xsd xs:positiveInteger XSD definition leads to BigInt. Complicates conversions. Check how to have
   * Long instead for simplification.
   */
  implicit object BigIntFormat extends Format[BigInt] {
    def reads(json: JsValue): JsResult[BigInt] = (json).validate[Long] match {
      case JsSuccess(pos, path) => JsSuccess(BigInt.long2bigInt(pos))
      case JsError(e) => JsError(e)
    }
    def writes(b: BigInt): JsValue = JsNumber(b.toLong)
  }

  // ==================================================================
  // ch.bsisa.hyperbird.model.MUTATIONS and sub tree
  // ==================================================================
  implicit val MUTATIONFormat: Format[MUTATION] = Json.format[MUTATION]

  implicit object MUTATIONSFormat extends Format[MUTATIONS] {
    def reads(json: JsValue): JsResult[MUTATIONS] = {
      val jsResult = (json \ "MUTATION").validate[List[MUTATION]]
      jsResult match {
        case JsSuccess(mutations, path) => JsSuccess(MUTATIONS(mutations: _*))
        case JsError(e) => JsError(e) // Simply forward
      }
    }
    def writes(ms: MUTATIONS): JsValue = {
      val mutationJsSeq = for (m <- ms.MUTATION) yield Json.toJson(m)
      Json.obj("MUTATION" -> JsArray(mutationJsSeq))
    }
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
    def writes(gs: GEOSELECTION): JsValue = {
      val centroideJsSeq = for (c <- gs.CENTROIDE) yield Json.toJson(c)
      Json.obj("CENTROIDE" -> JsArray(centroideJsSeq))
    }
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

  // BigInt formatter allows JSON inception.
  implicit val CARSET_CARTypeFormat: Format[CARSET_CARType] = Json.format[CARSET_CARType]

  //  implicit object CARSET_CARTypeFormat extends Format[CARSET_CARType] {
  //
  //    def reads(json: JsValue): JsResult[CARSET_CARType] = {
  //
  //      val nomUniteValeurPos: JsResult[(String, String, String, Long)] = for {
  //        nom <- (json \ "NOM").validate[String]
  //        unite <- (json \ "UNITE").validate[String]
  //        valeur <- (json \ "VALEUR").validate[String]
  //        pos <- (json \ "POS").validate[Long] // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
  //      } yield (nom, unite, valeur, pos)
  //
  //      nomUniteValeurPos match {
  //        case JsSuccess((nom, unite, valeur, pos), path) => JsSuccess(CARSET_CARType(Option(nom), Option(unite), Option(valeur), pos))
  //        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
  //      }
  //    }
  //
  //    def writes(c: CARSET_CARType): JsValue = Json.obj(
  //      "NOM" -> c.NOM,
  //      "UNITE" -> c.UNITE,
  //      "VALEUR" -> c.VALEUR,
  //      "POS" -> c.POS.toLong // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
  //      )
  //  }

  implicit object CARSETFormat extends Format[CARSET] {
    def reads(json: JsValue): JsResult[CARSET] =
      JsSuccess(CARSET((json \ "CAR").as[List[CARSET_CARType]]: _*))
    def writes(cs: CARSET): JsValue = {
      // Note: Force CARTypableFormat between it and CARSET_CARTypeFormat
      val carJsSeq = for (c <- cs.CAR) yield Json.toJson(c)(CARTypableFormat)
      Json.obj("CAR" -> JsArray(carJsSeq))
    }
  }

  implicit object STATETypeFormat extends Format[STATEType] {

    def reads(json: JsValue): JsResult[STATEType] = {
      val bAndCAndContent: JsResult[(String, String, String)] = for {
        b <- (json \ "B").validate[String]
        c <- (json \ "C").validate[String]
        content <- (json \ "MIXED-CONTENT").validate[String]
      } yield (b, c, content)

      bAndCAndContent match {
        case JsSuccess((b, c, content), path) =>
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
      "MIXED-CONTENT" -> getMixedContent(st.mixed))
  }

  //ETAT
  implicit val ETATFormat: Format[ETAT] = Json.format[ETAT]

  implicit object CFormat extends Format[C] {

    def reads(json: JsValue): JsResult[C] = {
      val posContent: JsResult[(BigInt, String)] = for {
        pos <- (json \ "POS").validate[BigInt]
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
      "POS" -> c.POS,
      "MIXED-CONTENT" -> getMixedContent(c.mixed))
  }

  implicit val LFormat: Format[L] = Json.format[L]

  //  implicit object LFormat extends Format[L] {
  //
  //    def reads(json: JsValue): JsResult[L] = {
  //      val posCList: JsResult[(Long, Seq[C])] = for {
  //        pos <- (json \ "POS").validate[Long] // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
  //        cList <- (json \ "C").validate[Seq[C]]
  //      } yield (pos, cList)
  //
  //      posCList match {
  //        case JsSuccess((pos, cList), path) => JsSuccess(L(cList, pos))
  //        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
  //      }
  //    }
  //
  //    def writes(l: L): JsValue = {
  //      JsObject(
  //        Seq(
  //          ("POS" -> JsNumber(l.POS.toLong)), // TODO: xs:positiveInteger XSD definition leads to BigInt. This seems not wise, to check. Using Long for simplification.
  //          ("C" -> JsArray(
  //            for {
  //              c <- l.C
  //            } yield Json.toJson[C](c)))))
  //    }
  //  }

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
      "MIXED-CONTENT" -> getMixedContent(d.mixed))
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
    def writes(ls: MATRICETypable): JsValue = {
      val lineJsSeq = for (l <- ls.L) yield Json.toJson(l)
      Json.obj("L" -> JsArray(lineJsSeq))
    }
  }

  //    CARACTERISTIQUE
  implicit val CARACTERISTIQUEFormat: Format[CARACTERISTIQUE] = Json.format[CARACTERISTIQUE]

  // PARTENAIRE => {GERANT,USAGER,FOURNISSEUR,PROPRIETAIRE} => PERSONNEType
  implicit object PERSONNETypeFormat extends Format[PERSONNEType] {

    def reads(json: JsValue): JsResult[PERSONNEType] = {
      val idIdgNomGroupeContent: JsResult[(String, String, String, String, String)] = for {
        id <- (json \ "Id").validate[String]
        idg <- (json \ "ID_G").validate[String]
        nom <- (json \ "NOM").validate[String]
        groupe <- (json \ "GROUPE").validate[String]
        content <- (json \ "MIXED-CONTENT").validate[String]
      } yield (id, idg, nom, groupe, content)

      idIdgNomGroupeContent match {
        case JsSuccess((id, idg, nom, groupe, content), path) =>
          val node = JsonXmlConverter.xmlStringToNodeSeq(content)
          val dataRecord = DataRecord.fromAny(node)
          val recordsSeq = Seq(dataRecord)
          val personne = PERSONNEType(recordsSeq, Option(id), Option(idg), Option(nom), Option(groupe))
          JsSuccess(personne)
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(p: PERSONNEType): JsValue = Json.obj(
      "Id" -> JsString(p.Id.getOrElse("null")),
      "ID_G" -> JsString(p.ID_G.getOrElse("null")),
      "NOM" -> JsString(p.NOM.getOrElse("null")),
      "GROUPE" -> JsString(p.GROUPE.getOrElse("null")),
      "MIXED-CONTENT" -> getMixedContent(p.mixed)) //TODO: GET MIXED CONTENT
  }

  // PARTENAIRE => {GERANT,USAGER,FOURNISSEUR,PROPRIETAIRE} => PERSONNEType
  implicit val PARTENAIREFormat: Format[PARTENAIRE] = Json.format[PARTENAIRE]

  // ACTIVITE => EVENEMENT => ECHEANCE => E_STATUT  GESTION => MATRICETypable
  implicit object E_STATUTFormat extends Format[E_STATUT] {

    def reads(json: JsValue): JsResult[E_STATUT] =
      (json \ "E_STATUT") match {
        case JsString(value) => value match {
          case "OK" => JsSuccess(OK)
          case "A EFFECTUER" => JsSuccess(AEFFECTUER)
          case invalid => JsError(s"Invalid string value ${invalid} found for E_STATUT. Valid values are {OK,A EFFECTUER}")
        }
        case _ => JsError(s"Invalid JsValue type received for E_STATUT. Expecting JsString only.")
      }

    def writes(e: E_STATUT): JsValue = e.toString match {
      case "OK" => JsString("OK")
      case "A EFFECTUER" => JsString("A EFFECTUER")
    }
  }

  // ECHEANCE => E_STATUT  GESTION => MATRICETypable
  implicit val ECHEANCEFormat: Format[ECHEANCE] = Json.format[ECHEANCE]

  // EVENEMENT => ECHEANCE => E_STATUT  GESTION => MATRICETypable
  implicit object EVENEMENTFormat extends Format[EVENEMENT] {
    def reads(json: JsValue): JsResult[EVENEMENT] =
      JsSuccess(EVENEMENT((json \ "ECHEANCE").as[List[ECHEANCE]]: _*))
    def writes(es: EVENEMENT): JsValue = {
      val echeanceJsSeq = for (e <- es.ECHEANCE) yield Json.toJson(e)
      Json.obj("ECHEANCE" -> JsArray(echeanceJsSeq))
    }
  }

  // ACTIVITE => EVENEMENT => ECHEANCE => E_STATUT  GESTION => MATRICETypable
  implicit val ACTIVITEFormat: Format[ACTIVITE] = Json.format[ACTIVITE]

  // FORME => POINT => FONCTION (LIBRE|LIE|BASE)
  //       => LIGNE => PASSAGE => (PIECE | GUIDE | DIRECTION | FONCTIONType2)
  //                => GUIDE => (PIECE)
  //       => ZONE => LIGNE
  //       => SYMBOLE => LIBELLE
  implicit object FONCTIONFormat extends Format[FONCTION] {

    def reads(json: JsValue): JsResult[FONCTION] =
      (json \ "FONCTION") match {
        case JsString(value) => value match {
          case "LIBRE" => JsSuccess(LIBRE)
          case "LIE" => JsSuccess(LIE)
          case "BASE" => JsSuccess(BASE)
          case invalid => JsError(s"Invalid string value ${invalid} found for FONCTION. Valid values are {LIBRE,LIE,BASE}")
        }
        case _ => JsError(s"Invalid JsValue type received for FONCTION. Expecting JsString only.")
      }

    def writes(f: FONCTION): JsValue = f.toString match {
      case "LIBRE" => JsString("LIBRE")
      case "LIE" => JsString("LIE")
      case "BASE" => JsString("BASE")
    }
  }

  implicit val POINTFormat: Format[POINT] = Json.format[POINT]

  implicit val PIECEFormat: Format[PIECE] = Json.format[PIECE]

  implicit object FONCTIONTypeFormat extends Format[FONCTIONType] {

    def reads(json: JsValue): JsResult[FONCTIONType] =
      (json \ "FONCTION") match {
        case JsString(value) => value match {
          case "DEBUT" => JsSuccess(DEBUT)
          case "MILIEU" => JsSuccess(MILIEU)
          case "FIN" => JsSuccess(FIN)
          case "PASSAGE" => JsSuccess(PASSAGEValue)
          case "ACTUATEUR" => JsSuccess(ACTUATEUR)
          case "DEPART" => JsSuccess(DEPART)
          case invalid => JsError(s"Invalid string value ${invalid} found for FONCTION. Valid values are {DEBUT,MILIEU,FIN,PASSAGE,ACTUATEUR,DEPART}")
        }
        case _ => JsError(s"Invalid JsValue type received for FONCTION. Expecting JsString only.")
      }

    def writes(f: FONCTIONType): JsValue = f.toString match {
      case "DEBUT" => JsString("DEBUT")
      case "MILIEU" => JsString("MILIEU")
      case "FIN" => JsString("FIN")
      case "PASSAGE" => JsString("PASSAGE")
      case "ACTUATEUR" => JsString("ACTUATEUR")
      case "DEPART" => JsString("DEPART")
    }
  }

  implicit val PASSAGEFormat: Format[PASSAGE] = Json.format[PASSAGE]
  implicit val GUIDEFormat: Format[GUIDE] = Json.format[GUIDE]

  implicit object DIRECTIONFormat extends Format[DIRECTION] {

    def reads(json: JsValue): JsResult[DIRECTION] =
      (json \ "DIRECTION") match {
        case JsString(value) => value match {
          case "AVAL" => JsSuccess(AVAL)
          case "AMONT" => JsSuccess(AMONT)
          case invalid => JsError(s"Invalid string value ${invalid} found for DIRECTION. Valid values are {AVAL,AMONT}")
        }
        case _ => JsError(s"Invalid JsValue type received for DIRECTION. Expecting JsString only.")
      }

    def writes(f: DIRECTION): JsValue = f.toString match {
      case "AVAL" => JsString("AVAL")
      case "AMONT" => JsString("AMONT")
    }
  }

  //  
  implicit object FONCTIONType2Format extends Format[FONCTIONType2] {

    def reads(json: JsValue): JsResult[FONCTIONType2] =
      (json \ "FONCTION") match {
        case JsString(value) => value match {
          case "FRONTIERE" => JsSuccess(FRONTIERE)
          case "AXE" => JsSuccess(AXE)
          case invalid => JsError(s"Invalid string value ${invalid} found for FONCTION. Valid values are {FRONTIERE,AXE}")
        }
        case _ => JsError(s"Invalid JsValue type received for FONCTION. Expecting JsString only.")
      }

    def writes(f: FONCTIONType2): JsValue = f.toString match {
      case "FRONTIERE" => JsString("FRONTIERE")
      case "AXE" => JsString("AXE")
    }
  }

  implicit val LIGNEFormat: Format[LIGNE] = Json.format[LIGNE]
  implicit val LIBELLEFormat: Format[LIBELLE] = Json.format[LIBELLE]
  implicit val SYMBOLEFormat: Format[SYMBOLE] = Json.format[SYMBOLE]
  implicit val ZONEFormat: Format[ZONE] = Json.format[ZONE]

  implicit val FORMEFormat: Format[FORME] = Json.format[FORME]

  // ANNEXE => RENVOI => LIEN
  implicit object URIFormat extends Format[java.net.URI] {
    def reads(json: JsValue): JsResult[java.net.URI] = (json).validate[String] match {
      case JsSuccess(lien, path) => JsSuccess(new java.net.URI(lien))
      case JsError(e) => JsError(e)
    }
    def writes(l: java.net.URI): JsValue = JsString(l.toASCIIString) //use l.toASCIIString if encoding to ASCII is required, l.toString otherwise
  }

  implicit object RENVOIFormat extends Format[RENVOI] {

    def reads(json: JsValue): JsResult[RENVOI] = {
      val posLienContent: JsResult[(BigInt, java.net.URI, String)] = for {
        pos <- (json \ "POS").validate[BigInt]
        lien <- (json \ "LIEN").validate[java.net.URI]
        content <- (json \ "MIXED-CONTENT").validate[String]
      } yield (pos, lien, content)

      posLienContent match {
        case JsSuccess((pos, lien, content), path) =>
          val node: scala.xml.NodeSeq = scala.xml.Text(content)
          val dataRecord = DataRecord.fromAny(node)
          val recordsSeq = Seq(dataRecord)
          JsSuccess(RENVOI(recordsSeq, pos, lien))
        case JsError(errors) => JsError(errors) // forward JsError(errors) "as is"
      }
    }

    def writes(r: RENVOI): JsValue = Json.obj(
      "POS" -> r.POS,
      "LIEN" -> r.LIEN,
      "MIXED-CONTENT" -> getMixedContent(r.mixed))
  }

  // ANNEXE => RENVOI => LIEN
  implicit object ANNEXEFormat extends Format[ANNEXE] {
    def reads(json: JsValue): JsResult[ANNEXE] = {
      val jsResult = (json \ "RENVOI").validate[List[RENVOI]]
      jsResult match {
        case JsSuccess(renvois, path) => JsSuccess(ANNEXE(renvois: _*))
        case JsError(e) => JsError(e) // Simply forward 
      }
    }
    def writes(annexes: ANNEXE): JsValue = {
      val renvoiArr = for (r <- annexes.RENVOI) yield Json.toJson(r)
      Json.obj("RENVOI" -> JsArray(renvoiArr))
    }
  }

  // DIVERS
  implicit val DIVERSFormat: Format[DIVERS] = Json.format[DIVERS]

  implicit object TYPEFormat extends Format[TYPE] {

    def reads(json: JsValue): JsResult[TYPE] =
      (json \ "TYPE") match {
        case JsString(value) => value match {
          case "BIEN" => JsSuccess(BIEN)
          case "ACTIVITE" => JsSuccess(ACTIVITEValue)
          case "PERSONNE" => JsSuccess(PERSONNE)
          case "DOCUMENT" => JsSuccess(DOCUMENT)
          case invalid => JsError(s"Invalid string value ${invalid} found for TYPE. Valid values are {BIEN,ACTIVITE,PERSONNE,DOCUMENT}")
        }
        case _ => JsError(s"Invalid JsValue type received for TYPE. Expecting JsString only.")
      }

    def writes(t: TYPE): JsValue = JsString(t.toString)

  }

  implicit val ELFINFormat: Format[ELFIN] = Json.format[ELFIN]

  ///////////////////////////////////////////////////////////////////////////

}