package ch.bsisa.hyperbird.model.format

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.model.SCHEMATIQUE
import ch.bsisa.hyperbird.model.GEOGRAPHIE
import ch.bsisa.hyperbird.util.format.JsonXmlConverter

import play.api.Logger
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

  /**
   * Constant holding JSON property name used to store geoXml.xsd elements mixed-content.
   *
   * A better but more involved solution would be to modify the geoXml.xsd not to use mixed
   * content anymore. In all existing situations there is no good reason to use mixed content
   * and it makes developers life more difficult.
   */
  val MixedContentJsonPropName = "VALUE"

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
   * Creates a Seq[scalaxb.DataRecord[Any]] structure from provided `content parameter.
   * This type of data structure is necessary to manage mixed content with Scalaxb.
   */
  def setMixedContent(content: String): Seq[scalaxb.DataRecord[Any]] = {
    Seq(DataRecord.fromAny(scala.xml.Text(content)))
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
      case JsSuccess(pos, path) =>
        try {
          JsSuccess(BigInt.long2bigInt(pos))
        } catch {
          case nfe: java.lang.NumberFormatException =>
            JsError(s"Error reading BigInt from JSON. Could not parse invalid value at path: ${path}")
          case e: Throwable => JsError(s"Error reading BigInt: ${e.getMessage()}")
        }
      case JsError(e) => JsError("Error reading BigInt") ++ JsError(e)
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
        case JsSuccess(mutationList, path) => JsSuccess(MUTATIONS(mutationList))
        case JsError(e)                    => JsError("Error reading MUTATIONS") ++ JsError(e)
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
      json match {
        case JsString(value) => value match {
          case "SCHEMATIQUE" => JsSuccess(SCHEMATIQUE)
          case "GEOGRAPHIE"  => JsSuccess(GEOGRAPHIE)
          case invalid       => JsError(s"Invalid string value ${invalid} found for TYPE. Valid values are {SCHEMATIQUE,GEOGRAPHIE}")
        }
        case _ => JsError(s"Invalid JsValue type received for TYPE. Expecting JsString only.")
      }

    def writes(t: TYPEType): JsValue = t.toString match {
      case "SCHEMATIQUE" => JsString("SCHEMATIQUE")
      case "GEOGRAPHIE"  => JsString("GEOGRAPHIE")
    }
  }

  implicit val CENTROIDEFormat: Format[CENTROIDE] = Json.format[CENTROIDE]

  implicit object GEOSELECTIONFormat extends Format[GEOSELECTION] {

    def reads(json: JsValue): JsResult[GEOSELECTION] = {
      val centroideJsSeq = (json \ "CENTROIDE").validate[List[CENTROIDE]]
      centroideJsSeq match {
        case JsSuccess(centroideList, path) => JsSuccess(GEOSELECTION(centroideList))
        case JsError(e)                     => JsError("Error reading GEOSELECTION") ++ JsError(e)
      }
    }

    def writes(gs: GEOSELECTION): JsValue = {
      val centroideJsSeq = for (c <- gs.CENTROIDE) yield Json.toJson(c)
      Json.obj("CENTROIDE" -> JsArray(centroideJsSeq))
    }
  }

  // ==================================================================
  // ch.bsisa.hyperbird.model.IDENTIFIANT and sub tree
  // ==================================================================  

  // One liner works fine if no model modification is required
  //implicit val IDENTIFIANTFormat: Format[IDENTIFIANT] = Json.format[IDENTIFIANT]

  // Custom Reads/Writes might offer more fine grain control over complex validation
  //  implicit val IDENTIFIANTReads: Reads[IDENTIFIANT] = (
  //    (JsPath \ "AUT").readNullable[String] and
  //    (JsPath \ "GER").readNullable[String] and
  //    (JsPath \ "RES").readNullable[String] and
  //    (JsPath \ "NOM").readNullable[String] and
  //    (JsPath \ "ALIAS").readNullable[String] and
  //    (JsPath \ "ORIGINE").readNullable[String] and
  //    (JsPath \ "OBJECTIF").readNullable[String] and
  //    (JsPath \ "QUALITE").readNullable[String] and
  //    (JsPath \ "COMPTE").readNullable[String] and
  //    (JsPath \ "DE").readNullable[String] and
  //    (JsPath \ "A").readNullable[String] and
  //    (JsPath \ "PAR").readNullable[String] and
  //    (JsPath \ "VALEUR_A_NEUF").readNullable[Double] and
  //    (JsPath \ "VALEUR").readNullable[Double] and
  //    (JsPath \ "MOTCLE").read[Seq[String]])(IDENTIFIANT.apply _)

  /**
   * This object is not part of the XML data model but here to have
   * JSON object array instead of JSON primitive array:
   *
   * Objects array:
   * "MOTCLE": [{ "VALUE": "FIRST" }, {"VALUE": "SECOND"}, {"VALUE": "THIRD"}]
   *
   * Primitives array:
   * "MOTCLE": ["FIRST", "SECOND", "THIRD"]
   *
   * This is helpful to keep dealing with AngularJS 2-way data binding
   * in scopes straight forward.
   *
   * https://github.com/angular/angular.js/wiki/Understanding-Scopes
   *
   */
  case class MOTCLEValue(VALUE: String)

  implicit object MOTCLEValueFormat extends Format[MOTCLEValue] {

    def reads(json: JsValue): JsResult[MOTCLEValue] = {
      val motcleContent: JsResult[(String)] = for {
        value <- (json \ "VALUE").validate[String]
      } yield (value)

      motcleContent match {
        case JsSuccess((value), path) =>
          val motcleValue = MOTCLEValue(value)
          JsSuccess(motcleValue)
        case JsError(errors) => JsError("Error reading MOTCLEValue") ++ JsError(errors)
      }
    }

    def writes(motcleValue: MOTCLEValue): JsValue = Json.obj(
      "VALUE" -> motcleValue.VALUE)
  }

  implicit object IDENTIFIANTFormat extends Format[IDENTIFIANT] {

    def reads(json: JsValue): JsResult[IDENTIFIANT] = {

      // Read the motcle object layer additional to geoXML data model 
      val listOfJsMotcle = (json \ "MOTCLE").validate[List[MOTCLEValue]]
      val motsCleJsResult = listOfJsMotcle match {
        case JsSuccess(motcles, path) => {
          // Transform the data structure: 
          // "MOTCLE": [{ "VALUE": "FIRST" }, {"VALUE": "SECOND"}, {"VALUE": "THIRD"}]
          // to: 
          // "MOTCLE": ["FIRST", "SECOND", "THIRD"]
          val motcleStringList = for (motcle <- motcles) yield motcle.VALUE
          JsSuccess(motcleStringList, path)
        }
        case JsError(errors) => JsError("Error reading MOTCLE") ++ JsError(errors)
      }

      //      val autReader = (JsPath \ "AUT").readNullable[String]
      //      val gerReader = (JsPath \ "GER").readNullable[String]
      //      val resReader = (JsPath \ "RES").readNullable[String]
      //      val nomReader = (JsPath \ "NOM").readNullable[String]
      //      val aliasReader = (JsPath \ "ALIAS").readNullable[String]
      //      val origineReader = (JsPath \ "ORIGINE").readNullable[String]
      //      val objectifReader = (JsPath \ "OBJECTIF").readNullable[String]
      //      val qualiteReader = (JsPath \ "QUALITE").readNullable[String]
      //      val compteReader = (JsPath \ "COMPTE").readNullable[String]
      //      val deReader = (JsPath \ "DE").readNullable[String]
      //      val aReader = (JsPath \ "A").readNullable[String]
      //      val parReader = (JsPath \ "PAR").readNullable[String]
      //      val valeur_a_neufReader = (JsPath \ "VALEUR_A_NEUF").readNullable[Double]
      //      val valeurReader = (JsPath \ "VALEUR").readNullable[Double]

      val identifiantContent: JsResult[(Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[Double], Option[Double], Seq[String])] = for {
        //        aut <- (json \ "AUT").validate[Option[String]](autReader)
        //        ger <- (json \ "GER").validate[Option[String]](gerReader)
        //        res <- (json \ "RES").validate[Option[String]](resReader)
        //        nom <- (json \ "NOM").validate[Option[String]](nomReader)
        //        alias <- (json \ "ALIAS").validate[Option[String]](aliasReader)
        //        origine <- (json \ "ORIGINE").validate[Option[String]](origineReader)
        //        objectif <- (json \ "OBJECTIF").validate[Option[String]](objectifReader)
        //        qualite <- (json \ "QUALITE").validate[Option[String]](qualiteReader)
        //        compte <- (json \ "COMPTE").validate[Option[String]](compteReader)
        //        de <- (json \ "DE").validate[Option[String]](deReader)
        //        a <- (json \ "A").validate[Option[String]](aReader)
        //        par <- (json \ "PAR").validate[Option[String]](parReader)
        //        valeur_a_neuf <- (json \ "VALEUR_A_NEUF").validate[Option[Double]](valeur_a_neufReader)
        //        valeur <- (json \ "VALEUR").validate[Option[Double]](valeurReader)

        aut <- (json \ "AUT").validate[Option[String]]
        ger <- (json \ "GER").validate[Option[String]]
        res <- (json \ "RES").validate[Option[String]]
        nom <- (json \ "NOM").validate[Option[String]]
        alias <- (json \ "ALIAS").validate[Option[String]]
        origine <- (json \ "ORIGINE").validate[Option[String]]
        objectif <- (json \ "OBJECTIF").validate[Option[String]]
        qualite <- (json \ "QUALITE").validate[Option[String]]
        compte <- (json \ "COMPTE").validate[Option[String]]
        de <- (json \ "DE").validate[Option[String]]
        a <- (json \ "A").validate[Option[String]]
        par <- (json \ "PAR").validate[Option[String]]
        valeur_a_neuf <- (json \ "VALEUR_A_NEUF").validate[Option[Double]]
        valeur <- (json \ "VALEUR").validate[Option[Double]]

        motscle <- motsCleJsResult
      } yield (aut, ger, res, nom, alias, origine, objectif, qualite, compte, de, a, par, valeur_a_neuf, valeur, motscle)

      identifiantContent match {
        case JsSuccess((aut, ger, res, nom, alias, origine, objectif, qualite, compte, de, a, par, valeur_a_neuf, valeur, motscle), path) =>
          JsSuccess(IDENTIFIANT(aut, ger, res, nom, alias, origine, objectif, qualite, compte, de, a, par, valeur_a_neuf, valeur, motscle))
        case JsError(errors) => JsError("Error reading IDENTIFIANT") ++ JsError(errors)
      }

    }

    def writes(identifiant: IDENTIFIANT): JsValue = {

      // Create additional object level array for MOTCLE
      val motscleJsSeq = for (motcle <- identifiant.MOTCLE) yield Json.obj("VALUE" -> JsString(motcle))

      val valeur_a_neuf = identifiant.VALEUR_A_NEUF match {
        case Some(value) => JsNumber(value)
        case None        => JsNull
      }

      val valeur = identifiant.VALEUR match {
        case Some(value) => JsNumber(value)
        case None        => JsNull
      }

      val jsvalue = Json.obj(
        "AUT" -> JsString(identifiant.AUT.getOrElse(null)),
        "GER" -> JsString(identifiant.GER.getOrElse(null)),
        "RES" -> JsString(identifiant.RES.getOrElse(null)),
        "NOM" -> JsString(identifiant.NOM.getOrElse(null)),
        "ALIAS" -> JsString(identifiant.ALIAS.getOrElse(null)),
        "ORIGINE" -> JsString(identifiant.ORIGINE.getOrElse(null)),
        "OBJECTIF" -> JsString(identifiant.OBJECTIF.getOrElse(null)),
        "QUALITE" -> JsString(identifiant.QUALITE.getOrElse(null)),
        "COMPTE" -> JsString(identifiant.COMPTE.getOrElse(null)),
        "DE" -> JsString(identifiant.DE.getOrElse(null)),
        "A" -> JsString(identifiant.A.getOrElse(null)),
        "PAR" -> JsString(identifiant.PAR.getOrElse(null)),
        "VALEUR_A_NEUF" -> valeur_a_neuf,
        "VALEUR" -> valeur,
        "MOTCLE" -> JsArray(motscleJsSeq))
      jsvalue
    }

  }

  // ==================================================================
  // ch.bsisa.hyperbird.model.FILIATION and sub tree
  // ==================================================================  

  implicit val PROPRIETEFormat: Format[PROPRIETE] = Json.format[PROPRIETE]

  /**
   * PARENT has 0 to n PROPRIETE sub element but a List can be Nil
   */
  implicit object PARENTFormat extends Format[PARENT] {

    def reads(json: JsValue): JsResult[PARENT] = {

      val parentJsRes = for {
        propJsList <- (json \ "PROPRIETE").validate[List[PROPRIETE]]
        id <- (json \ "Id").validate[String]
        idg <- (json \ "ID_G").validate[Option[String]]
        classe <- (json \ "CLASSE").validate[Option[String]]
      } yield (propJsList, id, idg, classe)

      parentJsRes match {
        case JsSuccess((propList, id, idg, classe), path) => JsSuccess(PARENT(propList, id, idg, classe))
        case JsError(e)                                   => JsError("Error reading CARSET") ++ JsError(e)
      }
    }

    def writes(parent: PARENT): JsValue = {
      // Note: Force PROPRIETEFormat 

      val propJsSeqOption = parent.PROPRIETE match {
        case Nil => None
        case _ =>
          val propJsSeq = for {
            p <- parent.PROPRIETE
          } yield Json.toJson(p)(PROPRIETEFormat)
          Some(propJsSeq)
      }

      val propJsSeq = for {
        p <- parent.PROPRIETE
      } yield Json.toJson(p)(PROPRIETEFormat)
      Some(propJsSeq)

      val id: Option[(String, play.api.libs.json.Json.JsValueWrapper)] = Some(("Id" -> parent.Id))

      val idg: Option[(String, play.api.libs.json.Json.JsValueWrapper)] = parent.ID_G match {
        case Some(idg) => Some("ID_G" -> parent.ID_G)
        case None      => None
      }

      val classe: Option[(String, play.api.libs.json.Json.JsValueWrapper)] = parent.CLASSE match {
        case Some(classe) => Some("CLASSE" -> parent.CLASSE)
        case None         => None
      }

      // Filtering on empty PROPRIETE breaks tests, currently always keep a PROPRIETE property even if empty.
      val proprietes: Option[(String, play.api.libs.json.Json.JsValueWrapper)] = Some(("PROPRIETE", propJsSeq))
      //      val proprietes: Option[(String, play.api.libs.json.Json.JsValueWrapper)] = propJsSeqOption match {
      //        case Some(ps) => Some("PROPRIETE", ps)
      //        case None => None
      //      }

      val parentTupleOptions = Seq[Option[(String, play.api.libs.json.Json.JsValueWrapper)]](
        id,
        idg,
        classe,
        proprietes)

      val parentTupleCleaned = parentTupleOptions.filter(p => p.isDefined).map(f => f.get)

      // TODO: Use generic tuple for possible future filtering of optional or empty seq
      Json.obj(parentTupleCleaned: _*)

    }
  }

  /**
   * FILIATION has 1 to n PARENT sub elements.
   */
  implicit object FILIATIONFormat extends Format[FILIATION] {

    def reads(json: JsValue): JsResult[FILIATION] = {
      val parentsRes: JsResult[Seq[PARENT]] = for {
        parent <- (json \ "PARENT").validate[List[PARENT]]
      } yield (parent)

      parentsRes match {
        case JsSuccess((parents), path) => JsSuccess(FILIATION(parents))
        case JsError(errors)            => JsError("Error reading FILIATION") ++ JsError(errors)
      }
    }

    def writes(filiation: FILIATION): JsValue = {
      val parentJsSeq = for (p <- filiation.PARENT) yield Json.toJson(p)(PARENTFormat)
      Json.obj("PARENT" -> JsArray(parentJsSeq))
    }

  }

  // ==================================================================
  // ch.bsisa.hyperbird.model.CARACTERISTIQUE and sub tree
  // ==================================================================  

  implicit object CARTypableFormat extends Format[CARTypable] {

    //     val nomReader = (JsPath \ "NOM").readNullable[String]
    //     val uniteReader = (JsPath \ "UNITE").readNullable[String]
    //     val valeurReader = (JsPath \ "VALEUR").readNullable[String]

    def reads(json: JsValue): JsResult[CARType] = {
      val nomUniteValeur: JsResult[(Option[String], Option[String], Option[String])] = for {
        //        nom <- (json \ "NOM").validate[Option[String]](nomReader)
        //        unite <- (json \ "UNITE").validate[Option[String]](uniteReader)
        //        valeur <- (json \ "VALEUR").validate[Option[String]](valeurReader)
        nom <- (json \ "NOM").validate[Option[String]]
        unite <- (json \ "UNITE").validate[Option[String]]
        valeur <- (json \ "VALEUR").validate[Option[String]]
      } yield (nom, unite, valeur)

      nomUniteValeur match {
        case JsSuccess((nom, unite, valeur), path) => JsSuccess(CARType(nom, unite, valeur))
        case JsError(errors)                       => JsError("Error reading CARTypable") ++ JsError(errors)
      }
    }

    def writes(c: CARTypable): JsValue = Json.obj(
      "NOM" -> c.NOM,
      "UNITE" -> c.UNITE,
      "VALEUR" -> c.VALEUR)
  }

  // BigInt formatter allows JSON inception.
  implicit val CARSET_CARTypeFormat: Format[CARSET_CARType] = Json.format[CARSET_CARType]

  implicit object CARSETFormat extends Format[CARSET] {

    def reads(json: JsValue): JsResult[CARSET] = {
      val carJsSeq = (json \ "CAR").validate[List[CARSET_CARType]]
      carJsSeq match {
        case JsSuccess(carList, path) => JsSuccess(CARSET(carList))
        case JsError(e)               => JsError("Error reading CARSET") ++ JsError(e)
      }
    }

    def writes(cs: CARSET): JsValue = {
      // Note: Force CARSET_CARTypeFormat between it and CARTypableFormat
      val carJsSeq = for (c <- cs.CAR) yield Json.toJson(c)(CARSET_CARTypeFormat)
      Json.obj("CAR" -> JsArray(carJsSeq))
    }
  }

  implicit object STATETypeFormat extends Format[STATEType] {

    //    val bReader = (JsPath \ "B").readNullable[String]
    //    val cReader = (JsPath \ "C").readNullable[String]

    def reads(json: JsValue): JsResult[STATEType] = {
      val bAndCAndContent: JsResult[(Option[String], Option[String], String)] = for {
        //        b <- (json \ "B").validate[Option[String]](bReader)
        //        c <- (json \ "C").validate[Option[String]](cReader)
        b <- (json \ "B").validate[Option[String]]
        c <- (json \ "C").validate[Option[String]]
        content <- (json \ MixedContentJsonPropName).validate[String]
      } yield (b, c, content)

      bAndCAndContent match {
        case JsSuccess((b, c, content), path) =>
          val stateType = STATEType(setMixedContent(content), b, c)
          JsSuccess(stateType)
        case JsError(errors) => JsError("Error reading STATEType") ++ JsError(errors)
      }
    }

    def writes(st: STATEType): JsValue = Json.obj(
      "B" -> st.B,
      "C" -> st.C,
      MixedContentJsonPropName -> getMixedContent(st.mixed))
  }

  //ETAT
  implicit val ETATFormat: Format[ETAT] = Json.format[ETAT]

  implicit object CFormat extends Format[C] {

    def reads(json: JsValue): JsResult[C] = {
      val posContent: JsResult[(BigInt, String)] = for {
        pos <- (json \ "POS").validate[BigInt]
        content <- (json \ MixedContentJsonPropName).validate[String]
      } yield (pos, content)

      posContent match {
        case JsSuccess((pos, content), path) =>
          val column = C(setMixedContent(content), pos)
          JsSuccess(column)
        case JsError(errors) => JsError("Error reading C") ++ JsError(errors)
      }
    }

    def writes(c: C): JsValue = Json.obj(
      "POS" -> c.POS,
      MixedContentJsonPropName -> getMixedContent(c.mixed))
  }

  implicit val LFormat: Format[L] = Json.format[L]

  // CALCUL => CALCULType (ok) => DIMENSION (ok) => NOM (ok) => TYPEType (ok)
  implicit object NOMFormat extends Format[NOM] {

    def reads(json: JsValue): JsResult[NOM] = json match {
      case JsString(value) => value match {
        case "LONGUEUR" => JsSuccess(LONGUEUR)
        case "SURFACE"  => JsSuccess(SURFACE)
        case "RAYON"    => JsSuccess(RAYON)
        case invalid    => JsError(s"Invalid string value ${invalid} found for NOM. Valid values are {LONGUEUR,SURFACE,RAYON}")
      }
      case _ => JsError(s"Invalid JsValue type received for NOM. Expecting JsString only.")
    }

    def writes(n: NOM): JsValue = n.toString match {
      case "LONGUEUR" => JsString("LONGUEUR")
      case "SURFACE"  => JsString("SURFACE")
      case "RAYON"    => JsString("RAYON")
    }
  }

  implicit object DIMENSIONFormat extends Format[DIMENSION] {

    def reads(json: JsValue): JsResult[DIMENSION] = {

      val nomTypeContent: JsResult[(NOM, TYPEType, String)] = for {
        nom <- (json \ "NOM").validate[NOM]
        typet <- (json \ "TYPE").validate[TYPEType]
        content <- (json \ MixedContentJsonPropName).validate[String]
      } yield (nom, typet, content)

      nomTypeContent match {
        case JsSuccess((nom, typet, content), path) =>
          val dimension = DIMENSION(setMixedContent(content), nom, typet)
          JsSuccess(dimension)
        case JsError(errors) => JsError("Error reading DIMENSION") ++ JsError(errors)
      }
    }

    def writes(d: DIMENSION): JsValue = Json.obj(
      "NOM" -> Json.toJson[NOM](d.NOM),
      "TYPE" -> Json.toJson[TYPEType](d.TYPE),
      MixedContentJsonPropName -> getMixedContent(d.mixed))
  }

  // CALCULType
  implicit val CALCULTypeFormat: Format[CALCULType] = Json.format[CALCULType]

  // FRACTION => MATRICETypable (abtract trait) => MATRICEType (ok)
  implicit object MATRICETypableFormat extends Format[MATRICETypable] {

    def reads(json: JsValue): JsResult[MATRICEType] = {
      val lineJsSeq = (json \ "L").validate[List[L]]
      lineJsSeq match {
        case JsSuccess(lineList, path) => JsSuccess(MATRICEType(lineList))
        case JsError(e)                => JsError("Error reading MATRICETypable") ++ JsError(e)
      }
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
        content <- (json \ MixedContentJsonPropName).validate[String]
      } yield (id, idg, nom, groupe, content)

      idIdgNomGroupeContent match {
        case JsSuccess((id, idg, nom, groupe, content), path) =>
          val personne = PERSONNEType(setMixedContent(content), Option(id), Option(idg), Option(nom), Option(groupe))
          JsSuccess(personne)
        case JsError(errors) => JsError("Error reading PERSONNEType") ++ JsError(errors)
      }
    }

    def writes(p: PERSONNEType): JsValue = Json.obj(
      "Id" -> JsString(p.Id.getOrElse("null")),
      "ID_G" -> JsString(p.ID_G.getOrElse("null")),
      "NOM" -> JsString(p.NOM.getOrElse("null")),
      "GROUPE" -> JsString(p.GROUPE.getOrElse("null")),
      MixedContentJsonPropName -> getMixedContent(p.mixed))
  }

  // PARTENAIRE => {GERANT,USAGER,FOURNISSEUR,PROPRIETAIRE} => PERSONNEType
  implicit val PARTENAIREFormat: Format[PARTENAIRE] = Json.format[PARTENAIRE]

  // ACTIVITE => EVENEMENT => ECHEANCE => E_STATUT  GESTION => MATRICETypable
  implicit object E_STATUTFormat extends Format[E_STATUT] {

    def reads(json: JsValue): JsResult[E_STATUT] = json match {
      case JsString(value) => value match {
        case "OK"          => JsSuccess(OK)
        case "A EFFECTUER" => JsSuccess(AEFFECTUER)
        case invalid       => JsError(s"Invalid string value ${invalid} found for E_STATUT. Valid values are {OK,A EFFECTUER}")
      }
      case _ => JsError(s"Invalid JsValue type received for E_STATUT. Expecting JsString only.")
    }

    def writes(e: E_STATUT): JsValue = e.toString match {
      case "OK"          => JsString("OK")
      case "A EFFECTUER" => JsString("A EFFECTUER")
    }
  }

  // ECHEANCE => E_STATUT  GESTION => MATRICETypable
  implicit val ECHEANCEFormat: Format[ECHEANCE] = Json.format[ECHEANCE]

  // EVENEMENT => ECHEANCE => E_STATUT  GESTION => MATRICETypable
  implicit object EVENEMENTFormat extends Format[EVENEMENT] {

    def reads(json: JsValue): JsResult[EVENEMENT] = {

      val echeanceJsSeq = (json \ "ECHEANCE").validate[List[ECHEANCE]]
      echeanceJsSeq match {
        case JsSuccess(echeanceList, path) => JsSuccess(EVENEMENT(echeanceList))
        case JsError(e)                    => JsError("Error reading EVENEMENT") ++ JsError(e)
      }
    }

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

    def reads(json: JsValue): JsResult[FONCTION] = json match {
      case JsString(value) => value match {
        case "LIBRE" => JsSuccess(LIBRE)
        case "LIE"   => JsSuccess(LIE)
        case "BASE"  => JsSuccess(BASE)
        case invalid => JsError(s"Invalid string value ${invalid} found for FONCTION. Valid values are {LIBRE,LIE,BASE}")
      }
      case _ => JsError(s"Invalid JsValue type received for FONCTION. Expecting JsString with valid values {LIBRE,LIE,BASE} only.")
    }

    def writes(f: FONCTION): JsValue = f.toString match {
      case "LIBRE" => JsString("LIBRE")
      case "LIE"   => JsString("LIE")
      case "BASE"  => JsString("BASE")
    }
  }

  implicit val POINTFormat: Format[POINT] = Json.format[POINT]

  implicit val PIECEFormat: Format[PIECE] = Json.format[PIECE]

  implicit object FONCTIONTypeFormat extends Format[FONCTIONType] {

    def reads(json: JsValue): JsResult[FONCTIONType] = json match {
      case JsString(value) => value match {
        case "DEBUT"     => JsSuccess(DEBUT)
        case "MILIEU"    => JsSuccess(MILIEU)
        case "FIN"       => JsSuccess(FIN)
        case "PASSAGE"   => JsSuccess(PASSAGEValue)
        case "ACTUATEUR" => JsSuccess(ACTUATEUR)
        case "DEPART"    => JsSuccess(DEPART)
        case invalid     => JsError(s"Invalid string value ${invalid} found for FONCTION. Valid values are {DEBUT,MILIEU,FIN,PASSAGE,ACTUATEUR,DEPART}")
      }
      case _ => JsError(s"Invalid JsValue type received for FONCTION. Expecting JsString with valid values {DEBUT,MILIEU,FIN,PASSAGE,ACTUATEUR,DEPART} only.")
    }

    def writes(f: FONCTIONType): JsValue = f.toString match {
      case "DEBUT"     => JsString("DEBUT")
      case "MILIEU"    => JsString("MILIEU")
      case "FIN"       => JsString("FIN")
      case "PASSAGE"   => JsString("PASSAGE")
      case "ACTUATEUR" => JsString("ACTUATEUR")
      case "DEPART"    => JsString("DEPART")
    }
  }

  implicit val PASSAGEFormat: Format[PASSAGE] = Json.format[PASSAGE]

  implicit val GUIDEFormat: Format[GUIDE] = Json.format[GUIDE]

  implicit object DIRECTIONFormat extends Format[DIRECTION] {

    def reads(json: JsValue): JsResult[DIRECTION] = json match {
      case JsString(value) => value match {
        case "AVAL"  => JsSuccess(AVAL)
        case "AMONT" => JsSuccess(AMONT)
        case invalid => JsError(s"Invalid string value ${invalid} found for DIRECTION. Valid values are {AVAL,AMONT}")
      }
      case _ => JsError(s"Invalid JsValue type received for DIRECTION. Expecting JsString only.")
    }

    def writes(f: DIRECTION): JsValue = f.toString match {
      case "AVAL"  => JsString("AVAL")
      case "AMONT" => JsString("AMONT")
    }
  }

  implicit object FONCTIONType2Format extends Format[FONCTIONType2] {

    def reads(json: JsValue): JsResult[FONCTIONType2] = json match {
      case JsString(value) => value match {
        case "FRONTIERE" => JsSuccess(FRONTIERE)
        case "AXE"       => JsSuccess(AXE)
        case invalid     => JsError(s"Invalid string value ${invalid} found for FONCTION. Valid values are {FRONTIERE,AXE}")
      }
      case _ => JsError(s"Invalid JsValue type received for FONCTION. Expecting JsString with valid values {FRONTIERE,AXE} only.")
    }

    def writes(f: FONCTIONType2): JsValue = f.toString match {
      case "FRONTIERE" => JsString("FRONTIERE")
      case "AXE"       => JsString("AXE")
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
      case JsError(e)            => JsError("Error reading java.net.URI") ++ JsError(e)
    }

    //use l.toASCIIString if encoding to ASCII is required, l.toString otherwise
    def writes(l: java.net.URI): JsValue = JsString(l.toASCIIString)
  }

  implicit object RENVOIFormat extends Format[RENVOI] {

    def reads(json: JsValue): JsResult[RENVOI] = {
      val posLienContent: JsResult[(BigInt, java.net.URI, String)] = for {
        pos <- (json \ "POS").validate[BigInt]
        lien <- (json \ "LIEN").validate[java.net.URI]
        content <- (json \ MixedContentJsonPropName).validate[String]
      } yield (pos, lien, content)

      posLienContent match {
        case JsSuccess((pos, lien, content), path) =>
          JsSuccess(RENVOI(setMixedContent(content), pos, lien))
        case JsError(errors) => JsError("Error reading RENVOI") ++ JsError(errors)
      }
    }

    def writes(r: RENVOI): JsValue = Json.obj(
      "POS" -> r.POS,
      "LIEN" -> r.LIEN,
      MixedContentJsonPropName -> getMixedContent(r.mixed))
  }

  // ANNEXE => RENVOI => LIEN
  implicit object ANNEXEFormat extends Format[ANNEXE] {

    def reads(json: JsValue): JsResult[ANNEXE] = {
      val jsResult = (json \ "RENVOI").validate[List[RENVOI]]
      jsResult match {
        case JsSuccess(renvoiList, path) => JsSuccess(ANNEXE(renvoiList))
        case JsError(e)                  => JsError("Error reading ANNEXE") ++ JsError(e)
      }
    }

    def writes(annexes: ANNEXE): JsValue = {
      val renvoiJsSeq = for (r <- annexes.RENVOI) yield Json.toJson(r)
      Json.obj("RENVOI" -> JsArray(renvoiJsSeq))
    }
  }

  // DIVERS
  implicit val DIVERSFormat: Format[DIVERS] = Json.format[DIVERS]

  implicit object TYPEFormat extends Format[TYPE] {

    def reads(json: JsValue): JsResult[TYPE] = json match {
      case JsString(value) => value match {
        case "BIEN"     => JsSuccess(BIEN)
        case "ACTIVITE" => JsSuccess(ACTIVITEValue)
        case "PERSONNE" => JsSuccess(PERSONNE)
        case "DOCUMENT" => JsSuccess(DOCUMENT)
        case invalid    => JsError(s"Invalid string value ${invalid} found for TYPE. Valid values are {BIEN,ACTIVITE,PERSONNE,DOCUMENT}")
      }
      case _ => JsError(s"Invalid JsValue type received for TYPE. Expecting JsString with valid values {BIEN,ACTIVITE,PERSONNE,DOCUMENT} only.")
    }

    def writes(t: TYPE): JsValue = JsString(t.toString)

  }

  implicit val ELFINFormat: Format[ELFIN] = Json.format[ELFIN]

  ///////////////////////////////////////////////////////////////////////////

}