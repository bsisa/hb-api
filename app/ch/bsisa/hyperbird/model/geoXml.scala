// Generated by <a href="http://scalaxb.org/">scalaxb</a>.
package ch.bsisa.hyperbird.model


/** 
            20051125. BSI/GdP. Suppression de l'attribut MUTATION de l'ELFIN et ajout d'un noeud MUTATIONS/MUTATION. Ajout des attributs SOURCE à ELFIN. Ajout de l'Attribut CLASSE à LIGNE
        20050308. BSI/GdP. Ajout de l'attribut MUTATION à ELFIN. Ajout des attributs ETA et ETAS à PASSAGE. Suppression de l'obligation d'avoir GEOSELECTION. 20040819. BSI/OG/GdP. Ajout d'un attribut FONCTION a LIGNE. Ajout de ENTREE et SORTIE dans les fonctions de PASSAGE (traitement des tissus) et ajout de BASE aux fonctions de POINT.20040819. BSI/GdP. Introduction de IdRefType pour les id de type G12345678901234567#123.20040818. BSI/GdP. Corrections mineures de champs rendus non obligatoires.20040816. BSI/PYS/GdP/OG. Ajout de KSIS, ALPHAS et ANGLES à POINT. Ajout de l'attribut VALEUR dans le type CAR et suppression de la définition mixte. Autorisation de plusieurs LIGNEs et plusieurs ZONEs par FORME. Ajout d'un attribute REMARQUE dans LIGNE, ZONE et SYMBOLE. Ajout de l'attribut POS à SYMBOLE.20040623. BSI/GdP/OG. Refonte et simplification du bloc FORME: Quatre formes possibles : POINT, LIGNE, ZONE et SYMBOLE. Corrections mineures20040623. BSI/GdP. Modification de PARTENAIREType en PERSONNEType et ETATType en STATEType pour la compatibilité JAXB20040614. BSI/GdP. GeoXML v4.020040105. BSI/OG. Adaptation pour HB3.2 selon DTD 20031001
            GdP et CD. Modification de FORME. Ajout attribut POS dans RENVOI. Passage de CALCUL en LC20030530. BSI/GdP. Suppression de CAR7. Ajout des balises
            PAR, PROPRIETAIRE, CARSET, RENVOI, L et C. Modification de HISTORIQUE, FRACTION, GESTION
            (ajout de L). Modification de ANNEXE (ajout de RENVOI)(C) 2004 BSI Bureau de Service et d'ingénierie SA
*/


case class MELFIN(ELFIN: Seq[ch.bsisa.hyperbird.model.ELFIN] = Nil)
      


case class ELFIN(MUTATIONS: Option[ch.bsisa.hyperbird.model.MUTATIONS] = None,
  GEOSELECTION: Option[ch.bsisa.hyperbird.model.GEOSELECTION] = None,
  IDENTIFIANT: Option[ch.bsisa.hyperbird.model.IDENTIFIANT] = None,
  CARACTERISTIQUE: Option[ch.bsisa.hyperbird.model.CARACTERISTIQUE] = None,
  PARTENAIRE: Option[ch.bsisa.hyperbird.model.PARTENAIRE] = None,
  ACTIVITE: Option[ch.bsisa.hyperbird.model.ACTIVITE] = None,
  FORME: Option[ch.bsisa.hyperbird.model.FORME] = None,
  ANNEXE: Option[ch.bsisa.hyperbird.model.ANNEXE] = None,
  DIVERS: Option[ch.bsisa.hyperbird.model.DIVERS] = None,
  Id: String,
  ID_G: String,
  CLASSE: String,
  GROUPE: Option[String] = None,
  TYPE: Option[ch.bsisa.hyperbird.model.TYPE] = None,
  NATURE: String,
  SOURCE: Option[String] = None)
      

trait TYPE

object TYPE {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): TYPE = value match {
    case "BIEN" => BIEN
    case "ACTIVITE" => ACTIVITEValue
    case "PERSONNE" => PERSONNE
    case "DOCUMENT" => DOCUMENT

  }
}

case object BIEN extends TYPE { override def toString = "BIEN" }
case object ACTIVITEValue extends TYPE { override def toString = "ACTIVITE" }
case object PERSONNE extends TYPE { override def toString = "PERSONNE" }
case object DOCUMENT extends TYPE { override def toString = "DOCUMENT" }


case class MUTATIONS(MUTATION: Seq[ch.bsisa.hyperbird.model.MUTATION] = Nil)
      


case class MUTATION(DATE: String,
  ROLE: String,
  MOT_DE_PASSE: Option[String] = None,
  UTILISATEUR: Option[String] = None)
      


case class GEOSELECTION(CENTROIDE: Seq[ch.bsisa.hyperbird.model.CENTROIDE] = Nil)
      


case class CENTROIDE(TYPE: ch.bsisa.hyperbird.model.TYPEType,
  XM: Double,
  YM: Double,
  ZM: Option[Double] = None,
  RM: Double)
      


case class IDENTIFIANT(AUT: Option[String] = None,
  GER: Option[String] = None,
  RES: Option[String] = None,
  NOM: Option[String] = None,
  ALIAS: Option[String] = None,
  ORIGINE: Option[String] = None,
  OBJECTIF: Option[String] = None,
  QUALITE: Option[String] = None,
  COMPTE: Option[String] = None,
  DE: Option[String] = None,
  A: Option[String] = None,
  PAR: Option[String] = None,
  VALEUR_A_NEUF: Option[Double] = None,
  VALEUR: Option[Double] = None,
  MOTCLE: Seq[String] = Nil)
      


case class FORME(POINT: Seq[ch.bsisa.hyperbird.model.POINT] = Nil,
  LIGNE: Seq[ch.bsisa.hyperbird.model.LIGNE] = Nil,
  ZONE: Seq[ch.bsisa.hyperbird.model.ZONE] = Nil,
  SYMBOLE: Seq[ch.bsisa.hyperbird.model.SYMBOLE] = Nil)
      


case class POINT(POS: BigInt,
  X: Option[Double] = None,
  Y: Option[Double] = None,
  Z: Double,
  KSI: Double,
  ANGLE: Double,
  ALPHA: String,
  XS: Option[Double] = None,
  YS: Option[Double] = None,
  ZS: Double,
  KSIS: Double,
  ANGLES: Double,
  ALPHAS: String,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  FONCTION: ch.bsisa.hyperbird.model.FONCTION,
  CLASSE: Option[String] = None,
  GROUPE: Option[String] = None,
  REMARQUE: Option[String] = None)
      

trait FONCTION

object FONCTION {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): FONCTION = value match {
    case "LIBRE" => LIBRE
    case "LIE" => LIE
    case "BASE" => BASE

  }
}

case object LIBRE extends FONCTION { override def toString = "LIBRE" }
case object LIE extends FONCTION { override def toString = "LIE" }
case object BASE extends FONCTION { override def toString = "BASE" }


case class PASSAGE(PIECE: Option[ch.bsisa.hyperbird.model.PIECE] = None,
  POS: Option[BigInt] = None,
  KSI: Double,
  ANGLE: Double,
  ALPHA: Option[String] = None,
  KSIS: Double,
  ANGLES: Double,
  ALPHAS: Option[String] = None,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  FONCTION: ch.bsisa.hyperbird.model.FONCTIONType,
  CLASSE: Option[String] = None,
  GROUPE: Option[String] = None,
  REMARQUE: Option[String] = None)
      

trait FONCTIONType

object FONCTIONType {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): FONCTIONType = value match {
    case "DEBUT" => DEBUT
    case "MILIEU" => MILIEU
    case "FIN" => FIN
    case "PASSAGE" => PASSAGEValue
    case "ACTUATEUR" => ACTUATEUR
    case "DEPART" => DEPART

  }
}

case object DEBUT extends FONCTIONType { override def toString = "DEBUT" }
case object MILIEU extends FONCTIONType { override def toString = "MILIEU" }
case object FIN extends FONCTIONType { override def toString = "FIN" }
case object PASSAGEValue extends FONCTIONType { override def toString = "PASSAGE" }
case object ACTUATEUR extends FONCTIONType { override def toString = "ACTUATEUR" }
case object DEPART extends FONCTIONType { override def toString = "DEPART" }


case class GUIDE(PIECE: Option[ch.bsisa.hyperbird.model.PIECE] = None,
  POS: BigInt,
  KSI: Double,
  KSI2: Double,
  ETA: Double,
  ALPHA: Option[String] = None,
  KSIS: Double,
  KSIS2: Double,
  ETAS: Double,
  ALPHAS: Option[String] = None,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  CLASSE: Option[String] = None,
  REMARQUE: Option[String] = None)
      


case class PIECE(CAR1: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CAR2: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CAR3: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  ALIAS: String,
  Id: String,
  ID_G: String,
  SYMBOLE: Option[String] = None,
  LONGUEUR_UTILE: Double,
  ANGLE: Double,
  ECHELLE_X: Double,
  ECHELLE_Y: Double,
  ECHELLE_Z: Double)
      


case class LIGNE(PASSAGE: Seq[ch.bsisa.hyperbird.model.PASSAGE] = Nil,
  GUIDE: Seq[ch.bsisa.hyperbird.model.GUIDE] = Nil,
  POS: BigInt,
  DIRECTION: ch.bsisa.hyperbird.model.DIRECTION,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  CLASSE: Option[String] = None,
  FONCTION: Option[ch.bsisa.hyperbird.model.FONCTIONType2] = None,
  REMARQUE: Option[String] = None)
      

trait DIRECTION

object DIRECTION {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): DIRECTION = value match {
    case "AVAL" => AVAL
    case "AMONT" => AMONT

  }
}

case object AVAL extends DIRECTION { override def toString = "AVAL" }
case object AMONT extends DIRECTION { override def toString = "AMONT" }

trait FONCTIONType2

object FONCTIONType2 {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): FONCTIONType2 = value match {
    case "FRONTIERE" => FRONTIERE
    case "AXE" => AXE

  }
}

case object FRONTIERE extends FONCTIONType2 { override def toString = "FRONTIERE" }
case object AXE extends FONCTIONType2 { override def toString = "AXE" }


case class ZONE(LIGNE: Seq[ch.bsisa.hyperbird.model.LIGNE] = Nil,
  POS: Option[BigInt] = None,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  CLASSE: Option[String] = None,
  REMARQUE: Option[String] = None)
      


case class SYMBOLE(LIBELLE: Seq[ch.bsisa.hyperbird.model.LIBELLE] = Nil,
  POS: BigInt,
  NOM: String,
  CONTEXTE: String,
  X: Double,
  Y: Double,
  ALPHA: Option[String] = None,
  ANGLE: Double,
  ECHELLE_X: Double,
  ECHELLE_Y: Double,
  ECHELLE_Z: Double,
  REMARQUE: Option[String] = None)
      


case class LIBELLE(POS: Option[BigInt] = None,
  TEXTE: Option[String] = None,
  X: Double,
  Y: Double,
  ALPHA: Option[String] = None,
  TAILLE: Double,
  ANGLE: Double,
  REMARQUE: Option[String] = None)
      


case class CARACTERISTIQUE(CAR1: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CAR2: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CAR3: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CAR4: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CAR5: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CAR6: Option[ch.bsisa.hyperbird.model.CARTypable] = None,
  CARSET: Option[ch.bsisa.hyperbird.model.CARSET] = None,
  ETAT: Option[ch.bsisa.hyperbird.model.ETAT] = None,
  CALCUL: Option[ch.bsisa.hyperbird.model.CALCULType] = None,
  FRACTION: Option[ch.bsisa.hyperbird.model.MATRICETypable] = None)
      


/** Type générique
                définissant CAR1 à CAR6
*/
trait CARTypable {
  val NOM: Option[String]
  val UNITE: Option[String]
  val VALEUR: Option[String]
}


/** Type générique
                définissant CAR1 à CAR6
*/
case class CARType(NOM: Option[String] = None,
  UNITE: Option[String] = None,
  VALEUR: Option[String] = None) extends CARTypable
      


/** Type générique de
                caractéristique incluse dans CARSET. Etend CARType
*/
case class CARSET_CARType(NOM: Option[String] = None,
  UNITE: Option[String] = None,
  VALEUR: Option[String] = None,
  POS: BigInt) extends CARTypable
      


case class CARSET(CAR: Seq[ch.bsisa.hyperbird.model.CARSET_CARType] = Nil)
      


case class ETAT(ETAT1: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT2: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT3: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT4: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT5: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT6: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT7: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT8: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT9: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT10: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT11: Option[ch.bsisa.hyperbird.model.STATEType] = None,
  ETAT12: Option[ch.bsisa.hyperbird.model.STATEType] = None)
      


/** Type générique
                décrivant un ETAT
*/
case class STATEType(mixed: Seq[scalaxb.DataRecord[Any]] = Nil,
  B: Option[String] = None,
  C: Option[String] = None)
      


/** Le bloc CALCUL contient les dimensions de l'objet et
                l'ensemble des paramètres utilisés lors des calculs
*/
case class CALCULType(L: Seq[ch.bsisa.hyperbird.model.L] = Nil,
  DIMENSION: Seq[ch.bsisa.hyperbird.model.DIMENSION] = Nil) extends MATRICETypable
      

trait NOM

object NOM {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): NOM = value match {
    case "LONGUEUR" => LONGUEUR
    case "SURFACE" => SURFACE
    case "RAYON" => RAYON

  }
}

case object LONGUEUR extends NOM { override def toString = "LONGUEUR" }
case object SURFACE extends NOM { override def toString = "SURFACE" }
case object RAYON extends NOM { override def toString = "RAYON" }


case class DIMENSION(mixed: Seq[scalaxb.DataRecord[Any]] = Nil,
  NOM: ch.bsisa.hyperbird.model.NOM,
  TYPE: ch.bsisa.hyperbird.model.TYPEType)
      


/** MATRICE définit un bloc matricielle
*/
trait MATRICETypable {
  val L: Seq[ch.bsisa.hyperbird.model.L]
}


/** MATRICE définit un bloc matricielle
*/
case class MATRICEType(L: Seq[ch.bsisa.hyperbird.model.L] = Nil) extends MATRICETypable
      


case class L(C: Seq[ch.bsisa.hyperbird.model.C] = Nil,
  POS: BigInt)
      


case class C(mixed: Seq[scalaxb.DataRecord[Any]] = Nil,
  POS: BigInt)
      


case class PARTENAIRE(GERANT: Option[ch.bsisa.hyperbird.model.PERSONNEType] = None,
  USAGER: Option[ch.bsisa.hyperbird.model.PERSONNEType] = None,
  FOURNISSEUR: Option[ch.bsisa.hyperbird.model.PERSONNEType] = None,
  PROPRIETAIRE: Option[ch.bsisa.hyperbird.model.PERSONNEType] = None)
      


/** Type générique de partenaire. Le
                contenu du noeud contient une description du partenaire.
*/
case class PERSONNEType(mixed: Seq[scalaxb.DataRecord[Any]] = Nil,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  NOM: Option[String] = None,
  GROUPE: Option[String] = None)
      


case class ACTIVITE(EVENEMENT: Option[ch.bsisa.hyperbird.model.EVENEMENT] = None,
  GESTION: Option[ch.bsisa.hyperbird.model.MATRICETypable] = None)
      


case class EVENEMENT(ECHEANCE: Seq[ch.bsisa.hyperbird.model.ECHEANCE] = Nil)
      


case class ECHEANCE(DATE: String,
  ACTION: String,
  PAR_QUI: String,
  POUR_QUI: String,
  E_DATE: String,
  E_ACTION: String,
  E_PAR_QUI: String,
  E_POUR_QUI: String,
  E_STATUT: ch.bsisa.hyperbird.model.E_STATUT,
  REMARQUE: Option[String] = None)
      

trait E_STATUT

object E_STATUT {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): E_STATUT = value match {
    case "OK" => OK
    case "A EFFECTUER" => AEFFECTUER

  }
}

case object OK extends E_STATUT { override def toString = "OK" }
case object AEFFECTUER extends E_STATUT { override def toString = "A EFFECTUER" }


case class ANNEXE(RENVOI: Seq[ch.bsisa.hyperbird.model.RENVOI] = Nil)
      


case class RENVOI(mixed: Seq[scalaxb.DataRecord[Any]] = Nil,
  POS: BigInt,
  LIEN: java.net.URI)
      


case class DIVERS(REMARQUE: Option[String] = None,
  METHODE: Option[String] = None)
      

trait TYPEType

object TYPEType {
  def fromString(value: String, scope: scala.xml.NamespaceBinding): TYPEType = value match {
    case "GEOGRAPHIE" => GEOGRAPHIE
    case "SCHEMATIQUE" => SCHEMATIQUE

  }
}

case object GEOGRAPHIE extends TYPEType { override def toString = "GEOGRAPHIE" }
case object SCHEMATIQUE extends TYPEType { override def toString = "SCHEMATIQUE" }


case class ECHEANCEu46attributeGroup(DATE: String,
  ACTION: String,
  PAR_QUI: String,
  POUR_QUI: String,
  E_DATE: String,
  E_ACTION: String,
  E_PAR_QUI: String,
  E_POUR_QUI: String,
  E_STATUT: ch.bsisa.hyperbird.model.E_STATUT,
  REMARQUE: Option[String] = None)


case class STATETypeu46attributeGroup(B: Option[String] = None,
  C: Option[String] = None)


case class LIBELLEu46attributeGroup(POS: Option[BigInt] = None,
  TEXTE: Option[String] = None,
  X: Double,
  Y: Double,
  ALPHA: Option[String] = None,
  TAILLE: Double,
  ANGLE: Double,
  REMARQUE: Option[String] = None)


case class LIGNEu46attributeGroup(POS: BigInt,
  DIRECTION: ch.bsisa.hyperbird.model.DIRECTION,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  CLASSE: Option[String] = None,
  FONCTION: Option[ch.bsisa.hyperbird.model.FONCTIONType2] = None,
  REMARQUE: Option[String] = None)


case class GUIDEu46attributeGroup(POS: BigInt,
  KSI: Double,
  KSI2: Double,
  ETA: Double,
  ALPHA: Option[String] = None,
  KSIS: Double,
  KSIS2: Double,
  ETAS: Double,
  ALPHAS: Option[String] = None,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  CLASSE: Option[String] = None,
  REMARQUE: Option[String] = None)


case class POINTu46attributeGroup(POS: BigInt,
  X: Option[Double] = None,
  Y: Option[Double] = None,
  Z: Double,
  KSI: Double,
  ANGLE: Double,
  ALPHA: String,
  XS: Option[Double] = None,
  YS: Option[Double] = None,
  ZS: Double,
  KSIS: Double,
  ANGLES: Double,
  ALPHAS: String,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  FONCTION: ch.bsisa.hyperbird.model.FONCTION,
  CLASSE: Option[String] = None,
  GROUPE: Option[String] = None,
  REMARQUE: Option[String] = None)


case class MUTATIONu46attributeGroup(DATE: String,
  ROLE: String,
  MOT_DE_PASSE: Option[String] = None,
  UTILISATEUR: Option[String] = None)


case class ELFINu46attributeGroup(Id: String,
  ID_G: String,
  CLASSE: String,
  GROUPE: Option[String] = None,
  TYPE: Option[ch.bsisa.hyperbird.model.TYPE] = None,
  NATURE: String,
  SOURCE: Option[String] = None)


case class CENTROIDEu46attributeGroup(TYPE: ch.bsisa.hyperbird.model.TYPEType,
  XM: Double,
  YM: Double,
  ZM: Option[Double] = None,
  RM: Double)


case class PASSAGEu46attributeGroup(POS: Option[BigInt] = None,
  KSI: Double,
  ANGLE: Double,
  ALPHA: Option[String] = None,
  KSIS: Double,
  ANGLES: Double,
  ALPHAS: Option[String] = None,
  Id: Option[String] = None,
  ID_G: Option[String] = None,
  FONCTION: ch.bsisa.hyperbird.model.FONCTIONType,
  CLASSE: Option[String] = None,
  GROUPE: Option[String] = None,
  REMARQUE: Option[String] = None)


case class PIECEu46attributeGroup(ALIAS: String,
  Id: String,
  ID_G: String,
  SYMBOLE: Option[String] = None,
  LONGUEUR_UTILE: Double,
  ANGLE: Double,
  ECHELLE_X: Double,
  ECHELLE_Y: Double,
  ECHELLE_Z: Double)


case class SYMBOLEu46attributeGroup(POS: BigInt,
  NOM: String,
  CONTEXTE: String,
  X: Double,
  Y: Double,
  ALPHA: Option[String] = None,
  ANGLE: Double,
  ECHELLE_X: Double,
  ECHELLE_Y: Double,
  ECHELLE_Z: Double,
  REMARQUE: Option[String] = None)


case class CARTypeu46attributeGroup(NOM: Option[String] = None,
  UNITE: Option[String] = None,
  VALEUR: Option[String] = None)


case class PERSONNETypeu46attributeGroup(Id: Option[String] = None,
  ID_G: Option[String] = None,
  NOM: Option[String] = None,
  GROUPE: Option[String] = None)

