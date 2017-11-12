package ch.bsisa.hyperbird.security.social

import ch.bsisa.hyperbird.ApiConfig
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model._

import play.api.libs.concurrent.Execution.Implicits._
import play.api.Logger

/**
 * Contains methods to check user access rights given ELFIN CLASSE
 * information
 * 
 * TODO: solve type duplicate method due to useless type parametrisation mismatch. 
 */
// TODO: evaluate whether to adapt once SecureSocial 2.1.4 is out provided it includes 
// request access similar to https://github.com/mohiva/play-silhouette/pull/152 
// Note: elfinClasse could be added as request header field to avoid computation.
case object WithManagerEditRight {

  val HTTP_HEADER_DATA_MANAGER_ACCESS_RIGHTS_CREATE_UPDATE = "HyperBird-Data-Manager-Access-Rights-Create-Update"
  val HTTP_HEADER_DATA_MANAGER_ACCESS_RIGHTS_READ = "HyperBird-Data-Manager-Access-Rights-Read"    
  
  /**
   * Return true if the provided elfin has manager rights corresponding to HTTP header 
   * authorised data manager rights or if optional data manager rights in turn off. 
   * Otherwise throws as WithManagerEditRightException
   * TODO: Refactor to hasAuthorizationLimits(...) : Option[AutorisationLimit] 
   */
  def isAuthorizedJs(elfin: ELFIN, request: securesocial.core.SecuredRequest[play.api.libs.json.JsValue])(implicit apiConfig: ApiConfig): Boolean = {
    
    val dataManagerRight: String = request.headers.get(HTTP_HEADER_DATA_MANAGER_ACCESS_RIGHTS_CREATE_UPDATE).getOrElse("")
    
    val authorised = if (apiConfig.dataManagerSecurityEnabled) {
      elfin.IDENTIFIANT match {
        case Some(identifiant) => identifiant.GER match {
          case Some(ger) =>
            Logger.error(s"IDENTIFIANT/GER '$ger' information comparing to HTTP_HEADER_DATA_MANAGER_ACCESS_RIGHTS_CREATE_UPDATE: '$dataManagerRight'")
            ger.equals(dataManagerRight) // Some(AutorisationLimit.INSUFFICIENT_ACCESS_RIGHTS) or None
          case None      => 
            // Some(AutorisationLimit.MISSING_GER_INFO_ACCESS_RIGHTS)
            Logger.error(s"IDENTIFIANT/GER information missing for ELFIN IG_G/CLASSE/Id = ${elfin.ID_G}/${elfin.CLASSE}/${elfin.Id}") 
            false
        }
        case None =>
          // Some(AutorisationLimit.MISSING_IDENTIFIANT_INFO_ACCESS_RIGHTS)
          Logger.error(s"IDENTIFIANT information missing for ELFIN IG_G/CLASSE/Id = ${elfin.ID_G}/${elfin.CLASSE}/${elfin.Id}")
          false
      }
    } else {
      // None
      true
    }

    // TODO: Remove that ugly piece of code
    if (authorised) {
      authorised
    } else {
      throw WithManagerEditRightException(s"Insufficiant mandatory data manager rights - '$dataManagerRight' does not match GER '${elfin.IDENTIFIANT.get.GER.get}'")
    }

  }
  
  /**
   * Return true if the provided elfin has manager rights corresponding to HTTP header 
   * authorised data manager rights or if optional data manager rights in turn off. 
   * Otherwise throws as WithManagerEditRightException
   * TODO: Refactor to hasAuthorizationLimits(...) : Option[AutorisationLimit] check above function for implementation guidelines.
   */  
  def isAuthorizedAny(elfin: ELFIN, request: securesocial.core.SecuredRequest[play.api.mvc.AnyContent])(implicit apiConfig: ApiConfig): Boolean = {    
    
    val dataManagerRight: String = request.headers.get(HTTP_HEADER_DATA_MANAGER_ACCESS_RIGHTS_CREATE_UPDATE).getOrElse("")
    
    val authorised = if (apiConfig.dataManagerSecurityEnabled) {
      elfin.IDENTIFIANT match {
        case Some(identifiant) => identifiant.GER match {
          case Some(ger) =>
            Logger.error(s"IDENTIFIANT/GER '$ger' information comparing to HTTP_HEADER_DATA_MANAGER_ACCESS_RIGHTS_CREATE_UPDATE: '$dataManagerRight'")
            ger.equals(dataManagerRight)
          case None      => 
            Logger.error(s"IDENTIFIANT/GER information missing for ELFIN IG_G/CLASSE/Id = ${elfin.ID_G}/${elfin.CLASSE}/${elfin.Id}") 
            false
        }
        case None => 
          Logger.error(s"IDENTIFIANT information missing for ELFIN IG_G/CLASSE/Id = ${elfin.ID_G}/${elfin.CLASSE}/${elfin.Id}")
          false
      }
    } else {
      true
    }

    if (authorised) {
      authorised
    } else {
      throw WithManagerEditRightException("Insufficiant mandatory data manager rights")
    }

  }  

}

/**
 * WithManagerEditRightException exception class
 */
case class WithManagerEditRightException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

