package ch.bsisa.hyperbird.security

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.dao.DbConfig
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import securesocial.core.Authorization
import securesocial.core.Identity
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import java.util.Date
import ch.bsisa.hyperbird.model.CARACTERISTIQUE
import ch.bsisa.hyperbird.model.MATRICEType
import ch.bsisa.hyperbird.model.format.ElfinFormat

/**
 * Contains methods to check user access rights given ELFIN CLASSE 
 * information
 */
// TODO: evaluate whether to adapt once SecureSocial 2.1.4 is out provided it includes 
// request access similar to https://github.com/mohiva/play-silhouette/pull/152 
// Note: elfinClasse could be added as request header field to avoid computation.
case object WithClasseEditRight {

  /**
   * Return true if the provided user has edit rights for given ELFIN CLASSE
   * otherwise throws as WithClasseEditRightException
   */
  def isAuthorized(user: User, elfinClasse: String) : Boolean = {

    val authorised = user.roles match {
      case Some(roles) => roles.exists(role => role.name == elfinClasse)
      case None => false
    }

    if (authorised){
      authorised
    } else {
      throw WithClasseEditRightException("Insufficiant rights, missing mandatory role : " + elfinClasse )
    }
  }

}

/**
 * WithClasseEditRightException exception class
 */
case class WithClasseEditRightException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

