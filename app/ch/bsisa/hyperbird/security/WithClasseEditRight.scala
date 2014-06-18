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
// TODO: evaluate wether to adapt once SecureSocial 2.1.4 is out provided it includes 
// request access similar to https://github.com/mohiva/play-silhouette/pull/152 
// Note: elfinClasse could be added as request header field to avoid computation.
case object WithClasseEditRight {

  /**
   * Return true if the provided user has edit rights for given ELFIN CLASSE
   * otherwise throws as WithClasseEditRightException
   */
  def isAuthorized(user: Identity, elfinClasse: String) : Boolean = {

    val futureElfinUser = ElfinDAO.findUser(user.identityId.userId)

    // Wait for the result of the last future layer
    val elfinUser = Await.result[ELFIN](futureElfinUser, Duration(8000, MILLISECONDS))

    // We can assume a user always has a FRACTION
    val userRoles = for {
      line <- elfinUser.CARACTERISTIQUE.get.FRACTION.get.L
    } yield {
      val cSeq = line.C.seq
      Role(ID_G = getMixedContent(cSeq(0).mixed), Id = getMixedContent(cSeq(1).mixed), name = getMixedContent(cSeq(2).mixed))
    }
    
    val authorised = userRoles.exists(userRole => userRole.name == elfinClasse)
    
    Logger.debug(s"User ${user.identityId.userId} has roles: ${userRoles}. Checking elfinClasse: ${elfinClasse} authorised: ${authorised}");

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

