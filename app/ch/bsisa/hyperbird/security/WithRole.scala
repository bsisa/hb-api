package ch.bsisa.hyperbird.security

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.dao.DbConfig
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.ELFIN

import securesocial.core.Authorization
import securesocial.core.Identity

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import java.util.Date

case class WithRole(role: String) extends Authorization {

  def isAuthorized(user: Identity) = {

    val futureElfinUser = ElfinDAO.findUser(user.identityId.userId)

    // Wait for the result of the last future layer
    val elfinUser = Await.result[ELFIN](futureElfinUser, Duration(8000, MILLISECONDS))

    Logger.debug(s"WithRole(${role}) called at ${new Date} and actual user.role = ..." )
    Logger.debug(s"elfinUser.IDENTIFIANT.get.NOM.get = ${elfinUser.IDENTIFIANT.get.NOM.get}" )
    
    elfinUser.IDENTIFIANT.get.NOM.get == "refon"
      
  }

}

