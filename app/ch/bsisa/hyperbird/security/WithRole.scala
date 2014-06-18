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

case class WithRole(role: String) extends Authorization {

  /**
   * securesocial.core.Authorization trait method. Pending till 
   */
  def isAuthorized(user: Identity) = {

    val futureElfinUser = ElfinDAO.findUser(user.identityId.userId)

    // Wait for the result of the last future layer
    val elfinUser = Await.result[ELFIN](futureElfinUser, Duration(8000, MILLISECONDS))

    val userRoles = for { 
      line <- elfinUser.CARACTERISTIQUE.get.FRACTION.get.L      
      } yield {
        val cSeq = line.C.seq
        Role(ID_G = getMixedContent(cSeq(0).mixed) , Id = getMixedContent(cSeq(1).mixed), name = getMixedContent(cSeq(2).mixed))
      }
      Logger.debug(s"User ${user.identityId.userId} has roles: ${userRoles}");
      userRoles.exists(userRole => userRole.name == role)
  }  

}

