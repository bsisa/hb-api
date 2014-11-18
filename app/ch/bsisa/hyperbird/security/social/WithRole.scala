package ch.bsisa.hyperbird.security.social

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.security.Role
import securesocial.core.Authorization
import securesocial.core.Identity
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

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

