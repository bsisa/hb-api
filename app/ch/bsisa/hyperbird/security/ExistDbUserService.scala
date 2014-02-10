package ch.bsisa.hyperbird.security

import play.api.{ Logger, Application }
import securesocial.core._
import securesocial.core.providers.Token
import securesocial.core.IdentityId
import securesocial.core.providers.utils.PasswordHasher
import ch.bsisa.hyperbird.dao.xqs.XQConnectionHelper
import org.mindrot.jbcrypt.BCrypt

/**
 * SecureSocial UserService implementation targeted at eXist database.
 *
 * @author Patrick Refondini
 */
class ExistDbUserService(application: Application) extends UserServicePlugin(application) {
  private var users = Map[String, Identity]()
  private var tokens = Map[String, Token]()

  def find(id: IdentityId): Option[Identity] = {
    Logger.debug(s"find: id.userId=${id.userId}...")
    // We only want to deal with password authentication
    val authMethod: AuthenticationMethod = AuthenticationMethod.UserPassword
    val password = "test"
    val pwdInfo: PasswordInfo = new PasswordInfo(PasswordHasher.BCryptHasher, BCrypt.hashpw(password, BCrypt.gensalt(10)))

    if (Logger.isDebugEnabled) {
      Logger.debug(s"IdentityId.providerId=${id.providerId}, IdentityId.userId=${id.userId}")

      Logger.debug("users = %s".format(users))
    }
    //users.get(id.userId + id.providerId)

    if (id.userId == "pat") {
      val userName = "Patrick"
      val userEmail = "refon@pobox.com"
      val firstName = "Patrick"
      val lastName = "Refondini"
      val socialUser = new SocialUser(
        id, firstName, lastName, userName, Option(userEmail),
        None, authMethod, None, None, Some(pwdInfo))
      Option(socialUser)
    } else {
      None
    }
    //    val user = User.findByUserId(userId);
    //    user match {
    //      case Some(user) => {
    //        val socialUser = new SocialUser(userId, null, null, user.name, Option(user.email), Option(user.photo), AuthenticationMethod("userPassword"), null, null, Some(PasswordInfo(PasswordHasher.BCryptHasher, BCrypt.hashpw(user.password, BCrypt.gensalt(10)))))
    //        Option(socialUser)
    //      }
    //      case None => {
    //         None
    //      }
    //    }

  }

  // As of : http://stackoverflow.com/questions/16124184/play-framework-securesocial-userpass-implementation
  //  def find(userId: UserId): Option[Identity] = {
  //    val user = User.findByUserId(userId);
  //    user match {
  //      case Some(user) => {
  //        val socialUser = new SocialUser(userId, null, null, user.name, Option(user.email), Option(user.photo), AuthenticationMethod("userPassword"), null, null, Some(PasswordInfo(PasswordHasher.BCryptHasher, user.password)))
  //        Option(socialUser)
  //      }
  //      case None => {
  //        None
  //      }
  //    }
  //  }

  def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
    Logger.debug(s"findByEmailAndProvider: email=${email}, providerId=${providerId}")
    if (Logger.isDebugEnabled) {
      Logger.debug("users = %s".format(users))
    }
    users.values.find(u => u.email.map(e => e == email && u.identityId.providerId == providerId).getOrElse(false))
  }

  def save(user: Identity): Identity = {
    Logger.debug(s"save: Identity...")
    users = users + (user.identityId.userId + user.identityId.providerId -> user)
    // this sample returns the same user object, but you could return an instance of your own class
    // here as long as it implements the Identity trait. This will allow you to use your own class in the protected
    // actions and event callbacks. The same goes for the find(id: IdentityId) method.
    user
  }

  // Check: 
  // * http://stackoverflow.com/questions/16124184/play-framework-securesocial-userpass-implementation
  // * http://www.shrikar.com/blog/2013/10/26/playframework-securesocial-and-mongodb

  //  def save(user: Identity): Identity = {
  //    user.id.providerId match {
  //      case "facebook" => {
  //
  //      }
  //      case "google" => {
  //
  //      }
  //      case "twitter" => {
  //      }
  //
  //      case "userpass" => {
  //        val eUser = User.findByEmail(user.id.id) match {
  //          case Some(eUser) => {
  //            //Existing User - update only
  //          }
  //          case None => {
  //            val appUser: User = new User(NotAssigned, "student", user.id.providerId, user.fullName, user.id.id, user.passwordInfo.get.password, null, null, null, null, null, "active")
  //            User.create(appUser)
  //          }
  //        }
  //      }
  //    }
  //    user
  //  }

  //  val socialUser = new SocialUser(
  //    userId,
  //    null,
  //    null,
  //    user.name,
  //    Option(user.email),
  //    Option(user.photo),
  //    AuthenticationMethod("userPassword"),
  //    null,
  //    null,
  //    Some(
  //      PasswordInfo(
  //        PasswordHasher.BCryptHasher,
  //        BCrypt.hashpw(
  //          "password",
  //          BCrypt.gensalt(10)))))

  def save(token: Token) {
    tokens += (token.uuid -> token)
  }

  def findToken(token: String): Option[Token] = {
    tokens.get(token)
  }

  def deleteToken(uuid: String) {
    tokens -= uuid
  }

  def deleteTokens() {
    tokens = Map()
  }

  def deleteExpiredTokens() {
    tokens = tokens.filter(!_._2.isExpired)
  }
}