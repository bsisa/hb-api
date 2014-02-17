package ch.bsisa.hyperbird.security

import java.util.Calendar
import java.util.Date
import java.util.GregorianCalendar
import play.api.{ Logger, Application }
import securesocial.core._
import securesocial.core.providers.Token
import securesocial.core.IdentityId
import securesocial.core.providers.utils.PasswordHasher
import ch.bsisa.hyperbird.dao.xqs.XQConnectionHelper
import org.mindrot.jbcrypt.BCrypt
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.ws.WSQueries
import scala.concurrent.Future
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.Implicits._
import play.api.libs.concurrent.Execution.Implicits._
import ch.bsisa.hyperbird.dao.ResultNotFound
import ch.bsisa.hyperbird.model.IDENTIFIANT
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

/**
 * SecureSocial UserService implementation targeted at eXist database.
 *
 *
 * == Password hashing with BCrypt ==
 *
 * Javadoc information using BCrypt copied and adapted from original
 * org.mindrot.jbcrypt.BCrypt Javadoc available at:
 *
 * [[http://www.mindrot.org/files/jBCrypt/jBCrypt-0.2-doc/ jBCrypt-0.2-doc]]
 *
 * Note that the current lastest version is 0.3 but online Javadoc link is only available for 0.2.
 *
 * === Usage ===
 *
 * ==== Hash a password for the first time ====
 *
 * Call `hashpw` method with a random salt:
 * {{{
 * String pw_hash = BCrypt.hashpw(plain_password, BCrypt.gensalt());
 * }}}
 *
 * ==== Check plain text password matches previously hashed one ====
 *
 * Use `checkpw` method:
 * {{{
 * if (BCrypt.checkpw(candidate_password, stored_hash))
 * System.out.println("It matches");
 * else
 * System.out.println("It does not match");
 * }}}
 *
 * ==== Computational complexity of the hashing ====
 *
 * `gensalt()` method takes an optional parameter (log_rounds) that determines the computational complexity of the hashing:
 * {{{
 * String strong_salt = BCrypt.gensalt(10)
 * String stronger_salt = BCrypt.gensalt(12)
 * }}}
 * The amount of work increases exponentially (2**log_rounds), so each increment is twice as much work. The default log_rounds is 10, and the valid range is 4 to 31.
 *
 *
 * @author Patrick Refondini
 */
class ExistDbUserService(application: Application) extends UserServicePlugin(application) {
  private var users = Map[String, Identity]()
  private var tokens = Map[String, Token]()

  override def find(id: IdentityId): Option[Identity] = {
    Logger.debug(s"ExistDbUserService.find: id.userId=${id.userId}...")

    // We only want to deal with password authentication at the moment
    val authMethod: AuthenticationMethod = AuthenticationMethod.UserPassword

    val userFirstName = "Undefined"
    val userLastName = "Undefined"
    val userPhoto = None
    try {
      val futureElfinUser = ElfinDAO.findUser(id.userId)
      val futureSocialUserFromFutureElfinUser = futureElfinUser.map { elfinUser =>
        val passwordInfo = elfinUser.IDENTIFIANT.get.ALIAS.get
        val pwdInfo: PasswordInfo = new PasswordInfo(PasswordHasher.BCryptHasher, passwordInfo)
        val userDetailsId = elfinUser.PARTENAIRE.get.USAGER.get.Id.get
        val userDetailsID_G = elfinUser.PARTENAIRE.get.USAGER.get.ID_G.get
        val futureUserDetails = XQueryWSHelper.find(WSQueries.elfinQuery(userDetailsID_G, userDetailsId))
        val futureSocialUser = futureUserDetails.map { userDetails =>
          val email: String = userDetails.CARACTERISTIQUE.get.CAR5.get.VALEUR.get
          val socialUser = new SocialUser(id, userFirstName, userLastName, id.userId, Option(email),
            userPhoto, authMethod, None, None, Some(pwdInfo))
          Option(socialUser)
        }
        futureSocialUser
      }
      // Peal one future layer out of two
      val futureSocialUser = for {
        futureSocialUser <- futureSocialUserFromFutureElfinUser
        socialUser <- futureSocialUser
      } yield socialUser
      // Wait for the result of the last future layer
      Await.result[Option[securesocial.core.SocialUser]](futureSocialUser, Duration(8000, MILLISECONDS))
    } catch {
      case rnf: ResultNotFound => None
    }

    // Find user details from eXist database ELFIN objects matching 
    // (username :String, provider :String) where provider information is  
    // not required for AuthenticationMethod.UserPassword specific case
    //    val userName = "pat"
    //    val userFirstName = "Patrick"
    //    val userLastName = "Refondini"
    //    val userEmail = "refon@pobox.com"
    //    val userPhoto = None

    // Get password hash from database
    //    val password = "test"
    //    val hashedPassword = BCrypt.hashpw(password, BCrypt.gensalt(12))

    //    val pwdInfo: PasswordInfo = new PasswordInfo(PasswordHasher.BCryptHasher, hashedPassword)
    //
    //    if (Logger.isDebugEnabled) {
    //      Logger.debug(s"IdentityId.providerId=${id.providerId}, IdentityId.userId=${id.userId}")
    //      Logger.debug(s"hashedPassword=${hashedPassword}")
    //      Logger.debug("users = %s".format(users))
    //    }

    //users.get(id.userId + id.providerId)

    //    if (id.userId == userName) {
    //      val socialUser = new SocialUser(id, userFirstName, userLastName, userName, Option(userEmail),
    //        userPhoto, authMethod, None, None, Some(pwdInfo))
    //      Option(socialUser)
    //    } else {
    //      None
    //    }
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

  override def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
    Logger.debug(s"ExistDbUserService.findByEmailAndProvider: email=${email}, providerId=${providerId}")
    // FIND HB USER ///////////////////////////////////////////////////
    val futureUserElfin = XQueryWSHelper.findElfinUserPerEmailQuery(email)

    futureUserElfin.map { userElfin =>
      Logger.debug(s"""=======================================================
    	    userElfin: ${userElfin} 
    	=======================================================""")
    }
    // FIND HB USER ///////////////////////////////////////////////////    

    if (Logger.isDebugEnabled) {
      Logger.debug("users = %s".format(users))
    }
    users.values.find(u => u.email.map(e => e == email && u.identityId.providerId == providerId).getOrElse(false))
  }

  override def save(user: Identity): Identity = {

    Logger.debug(s"""ExistDbUserService.save: Identity:
  user.identityId.providerId=${user.identityId.providerId}
  user.identityId.userId=${user.identityId.userId}
  user.email=${user.email.getOrElse("No email!")}
  user.passwordInfo.get.password=${user.passwordInfo.get.password}
  user.passwordInfo.get.hasher=${user.passwordInfo.get.hasher.toString()}
  user.avatarUrl=${user.avatarUrl.getOrElse("NoAvatarURL")}
  user.passwordInfo.hashCode()= ${user.passwordInfo.hashCode()}""")

    // Create a new user in the database
    val futureUpdatedElfin = ElfinDAO.createUser(userName = user.identityId.userId, userPwdInfo = user.passwordInfo.get.password)

    futureUpdatedElfin.map { updatedElfinUser =>
      // TODO: define a validation to provide feedback whether or not the user has effectively been created.
      Logger.debug(s"ExistDbUserService.save: NEW USER with elfinId: ${updatedElfinUser.Id} and elfinID_G: ${updatedElfinUser.ID_G} SHALL HAVE BEEN CREATED TO DATABASE... ")
    }

//    users = users + (user.identityId.userId + user.identityId.providerId -> user)
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

  override def save(token: Token) {
    Logger.debug("ExistDbUserService.save(token)")
    tokens += (token.uuid -> token)
  }

  override def findToken(token: String): Option[Token] = {
    Logger.debug("ExistDbUserService.findToken(token)")
    tokens.get(token)
  }

  override def deleteToken(uuid: String) {
    Logger.debug("ExistDbUserService.deleteToken(token)")
    tokens -= uuid
  }

  // TODO: make sure this is used otherwise remove it. 
  def deleteTokens() {
    Logger.debug("ExistDbUserService.deleteTokens()")
    tokens = Map()
  }

  override def deleteExpiredTokens() {
    Logger.debug("ExistDbUserService.deleteExpiredTokens()")
    tokens = tokens.filter(!_._2.isExpired)
  }

}