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
import ch.bsisa.hyperbird.dao.ResultNotFoundException
import ch.bsisa.hyperbird.model.IDENTIFIANT
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import ch.bsisa.hyperbird.dao.ExpectedSingleResultException
import securesocial.core.providers.UsernamePasswordProvider

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
  //private var users = Map[String, Identity]()
  private var tokens = Map[String, Token]()

  /**
   * Searches the ELFIN corresponding to the provided IdentityId.userId
   * and returns an Option encapsulating the and Identity created from
   * the ELFIN user information obtained from the database.
   * If no corresponding user is found we catch `ResultNotFoundException` 
   * and return Option `None`.
   * Any other exception will be thrown an managed later.
   */
  override def find(id: IdentityId): Option[Identity] = {
    Logger.debug(s"ExistDbUserService.find: id.userId=${id.userId}...")

    try {
      val futureElfinUser = ElfinDAO.findUser(id.userId)
      getIdentity(futureElfinUser)
    } catch {
      // This is expected in all new user creation cases.
      case ResultNotFoundException(message, throwable) => {
        Logger.info(s"Requested user with id.userId = ${id.userId} was not found: ${message}")
        None
      }
    }
  }

  /**
   * Searches the ELFIN user corresponding to the provided email, providerId pair 
   * and returns an Option encapsulating the and Identity created from
   * the ELFIN user information obtained from the database.
   * 
   * The providerId information is no relevant in the current context as we do always expect 
   * `securesocial.core.providers.UsernamePasswordProvider.UsernamePassword`
   *  
   * If no corresponding user is found we catch `ResultNotFoundException` 
   * and return Option `None`.
   * Any other exception will be thrown an managed later.
   */
  override def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
    Logger.debug(s"ExistDbUserService.findByEmailAndProvider: email=${email}, providerId=${providerId}")

    //    findUser(ElfinDAO.findUserByEmail, email).map(elfin => getIdentity(elfin))
    try {
      val futureElfinUser = ElfinDAO.findUserByEmail(email)
      getIdentity(futureElfinUser)
    } catch {
      // This is expected in all new user creation cases.
      case ResultNotFoundException(message, throwable) => {
        Logger.info(s"Requested user with email = ${email} was not found: ${message}")
        None
      }
    }

  }

  /**
   * Will create non existing user or update existing one.
   */
  override def save(user: Identity): Identity = {
    Logger.debug(s"""ExistDbUserService.save: Identity: ${user.toString}""")
    try {
      val futureElfinUser = ElfinDAO.findUser(user.identityId.userId)
      futureElfinUser.map { elfinUser =>
        // We currently only update the password
        val elfinUserUpdated = ElfinUtil.replaceElfinUserPasswordInfo(elfinUser, user.passwordInfo.get.password)
        ElfinDAO.update(elfinUserUpdated)
        Logger.debug("Updated passwordInfo for user ${user.identityId.userId}")
      }
      getIdentity(futureElfinUser) match {
        case Some(user) => user
        case None => throw SaveUserException(s"""Could not update user for Identity: ${user}""")
      }
    } catch {
      // Regular situation for new users
      case rnf: ResultNotFoundException => {
        // Create a new user in the database
        val futureUpdatedElfin = ElfinDAO.createUser(userName = user.identityId.userId, userPwdInfo = user.passwordInfo.get.password)
        futureUpdatedElfin.map { updatedElfinUser =>
          Logger.info(s"New user ${user.identityId.userId} with elfinId: ${updatedElfinUser.Id} and elfinID_G: ${updatedElfinUser.ID_G} has been created by ExistDbUserService.save.")
        }
        getIdentity(futureUpdatedElfin) match {
          case Some(user) => user
          case None => throw SaveUserException(s"""Could not create new user for Identity: ${user}""")
        }
      }
      // Unknown exceptions 
      case other: Throwable => {
        Logger.error(s"Could neither find nor create user: ${user.identityId.userId}. Exception is: ${other.toString}")
        throw other
      }
    }
  }

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

  /**
   * Builds the Identity corresponding to the provided IdentityId and Future[ELFIN]
   */
  private def getIdentity(futureElfinUser: Future[ELFIN]): Option[Identity] = {

    //    try {
    val futureSocialUserFromFutureElfinUser = futureElfinUser.map { elfinUser =>
      val userId = elfinUser.IDENTIFIANT.get.NOM.get
      val providerId = UsernamePasswordProvider.UsernamePassword
      val identityId = IdentityId(userId, providerId)
      val passwordInfo = elfinUser.IDENTIFIANT.get.ALIAS.get
      val pwdInfo: PasswordInfo = new PasswordInfo(PasswordHasher.BCryptHasher, passwordInfo)
      val userDetailsId = elfinUser.PARTENAIRE.get.USAGER.get.Id.get
      val userDetailsID_G = elfinUser.PARTENAIRE.get.USAGER.get.ID_G.get
      val futureUserDetails = XQueryWSHelper.find(WSQueries.elfinQuery(userDetailsID_G, userDetailsId))
      val futureSocialUser = futureUserDetails.map { userDetails =>
        val email: String = userDetails.CARACTERISTIQUE.get.CAR5.get.VALEUR.get
        // TODO: pull this information from userDetails ELFIN 
        val userFirstName = "Undefined"
        val userLastName = "Undefined"
        val userPhoto = None
        // Only deal with password authentication at the moment
        val authMethod: AuthenticationMethod = AuthenticationMethod.UserPassword
        val socialUser = new SocialUser(identityId, userFirstName, userLastName, identityId.userId, Option(email),
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
  }

  case class SaveUserException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}