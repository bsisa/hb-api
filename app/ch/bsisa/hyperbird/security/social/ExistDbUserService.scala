package ch.bsisa.hyperbird.security.social
import play.api.{ Logger, Application }
import securesocial.core._
import securesocial.core.providers.Token
import securesocial.core.IdentityId
import securesocial.core.providers.utils.PasswordHasher
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.ws.WSQueries
import scala.concurrent.Future
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.security.Role
import play.api.libs.concurrent.Execution.Implicits._
import ch.bsisa.hyperbird.dao.ResultNotFoundException
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import securesocial.core.providers.UsernamePasswordProvider
import play.api.cache.Cache
import play.api.Play.current

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
      Cache.getOrElse[Option[User]](id.userId, ExistDbUserService.USER_CACHE_TTL_SECONDS) {
        val futureElfinUser = ElfinDAO.findUser(id.userId)
        getIdentity(futureElfinUser)
      }
    } catch {
      // This is expected in all new user creation cases.
      case ResultNotFoundException(message, throwable) => {
        Logger.info(s"Requested user with id.userId = ${id.userId} could not be obtained: ${message}")
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
  private def getIdentity(futureElfinUser: Future[ELFIN]): Option[User] = {

    val elfinUser = Await.result[ELFIN](futureElfinUser, Duration(ExistDbUserService.USER_QUERY_MAY_WAIT_MILLISECONDS, MILLISECONDS))
    //val elfinUser = ElapsedTime.time ({ Await.result[ELFIN](futureElfinUser, Duration(ExistDbUserService.USER_QUERY_MAY_WAIT_MILLISECONDS, MILLISECONDS))  }, "elfinUser")    
    
    val elfinUserValidFromStr = elfinUser.IDENTIFIANT.get.DE.get
    val elfinUserValidToStr = elfinUser.IDENTIFIANT.get.A.get
    val elfinUserValidFrom = DateUtil.hbDateFormat.parse(elfinUserValidFromStr)
    val elfinUserValidTo = DateUtil.hbDateFormat.parse(elfinUserValidToStr)
    val elfinUserId = elfinUser.IDENTIFIANT.get.NOM.get
    val providerId = UsernamePasswordProvider.UsernamePassword
    val elfinUserIdentityId = IdentityId(elfinUserId, providerId)
    val elfinUserPasswordInfo = elfinUser.IDENTIFIANT.get.ALIAS.get
    val pwdInfo: PasswordInfo = new PasswordInfo(PasswordHasher.BCryptHasher, elfinUserPasswordInfo)
    val elfinUserDetailsId = elfinUser.PARTENAIRE.get.USAGER.get.Id.get
    val elfinUserDetailsID_G = elfinUser.PARTENAIRE.get.USAGER.get.ID_G.get
    
    // Use recover to provide more details on the reason why obtaining the given elfinUserId failed.
    val futureElfinUserDetails = XQueryWSHelper.find(WSQueries.elfinQuery(elfinUserDetailsID_G, elfinUserDetailsId)) recover {
      case ResultNotFoundException(message, throwable) => 
        val augmentedMessage = s"User ${elfinUserId} details could not be obtained. Elfin of CLASSE ACTOR ${elfinUserDetailsID_G}/${elfinUserDetailsId} could not be found."
        throw ResultNotFoundException(augmentedMessage, throwable)
    }
    
    val elfinUserDetails = Await.result[ELFIN](futureElfinUserDetails, Duration(ExistDbUserService.USER_QUERY_MAY_WAIT_MILLISECONDS, MILLISECONDS))
    //val elfinUserDetails = ElapsedTime.time({ Await.result[ELFIN](futureElfinUserDetails, Duration(ExistDbUserService.USER_QUERY_MAY_WAIT_MILLISECONDS, MILLISECONDS)) },"elfinUserDetails")

    val elfinUserEmail: String = elfinUserDetails.CARACTERISTIQUE.get.CAR5.get.VALEUR.get
    val elfinUserFirstName = elfinUserDetails.IDENTIFIANT.get.NOM.get
    val elfinUserLastName = elfinUserDetails.IDENTIFIANT.get.ALIAS.get      
    
    // A user always has a FRACTION (can contain 0-n L)
    val userRoles = for {
      line <- elfinUser.CARACTERISTIQUE.get.FRACTION.get.L
    } yield {
      val cSeq = line.C.seq
      Role(ID_G = getMixedContent(cSeq(0).mixed), Id = getMixedContent(cSeq(1).mixed), name = getMixedContent(cSeq(2).mixed))
    }
    
    val hbUser = new User(
        identityId = elfinUserIdentityId, 
        firstName = elfinUserFirstName, 
        lastName = elfinUserLastName, 
        fullName = elfinUserFirstName + " " + elfinUserLastName, 
        email = Option(elfinUserEmail), 
        passwordInfo = Some(pwdInfo),
        validFrom = elfinUserValidFrom,
        validTo = elfinUserValidTo,
        rolesParam = Option(userRoles))

    Option(hbUser)
  }

  object ExistDbUserService {
    val USER_QUERY_MAY_WAIT_MILLISECONDS = 10000
    val USER_CACHE_TTL_SECONDS = 600
  }
  
  case class SaveUserException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}