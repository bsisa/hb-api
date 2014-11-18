package ch.bsisa.hyperbird.security.social

import securesocial.core.Registry
import securesocial.core.providers.utils.PasswordHasher

/**
 * Helper object which exposes encryption and other security related functions.
 */
object HbSecureService {

  
  /**
   * Make use of SecureSocial securesocial.core.providers.utils.PasswordHasher provided Password hasher.
   * See: https://github.com/jaliss/securesocial/blob/2.1.x/module-code/app/securesocial/controllers/PasswordChange.scala 
   * for example usage. 
   */
  def getPasswordHash(plainTextPassword : String) : String = Registry.hashers.get(PasswordHasher.BCryptHasher) match {
    case Some(hasher) => hasher.hash(plainTextPassword).password
    case _ => throw PasswordHashException("Could not provide password hashing service, please report to system administrator.")
  }
  

    /**
   * PasswordHashException exception class
   */
  case class PasswordHashException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}