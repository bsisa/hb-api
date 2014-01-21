package ch.bsisa.hyperbird.security

import securesocial.core.{AuthenticationMethod, Identity}

class User(name: String, firstNameParam: String, lastNameParam: String, password: String, emailParam: String) extends Identity {

  /**
   * As seen from class User, the missing signatures are as follows. * For convenience, these
   * are usable as stub implementations.
   */
  def authMethod: AuthenticationMethod = AuthenticationMethod.UserPassword
  def avatarUrl: Option[String] = None
  def email: Option[String] = Option(emailParam)
  def firstName: String = firstNameParam
  def fullName: String = firstName + " " + lastName
  def identityId: securesocial.core.IdentityId = ??? //name
  def lastName: String = lastNameParam
  def oAuth1Info: Option[securesocial.core.OAuth1Info] = None
  def oAuth2Info: Option[securesocial.core.OAuth2Info] = None
  def passwordInfo: Option[securesocial.core.PasswordInfo] = ??? // password

}