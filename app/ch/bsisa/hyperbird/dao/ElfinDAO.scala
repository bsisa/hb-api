package ch.bsisa.hyperbird.dao

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.xqs.XQConnectionHelper
import ch.bsisa.hyperbird.dao.xqs.XQueryHelper
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.util.ElfinIdGenerator
import scala.concurrent.Future
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.util.ElfinUtil
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import java.util.Date
import java.util.GregorianCalendar
import java.util.Calendar

/**
 * Provides CRUD operations for ELFIN
 *
 * @author Patrick Refondini
 */
object ElfinDAO {

  val logger = Logger("ch.bsisa.hyperbird.dao.ElfinDAO")

  /**
   * Creates XML database ELFIN representation corresponding to the provided ELFIN object
   * into the database.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def create(elfin: ELFIN)(implicit dbConfig: DbConfig) = {
    logger.debug(s"ElfinDAO.create elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    XQueryWSHelper.create(elfin)
  }

  /**
   * Updates XML database ELFIN representation corresponding to the provided ELFIN object.
   * <i>The database API do not provide any feedback on that operation.</i>
   * Note: elfin.ID_G and elfin.Id are mandatory
   */
  def update(elfin: ELFIN)(implicit dbConfig: DbConfig) : Unit = {

    if (elfin.ID_G != "" && elfin.Id != "") {
      logger.debug(s"ElfinDAO.update elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
      val elfinXml = ElfinFormat.toXml(elfin)
      // TODO: Refactor as follow (requires tests)
      // update(elfinXml)
      val updateStatetement =
        s"update replace collection('${dbConfig.databaseName}/${elfin.ID_G}')//ELFIN[@Id='${elfin.Id}'] with ${elfinXml.mkString}"
      executeStatement(updateStatetement)
    } else {
      logger.error(s"ElfinDAO.update elfin.ID_G and elfin.Id are mandatory but: elfin.ID_G = ${elfin.ID_G}, elfin.Id = ${elfin.Id}")
    }
  }

  /**
   * Updates XML database ELFIN representation corresponding to the provided ELFIN as scala.xml.Node
   * <i>The database API do not provide any feedback on that operation.</i>
   * Note: elfin.ID_G and elfin.Id are mandatory
   */
  def update(elfin: scala.xml.Node)(implicit dbConfig: DbConfig) : Unit = {

    val idg: String = (elfin \ "@ID_G").mkString
    val id: String = (elfin \ "@Id").mkString

    if (idg != "" && id != "") {
      logger.debug(s"ElfinDAO.update elfin.ID_G/Id: ${elfin \ "@ID_G"}/${elfin \ "@Id"}")
      val elfinXml = elfin
      val updateStatetement =
        s"update replace collection('${dbConfig.databaseName}/${elfin \ "@ID_G"}')//ELFIN[@Id='${elfin \ "@Id"}'] with ${elfinXml.mkString}"
      executeStatement(updateStatetement)
    } else {
      logger.error(s"ElfinDAO.update elfin.ID_G and elfin.Id are mandatory but: elfin.ID_G = ${idg}, elfin.Id = ${id}")
    }
  }

  /**
   * Deletes XML database ELFIN representation corresponding to the provided ELFIN object.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def delete(elfin: ELFIN)(implicit dbConfig: DbConfig) = {
    logger.debug(s"ElfinDAO.delete elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    XQueryWSHelper.delete(elfin)
  }

  /**
   * Gets new ELFIN instance from catalog for provided CLASSE and assign a new ELFIN.Id to it.
   * Note that this instance does not exist in database.
   * To persist a new ELFIN instance to database use `ElfinDAO.create`.
   */
  def getNewFromCatalogue(classeName: String)(implicit dbConfig: DbConfig, collectionsConfig: CollectionsConfig): Future[ELFIN] = {
    // Use generic find query with catalog collection id and ELFIN@CLASSE parameter 
    val futureElfin = XQueryWSHelper.find(
      WSQueries.filteredCollectionQuery(collectionsConfig.catalogCollectionId, s"//ELFIN[@CLASSE='${classeName}']"))

    // Clone futureElfin[ELFIN] and assign a new generated ELFIN.Id to it
    val futureElfinWithId: Future[ELFIN] = futureElfin.flatMap { elfin => ElfinUtil.assignElfinId(elfin) }
    /* for comprehension equivalent to above one liner.    
    val futureElfinWithId: Future[ELFIN] = for {
      elfin <- futureElfin
      elfinWithId <- ElfinUtil.assignElfinId(elfin)
    } yield {
      elfinWithId
    }
    */
    futureElfinWithId
  }

  /**
   * Creates XML database ELFIN of CLASSE USER representation into the database.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def createUser(userName: String, userPwdInfo: String)(implicit dbConfig: DbConfig): Future[ELFIN] = {
    // CREATE NEW USER IN HB DB ///////////////////////////////////////////////////
    val userClasseName = "USER"

    // Let's set userValidFrom to now 
    val userValidFrom = new Date()

    // Let's set userValidUntil to userValidFrom + a year.
    val userValidUntil = {
      val gregCal = new GregorianCalendar()
      gregCal.setTime(userValidFrom)
      gregCal.setLenient(false)
      gregCal.roll(Calendar.YEAR, 1)
      gregCal.getTime()
    }

    // TODO: replace hardcoded link by correct reference. This is only used for test draft.
    // This will have to be provided at user creation time
    val userPersonId = "G20140207193832484"
    val userPersonID_G = "G10000101010101000"

    val futureElfinUser: Future[ELFIN] = ElfinDAO.getNewFromCatalogue(userClasseName)

    // Update and create new user 
    val futureUpdatedElfin = futureElfinUser.map { elfinUserToUpdate =>

      val updatedElfinUser = ElfinUtil.replaceElfinUserProperties(
        elfinUser = elfinUserToUpdate,
        userName = userName,
        userPwdInfo = userPwdInfo,
        validFrom = userValidFrom,
        validUntil = userValidUntil,
        personId = userPersonId,
        personID_G = userPersonID_G)

      // Update database with new elfin
      ElfinDAO.create(updatedElfinUser)
      // TODO: define a validation to provide feedback whether or not the user has effectively been created.
      logger.debug(s"ExistDbUserService.save: NEW USER with elfinId: ${updatedElfinUser.Id} and elfinID_G: ${updatedElfinUser.ID_G} SHALL HAVE BEEN CREATED TO DATABASE... ")
      updatedElfinUser
    }
    futureUpdatedElfin
  }

  /**
   * Find XML database ELFIN of CLASSE USER for provided user name.
   */
  def findUser(userName: String)(implicit dbConfig: DbConfig, collectionsConfig: CollectionsConfig): Future[ELFIN] = {
    val userClasseName = "USER"
    //val query = s"""//ELFIN%5B@CLASSE=%27${userClasseName}%27 and IDENTIFIANT/NOM=%27${userName}%27%5D"""
    val query = s"""%2F%2FELFIN%5B%40CLASSE%3D%27${userClasseName}%27+and+IDENTIFIANT%2FNOM%3D%27${userName}%27%5D"""

    XQueryWSHelper.find(WSQueries.filteredCollectionQuery(collectionId = collectionsConfig.configurationCollectionId, xpath = query))
  }

  /**
   * Find XML database ELFIN of CLASSE USER for provided email.
   */
  def findUserByEmail(email: String)(implicit dbConfig: DbConfig, collectionsConfig: CollectionsConfig): Future[ELFIN] = {
    XQueryWSHelper.findElfinUserPerEmailQuery(email)
  }

  /**
   * Helper function designed to executes XUpdate statements
   *
   * Note: do not use prepareExpression(statement)
   * it will fail to validate XUpdate instructions.
   * Only valid XQuery syntax seems currently supported.
   */
  private def executeStatement(statement: String)(implicit conf: DbConfig) = {
    val conn = XQConnectionHelper.getConnection()(conf)
    try {
      // Too noisy even while debugging (this is super debug mode...)
      //logger.debug("executeStatement(statement) : " + statement)
      var xqe = conn.createExpression()
      xqe.executeCommand(statement)
    } finally {
      conn.close()
    }
  }

}