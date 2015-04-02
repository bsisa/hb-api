package ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model.ELFIN

import play.libs.Akka
import play.api.Logger

import akka.actor.Props
import akka.pattern.ask

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.matching.Regex

import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.Date

/**
 * Basic generator
 * TODO: review existing implementation use actor for thread safe counter
 */
object ElfinIdGenerator {

  val idActor = Akka.system.actorOf(Props[IdActor], name = "idactor")

  /**
   * Constant RegEx match group name used for Id/ID_G first letter match
   */
  val GmatchGroupName = "gMatch"
  /**
   * Constant RegEx match group name used for Id/ID_G date and time part match
   */
  val DateMatchGroupName = "dateMatch"

  /**
   * Creates a new unique ELFIN.Id upon each call.
   */
  def getNewElfinId(): Future[String] = {
    val futureId: Future[String] = (idActor.ask("new id please")(5 seconds)).mapTo[String]
    futureId
  }

  def getElfinFileName(elfin: ELFIN): String = {

    validateElfinIdandID_G(elfin)

    val elfinId = elfin.Id
    val elfinCLASSE = elfin.CLASSE
    elfinCLASSE + elfinId + ".xml"
  }

  /**
   * Returns nothing if this `elfin` `{elfin.Id, elfin.ID_G}` are valid.
   *
   * Should throw ElfinUniqueIdValidationException otherwise.
   */
  def validateElfinIdandID_G(elfin: ELFIN): Unit = {
    try { validateElfinUniqueIdentifier(elfin.Id) }
    catch {
      case pe: ParseException => throw new ElfinUniqueIdValidationException(s"ELFIN.Id ${elfin.Id} date part is not a valid date.")
      case e: Throwable => throw new ElfinUniqueIdValidationException(s"ELFIN.Id ${elfin.Id} validation failed with cause: ${e.getMessage()}")
    }
    try { validateElfinUniqueIdentifier(elfin.ID_G) }
    catch {
      case pe: ParseException => throw new ElfinUniqueIdValidationException(s"ELFIN.ID_G ${elfin.ID_G} date part is not a valid date.")
      case e: Throwable => throw new ElfinUniqueIdValidationException(s"ELFIN.ID_G ${elfin.ID_G} validation failed with cause: ${e.getMessage()}")
    }
  }

  /**
   * Provide common string validation for both ELFIN.Id and ELFIN.ID_G
   *
   * Example valid uniqueId values:
   * validateElfinUniqueIdentifier("G20140210140946909")
   * validateElfinUniqueIdentifier("G20040930101030005")
   * validateElfinUniqueIdentifier("G20040203114894000")
   */
  def validateElfinUniqueIdentifier(uniqueId: String): Unit = {

    val validId = new Regex("""([G]{1})(\d{17})""", GmatchGroupName, DateMatchGroupName)
    val uniqueIdMatchIt = validId findAllIn uniqueId

    // Check there is a match
    if (uniqueIdMatchIt.hasNext) {
      val gMatch = uniqueIdMatchIt.group(GmatchGroupName)
      val dateMatch = uniqueIdMatchIt.group(DateMatchGroupName)

      println(s"${GmatchGroupName} value = ${gMatch}")
      println(s"${DateMatchGroupName} value = ${dateMatch}")

      /* NOTE: This validation assertion does not hold true with 
       * current existing values and is currently dropped.
       * 
      // Make sure the date part is a valid date
      try { elfinUniqueIdDateFormat.parse(dateMatch) }
      catch {
        case pe: ParseException => throw new ElfinUniqueIdValidationException(s"${uniqueId} value failed validation with ${pe.getMessage}")
      }
       */
      // There must be only a single match
      uniqueIdMatchIt.next
      if (uniqueIdMatchIt.hasNext) {
        throw new ElfinUniqueIdValidationException(
          s"""More than a single match for unique Id/ID_G: ${uniqueId} with expected format (regexp): ([G]{1})(d{17}).""")
      }
    } else {
      // throw new exception
      throw new ElfinUniqueIdValidationException(
        s"""Unique Id/ID_G: ${uniqueId} does not match the expected format (regexp): ([G]{1})(d{17}).""")
    }
  }

  /**
   * Elfin unique Id `(ELFIN.Id and ELFIN.ID_G)` validation exception class
   */
  case class ElfinUniqueIdValidationException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

}