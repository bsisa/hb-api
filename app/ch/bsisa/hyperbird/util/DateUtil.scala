package ch.bsisa.hyperbird.util

import java.text.SimpleDateFormat
/**
 * Utility object aimed at centralising concerns with text to Date objects
 * used in geoXml.xsd related fields conversions.
 * Several of them contain dates without having XSD date definition but
 * simply normalised text.
 *
 * <i>Note: By default never ever let a java.text.DateFormat be lenient,
 * this is a terrible default behaviour.</i>
 *
 */
object DateUtil {

  /**
   * Constant simple date format pattern used for Id/ID_G date and time part
   * as well as for ELFIN.IDENTIFIANT.{DE,A,PAR} dates. This may be splitted
   * into different patterns in the future thus the private keyword.
   */
  private val DateFormatPattern = "yyyyMMddHHmmssSSS"

  /**
   * Private currently unique formatter
   */
  private val formatter = {
    val sdf = new SimpleDateFormat(DateFormatPattern)
    sdf.setLenient(false)
    sdf
  }

  /**
   * Returns a date formatter for elfin.Id/ID_G.
   *
   * <i>Note: By default never ever let a java.text.DateFormat be lenient,
   * this is a terrible default behaviour.</i>
   */
  val elfinUniqueIdDateFormat = {
    formatter
  }

  /**
   * Returns a date formatter for ELFIN.IDENTIFIANT.{DE,A,PAR} dates
   *
   *
   */
  val elfinIdentifiantDateFormat = {
    formatter
  }

}