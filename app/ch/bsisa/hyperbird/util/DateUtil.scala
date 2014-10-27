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
   * Constant simple date format pattern used for Id/ID_G date and time part.
   */
  private val DateFormatPattern = "yyyyMMddHHmmssSSS"
    
  /**
   * Constant simple date format pattern used for ELFIN.IDENTIFIANT.{DE,A,PAR} 
   * dates.
   */
  private val HbDateFormatPattern = "yyyy-MM-dd"    

  /**
   * Ids formatter
   */
  private val idsFormatter = {
    val sdf = new SimpleDateFormat(DateFormatPattern)
    sdf.setLenient(false)
    sdf
  }
    
  /**
   * Hb dates formatter
   */
  private val hbDateFormatter = { 
    val sdf = new SimpleDateFormat(HbDateFormatPattern)
    sdf.setLenient(false)
    sdf
  }

  /**
   * Returns a date formatter for elfin.Id/ID_G.
   */
  val elfinUniqueIdDateFormat = {
    idsFormatter
  }

  /**
   * Returns a date formatter with pattern: "yyyyMMddHHmmssSSS"
   */
  val elfinIdentifiantDateFormat = {
    idsFormatter
  }
  
  /**
   * Returns a date formatter with pattern: "yyyy-MM-dd"
   */
  val hbDateFormat = {
    hbDateFormatter
  }  
  
  

}