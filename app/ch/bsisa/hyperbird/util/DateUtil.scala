package ch.bsisa.hyperbird.util

import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Date
/**
 * Utility object aimed at centralising concerns with text to Date objects
 * used in geoXml.xsd related fields conversions.
 * Several of them contain dates without having XSD date definition but
 * simply normalised text.
 *
 * <i>Note: By default never ever let a java.text.DateFormat be lenient,
 * this is a terrible default behaviour.</i>
 *
 * For date patterns check: http://docs.oracle.com/javase/6/docs/api/java/text/SimpleDateFormat.html
 * 
 */
object DateUtil {
  
  /**
   * Constant simple date format pattern used for Id/ID_G date and time part.
   */
  private val DateFormatPattern = "yyyyMMddHHmmssSSS"

  /**
   * Constant for ISO date pattern with time zone
   */
  private val IsoDateFromatPatternWithTz = "yyyy-MM-dd'T'HH:mm:ssZ"

  /**
   * Constant for ISO date pattern without time zone. (Timezone like +01:00 are not supported only +0100)
   * See: https://community.oracle.com/thread/2043704
   */    
  private val IsoDateFromatPatternWithoutTz = "yyyy-MM-dd'T'HH:mm:ss"
    
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
   * Hb dates formatter
   */
  private val isoDateFormatterWithTz = { 
    val sdf = new SimpleDateFormat(IsoDateFromatPatternWithTz)
    sdf.setLenient(false)
    sdf
  }  

  /**
   * Hb dates formatter
   */
  private val isoDateFormatterWithoutTz = { 
    val sdf = new SimpleDateFormat(IsoDateFromatPatternWithoutTz)
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
   * Returns a date formatter with pattern: `yyyyMMddHHmmssSSS`
   */
  val elfinIdentifiantDateFormat = {
    idsFormatter
  }
  
  /**
   * Returns a date formatter with pattern: `yyyy-MM-dd`
   */
  val hbDateFormat = {
    hbDateFormatter
  }  
  
  /**
   * Returns a date formatter with pattern: `yyyyMMdd'T'HH:mm:ssZ`
   */
  val isoWithTzDateFormat = {
    isoDateFormatterWithTz
  }  
  
    /**
   * Returns a date formatter with pattern: `yyyyMMdd'T'HH:mm:ssZ`
   */
  val isoWithoutTzDateFormat = {
    isoDateFormatterWithoutTz
  }  

  /**
   * Returns a triplet (hour24:Int, minute:Int, second:Int) for the given date
   */
  def getHourMinuteSecond(date: Date): (Int, Int, Int) = {
    val calendar = Calendar.getInstance()
    calendar.setTime(date)
    // Hour in 24h format
    (calendar.get(Calendar.HOUR_OF_DAY), calendar.get(Calendar.MINUTE), calendar.get(Calendar.SECOND))
  }  
  
}