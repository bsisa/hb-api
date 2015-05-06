package ch.bsisa.hyperbird.util

import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Date
import java.text.DateFormat
import org.joda.time.Seconds
import org.joda.time.DateTime
/**
 * 1) Centralises geoXml.xsd text to Date conversions. Indeed date semantic
 * is not defined as XSD date definition but simply as normalised text.
 *
 * 2) General date and time computations relying on org.joda.time library.
 *
 * <b>Note i) : By default never ever let a java.text.DateFormat be lenient,
 * this is a terrible default behaviour.</b>
 *
 * For date patterns check: http://docs.oracle.com/javase/6/docs/api/java/text/SimpleDateFormat.html
 *
 * <b>Note ii) : DateFormat/SimpleDateFormat must be created for each call otherwise it will
 * create concurrency access problem. As documented in Javadoc in Synchronization section:
 *
 * <i>Date formats are not synchronized. It is recommended to create separate format instances for each thread.
 * If multiple threads access a format concurrently, it must be synchronized externally.</i>
 * </b>
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
   *
   */
  private def idsFormatter: SimpleDateFormat = {
    val sdf = new SimpleDateFormat(DateFormatPattern)
    sdf.setLenient(false)
    sdf
  }

  /**
   * Hb dates formatter
   */
  private def hbDateFormatter: SimpleDateFormat = {
    val sdf = new SimpleDateFormat(HbDateFormatPattern)
    sdf.setLenient(false)
    sdf
  }

  /**
   * Returns a date formatter for elfin.Id/ID_G.
   */
  def elfinUniqueIdDateFormat = {
    idsFormatter
  }

  /**
   * Returns a date formatter with pattern: `yyyyMMddHHmmssSSS`
   */
  def elfinIdentifiantDateFormat = {
    idsFormatter
  }

  /**
   * Returns a date formatter with pattern: `yyyy-MM-dd`
   */
  def hbDateFormat = {
    hbDateFormatter
  }

  /**
   * Returns a date formatter with pattern: `yyyyMMdd'T'HH:mm:ssZ`
   */
  def getIsoDateFormatterWithTz: DateFormat = {
    val sdf = new SimpleDateFormat(IsoDateFromatPatternWithTz)
    sdf.setLenient(false)
    sdf
  }

  /**
   * Returns a date formatter with pattern: `yyyyMMdd'T'HH:mm:ss`
   */
  def getIsoDateFormatterWithoutTz: DateFormat = {
    val sdf = new SimpleDateFormat(IsoDateFromatPatternWithoutTz)
    sdf.setLenient(false)
    sdf
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

  
  /**
   * Computes the number of seconds from now till `hour` `minute` for the next 24 hours
   */
  def firstExecutionFromNowInSeconds(hour: Int = 0, minute: Int = 0, second: Int = 0, millisecond: Int = 0): Int = {
    Seconds.secondsBetween(
      new DateTime(),
      nextExecutionTime(hour, minute, second, millisecond)).getSeconds()
  }

  
  /**
   * Computes the next DateTime corresponding to `hour` `minute` `second` `millisecond` for the next 24 hours
   */
  def nextExecutionTime(hour: Int, minute: Int = 0, second: Int = 0, millisecond: Int = 0) : DateTime = {
    
    val nextExecutionTime = new DateTime()
      .withHourOfDay(hour)
      .withMinuteOfHour(minute)
      .withSecondOfMinute(second)
      .withMillisOfSecond(millisecond)

    if (nextExecutionTime.isBeforeNow()) {
      nextExecutionTime.plusHours(24)
    } else {
      nextExecutionTime
    }
  }
  
  /**
   * The time string precision will be interpreted as:
   *
   * ""    			=>  0h 0min  0sec 0millis
   * "14"  			=> 14h 0min  0sec 0millis
   * "14:01" 		=> 14h 1min  0sec 0millis
   * "14:01:23" 	=> 14h 1min 23sec 0millis
   * "14:01:23:2" 	=> 14h 1min 23sec 2millis
   *
   * Return type is an optional tuple4 as :  (hour : Option[Int], minute : Option[Int], second : Option[Int], millisecond : Option[Int])
   * hour must be between 0-23
   * minute must be between 0-59
   * second must be between 0-59
   * millesecond must be between 0-999
   * 
   * Only valid parameter will be taken into account from left to right till an invalid parameter is reached or no more parameter is found:
   * i.e.:  "14:7777:23:2" 	=> 14h 0min 0sec 0millis
   * 
   * Any other string format will return None
   * 
   */
  def parseTime(time : String) : Option[(Option[Int],Option[Int],Option[Int],Option[Int])] = {
    try {
    	val timeArray = time.split(":").map(_.toInt)
    	if ( timeArray.size > 0 && timeArray(0) >= 0 && timeArray(0) < 24 )
    	  if ( timeArray.size > 1 && timeArray(1) >= 0 && timeArray(1) < 60 )
    	    if ( timeArray.size > 2 && timeArray(2) >= 0 && timeArray(2) < 60 )
    	      if ( timeArray.size > 3 && timeArray(3) >= 0 && timeArray(3) < 1000 )
    	    	  Some( Some(timeArray(0)), Some(timeArray(1)), Some(timeArray(2)), Some(timeArray(3)) )
    	      else 
    	    	  Some( (Some(timeArray(0)), Some(timeArray(1)), Some(timeArray(2)), None) )
    	    else 
    	      Some( (Some(timeArray(0)), Some(timeArray(1)), None, None) )
    	  else 
    	    Some( (Some(timeArray(0)), None, None, None) )
    	else None
    } catch {
      case _ : Throwable => None
    }

  }
  

}