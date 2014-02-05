package ch.bsisa.hyperbird.util

import java.text.SimpleDateFormat
import java.util.Date

/**
 * Basic generator
 * TODO: review existing implementation (synchronise, pool,...)
 */
object ElfinIdGenerator {

  def getNewElfinId(): String = {
    val df = new SimpleDateFormat("yyyyMMddHHmmssSSS")
    val elfinId = "G" + df.format(new Date())
    elfinId
  }

  def getElfinFileName(elfinId: String, elfinCLASSE: String): String = {
    elfinCLASSE + elfinId + ".xml"
  }  
  
  
}