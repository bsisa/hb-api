package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.util.UrlEncode

/**
 * Tests URL encoding
 *
 * @author Patrick Refondini
 */
class UrlEncodeSpec extends BaseSerialisationSpec {

  //UrlEncode.encodeURL(scheme = "http", host = "localhost", port = 9000, path = "/api/mel fin/G20040930101030005", query = "xpath=//ELFIN[@Id='G2004 0203114894000']")

  val originalURL_scheme_001 = "http"
  val originalURL_host_001 = "localhost"
  val originalURL_port_001 = 9000
  val originalURL_path_001 = "/api/mel fin/G20040930101030005"
  val originalURL_fragment_001 = "xpath=//ELFIN[@Id='G2004 0203114894000']"

  val expectedURL_001 = "http://localhost:9000/api/mel%20fin/G20040930101030005?xpath=//ELFIN[@Id='G2004%200203114894000']"

  s"The URL scheme ${originalURL_scheme_001}, host ${originalURL_host_001}, port ${originalURL_port_001}, path ${originalURL_scheme_001}, fragment ${originalURL_scheme_001} " should {
    s"be encoded to ${expectedURL_001}" in {
      val encodeTest_001 = UrlEncode.encodeURL(
        scheme = originalURL_scheme_001,
        host = originalURL_host_001,
        port = originalURL_port_001,
        path = originalURL_path_001,
        query = originalURL_fragment_001)
      encodeTest_001 must be equalTo (expectedURL_001)
    }
  }

  val originalQueryParameter_002 = "//ELFIN[IDENTIFIANT/COMPTE='01 501 006 50']"
  val expectedQueryParameter_002 = "//ELFIN[IDENTIFIANT/COMPTE='01+501+006+50']"
    
  s"The Query parameter ${originalQueryParameter_002} " should {
    s"be encoded to ${expectedQueryParameter_002}" in {
      val encodeTest_002 = UrlEncode.encodeURLQueryParameter(queryParameter = originalQueryParameter_002)
      println(s"encodeTest_002 = ${encodeTest_002}")
      encodeTest_002 must be equalTo (expectedQueryParameter_002)
    }
  }
    
    
    
  
}