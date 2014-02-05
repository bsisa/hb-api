package ch.bsisa.hyperbird.util

object UrlEncode extends App {
  

  // TODO: make test usage with exist REST api
  // TODO: implement unit tests
  println("encodeURL : " + encodeURL(scheme = "http", host = "localhost", port = 9000, path = "/api/mel fin/G20040930101030005", query = "xpath=//ELFIN[@Id='G2004 0203114894000']"))
  
  
  def encodeURL(scheme: String = "", userInfo: String = null, host: String = "localhost", port: Int = 80, path: String = null, query: String = null, fragment: String = null): String = {
    val uri = new java.net.URI(scheme, userInfo, host, port, path, query, fragment)
    uri.toASCIIString()
  }

}