package ch.bsisa.hyperbird.util

/**
 * URL/URI encode helper
 *
 * Note: java.net.URLEncoder.encode() is an option but it requires to separate
 * individual query string parameter name and values from the rest of the URL.
 * Indeed parameter separator character `&` or parameter name/value separator
 * character `=` of the query string must not be escaped.
 *
 * @author Patrick Refondini
 */
object UrlEncode {

  /**
   * Use java.net.URI to URI encode using most detailed URI constructor.
   */
  def encodeURL(scheme: String = "", userInfo: String = null, host: String = "localhost", port: Int = 80, path: String = null, query: String = null, fragment: String = null): String = {
    val uri = new java.net.URI(scheme, userInfo, host, port, path, query, fragment)
    uri.toASCIIString()
  }

  
  /**
   * Simply replace white spaces ` ` by `+` sign. 
   * 
   * Note: Using full java.net.URLEncoder.encode such as: 
   * <code>java.net.URLEncoder.encode(queryParameter, scala.io.Codec.UTF8.name)</code>
   * breaks eXist database REST API. 
   */
  def encodeURLQueryParameter(queryParameter: String = null): String = {
    val encodedParameter = queryParameter.replaceAll(" ", "+")
    encodedParameter
  }  
  
  
}