package ch.bsisa.hyperbird.cache

import ch.bsisa.hyperbird.ApiConfig
import ch.bsisa.hyperbird.Implicits._

import play.api.Logger
import play.api.cache.Cache
import play.api.Play
import play.api.Play.current
import play.api.cache.EhCachePlugin

// EhCache is a Java library returning i.e. java.util.List
import scala.collection.JavaConversions._

/**
 * Centralises cache related operations.
 *
 * <ul>
 * <li>Helps refactoring code which is specific to EhCache and play default configurations (i.e. cache name).</li>
 * <li>Central point to enable / disable cache functionality (currently through implicit use).</li>
 * </ul>
 *
 * @author Patrick Refondini
 */
object CacheHelper {

  val CACHE_NAME = "play"
  val QUERY_CACHE_DEFAULT_TTL = 600 // 10 minutes

  /**
   * Set cache entry with `key`, `value` and `ttl` if specified otherwise defaults to QUERY_CACHE_DEFAULT_TTL.
   *
   */
  def setCache(key: String, value: Any, ttl: Integer = QUERY_CACHE_DEFAULT_TTL)(implicit apiConfig: ApiConfig) = {
    if (apiConfig.queryCacheEnabled) {
      Logger.debug(s"CacheHelper.setCache(cacheKey = ${key}, value (not displayed), ttl = ${ttl}) :: Cache enabled - action performed.")
      Cache.set(key, value, ttl)
    } else {
      Logger.debug(s"CacheHelper.setCache(cacheKey = ${key}, value (not displayed), ttl = ${ttl}) :: Cache disabled - action NOT performed.")
    }
  }

  /**
   * Get `play.api.libs.json.JsValue` cache entry for provided string `key` or None 
   * if either the entry is not found or the cache system is disabled.
   */
  def getCachedJsonValue(key: String)(implicit apiConfig: ApiConfig): Option[play.api.libs.json.JsValue] = {
    if (apiConfig.queryCacheEnabled) {
      Logger.debug(s"CacheHelper.getCachedJsonValue(cacheKey = ${key}) :: Cache enabled - action performed.")
      Cache.getAs[play.api.libs.json.JsValue](key)

    } else {
      Logger.debug(s"CacheHelper.getCachedJsonValue(cacheKey = ${key}) :: Cache disabled - action not performed returns None IN ALL CASE.")
      None
    }

  }

  /**
   * Remove cache entries with cache keys containing `matchString`.
   *
   * For instance when a database object identified by `collectionId` and `objectId` is modified or removed
   * we need to invalidate all cache entries for its `collectionId`.
   */
  def removeEntriesContaining(matchString: String)(implicit apiConfig: ApiConfig) = {

    if (apiConfig.queryCacheEnabled) {
      Logger.debug(s"CacheHelper.removeEntriesContaining(matchString = ${matchString}) :: Cache enabled - action performed.")

      // Get a reference to the EhCachePlugin manager
      val cacheManager = Play.application.plugin[EhCachePlugin]
        .getOrElse(throw new RuntimeException("EhCachePlugin not loaded")).manager

      val ehCache = cacheManager.getCache(CACHE_NAME)
      val ehCacheKeys = ehCache.getKeys() // ehCache.getKeysWithExpiryCheck()

      for (key <- ehCacheKeys) {
        key match {
          case stringKey: String =>
            {
              Logger.debug(s">>>> ehCacheKey TYPE String : stringKey = ${stringKey}")
              if (stringKey.contains(matchString)) {
                ehCache.remove(stringKey)
                Logger.debug("Removed entry with key ${stringKey}")
              }
            }
        }
      }
    } else {
      Logger.debug(s"CacheHelper.removeEntriesContaining(matchString = ${matchString}) :: Cache disabled - action NOT performed.")
    }

  }
  
  
   /**
   * Remove all cache entries starting with `http`, thus avoiding killing all user sessions for instance.
   *
   */
  def removeAllEntries()(implicit apiConfig: ApiConfig) = {

    if (apiConfig.queryCacheEnabled) {
      Logger.debug(s"CacheHelper.removeAllEntries :: Cache enabled - action performed.")

      // Get a reference to the EhCachePlugin manager
      val cacheManager = Play.application.plugin[EhCachePlugin]
        .getOrElse(throw new RuntimeException("EhCachePlugin not loaded")).manager

      val ehCache = cacheManager.getCache(CACHE_NAME)
      val ehCacheKeys = ehCache.getKeys() // ehCache.getKeysWithExpiryCheck()

      for (key <- ehCacheKeys) {
        key match {
          case stringKey: String =>
            {
              Logger.debug(s">>>> ehCacheKey TYPE String : stringKey = ${stringKey}")
             if (stringKey.startsWith("http")) {
                ehCache.remove(stringKey)
                Logger.debug("Removed entry with key ${stringKey}")
              }
            }
        }
      }
    } else {
      Logger.debug(s"CacheHelper.removeAllEntries :: Cache disabled - action NOT performed.")
    }

  }
  
  

}