package ch.bsisa.hyperbird

import ch.bsisa.hyperbird.dao.DbConfig
import ch.bsisa.hyperbird.report.ReportConfig

/**
 * Configuration objects exposed as implicit values.
 *
 *
 * To get access to these implicit values a class needs to declare:
 * <code>import ch.bsisa.hyperbird.Implicits._</code>
 *
 * @author Patrick Refondini
 */
object Implicits {

  /**
   * Database configurations
   */
  implicit val dbConfig = new DbConfig

  /**
   * Database configurations
   */
  implicit val collectionsConfig = new CollectionsConfig

  /**
   * Init configurations
   */
  implicit val initConfig = new InitConfig

  /**
   * Api configurations
   */
  implicit val apiConfig = new ApiConfig

  /**
   * Report Configuration
   */
  implicit val reportConfig = new ReportConfig

}