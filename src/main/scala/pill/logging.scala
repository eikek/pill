package pill

import pill.config._

package logging {

  class LoggingProperty extends ch.qos.logback.core.PropertyDefinerBase {
    lazy val conf = Config.log
    @scala.beans.BeanProperty
    var name: String = ""

    def getPropertyValue(): String = name match {
      case "logfile" => conf.file.toString
      case "loglevel" => conf.level
      case _ => ""
    }
  }
}
