package fr.acinq.pay2email

import java.io.{StringWriter, Reader, OutputStreamWriter, File}
import java.net.URL

import com.google.common.io.Resources
import freemarker.cache.URLTemplateLoader
import freemarker.template.{Version, TemplateExceptionHandler, DefaultObjectWrapper, Configuration}

import scala.beans.BeanInfo

object HtmlGenerator {

  val cfg = new Configuration()
  cfg.setClassForTemplateLoading(this.getClass, "/web")
  cfg.setObjectWrapper(new DefaultObjectWrapper())
  cfg.setDefaultEncoding("UTF-8")
  cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER)
  cfg.setIncompatibleImprovements(new Version(2, 3, 20))

  def generate(tplName: String, tplData: java.util.Map[String, Object]): String = {
    val tpl = cfg.getTemplate(tplName)
    val out = new StringWriter()
    tpl.process(tplData, out)
    out.toString
  }
}
