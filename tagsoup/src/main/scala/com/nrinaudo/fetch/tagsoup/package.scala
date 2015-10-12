package com.nrinaudo.fetch

import java.nio.charset.Charset

import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter

package object tagsoup {
  implicit def reader(implicit defaultCharset: Charset): EntityReader[Node] = EntityReader.chars { in =>
    new NoBindingFactoryAdapter().loadXML(new InputSource(in), new SAXFactoryImpl().newSAXParser)
  }(defaultCharset)
}
