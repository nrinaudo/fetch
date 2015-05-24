package com.nrinaudo.fetch

import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import scala.xml.NodeSeq
import scala.xml.parsing.NoBindingFactoryAdapter

package object tagsoup {
  implicit val reader: EntityReader[NodeSeq] = EntityReader.chars { in =>
      new NoBindingFactoryAdapter().loadXML(new InputSource(in), new SAXFactoryImpl().newSAXParser)
  }

}
