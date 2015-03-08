package com.nrinaudo.fetch

import com.nrinaudo.fetch.ResponseEntity._
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import scala.xml.NodeSeq
import scala.xml.parsing.NoBindingFactoryAdapter

package object tagsoup {
  implicit val Parser: EntityParser[NodeSeq] = (entity: ResponseEntity) =>
    entity.withReader { in =>
      new NoBindingFactoryAdapter().loadXML(new InputSource(in), new SAXFactoryImpl().newSAXParser)
    }
}
