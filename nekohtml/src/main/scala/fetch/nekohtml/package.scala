package fetch

import java.nio.charset.Charset
import javax.xml.transform.sax
import javax.xml.transform.sax.SAXSource

import org.xml.sax.{InputSource, XMLReader}

import scala.xml.Node
import scala.xml.parsing.NoBindingFactoryAdapter


package object nekohtml {
  private def asXml(reader: XMLReader, src: InputSource): Node = {
    val source = new SAXSource(reader, src)
    val adapter = new NoBindingFactoryAdapter
    val saxResult = new sax.SAXResult(adapter)
    val transformerFactory = javax.xml.transform.TransformerFactory.newInstance()
    val transformer = transformerFactory.newTransformer()
    transformer.transform(source, saxResult)
    adapter.rootElem
  }

  implicit def reader(implicit defaultCharset: Charset): EntityReader[Node] = EntityReader.chars { in =>
    val parser = new org.cyberneko.html.parsers.SAXParser

    parser.setProperty("http://cyberneko.org/html/properties/names/elems", "lower")
    parser.setProperty("http://cyberneko.org/html/properties/names/attrs", "lower")

    asXml(parser, new InputSource(in))
  }
}
