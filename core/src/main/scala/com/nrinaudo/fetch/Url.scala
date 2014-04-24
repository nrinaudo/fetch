package com.nrinaudo.fetch

import java.net.URLEncoder
import scala.util.Try

object Url {
  import java.net.URL

  type Query = Map[String, List[String]]

  private def splitPath(path: String) = path.split("/").toList.filter(!_.isEmpty)

  private def splitQuery(query: String): Query =
    if(query == null) Map()
    else              query.split("&").toList.foldLeft(Map[String, List[String]]()) {case (map, entry) =>
      val(name, values) = entry.splitAt(entry.indexOf('='))
      map + (name -> values.substring(1).split(',').toList)
    }

  def apply(url: URL): Url = {
    val proto = url.getProtocol match {
      case "http"  => Protocol.Http
      case "https" => Protocol.Https
      case v       => new Protocol(v, None)
    }
    Url(proto, url.getHost, url.getPort, splitPath(url.getPath), splitQuery(url.getQuery), Option(url.getRef))
  }

  def unapply(url: String): Option[Url] = Try {Url(new URL(url))}.toOption
}

/**
 * @author Nicolas Rinaudo
 */
case class Url(protocol: Protocol, host: String, port: Int, path: List[String] = List(),
               query: Url.Query = Map(), ref: Option[String] = None) {
  // - Url building ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def protocol(value: Protocol): Url = copy(protocol = value)
  def host(value: String): Url = copy(host = value)
  def port(value: Int): Url = copy(port = value)
  def path(value: String): Url = copy(path = value.split("/").toList)
  def path(value: List[String]): Url = copy(path = value)
  def ref(value: Option[String]): Url = copy(ref = value)
  def query(value: Url.Query): Url = copy(query = value)


  // - Object methods --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  override def hashCode: Int = toString.hashCode

  override def equals(o: Any): Boolean = o match {
    case u: Url => toString == u.toString
    case _      => false
  }

  override lazy val toString = {
    val builder = new StringBuilder

    // Protocol and host.
    builder.append(protocol.value).append("://").append(host)

    // Appends the port if different from the default one.
    if(protocol.defaultPort.filter(_ == port).isEmpty) builder.append(':').append(port)

    // Path
    builder.append(path.map(URLEncoder.encode(_, "utf-8")).mkString("/", "/", ""))

    val q = query.foldLeft(List[String]()) {case (list, (name, values)) =>
      (name + "=" + values.map(URLEncoder.encode(_, "utf-8")).mkString(",")):: list
    }
    if(!q.isEmpty) q.sorted.addString(builder, "?", "&", "")

    ref.foreach {r => builder.append(URLEncoder.encode(r, "utf-8"))}

    builder.toString()
  }
}