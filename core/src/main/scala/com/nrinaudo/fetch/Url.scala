package com.nrinaudo.fetch

import java.net.URLEncoder
import scala.util.Try
import javax.management.Query

object Url {
  import java.net.URL


  // - URL-based construction ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def unapply(url: URL): Option[Url] = Some(apply(url))
  def apply(url: URL): Url = {
    val Protocol(protocol) = url.getProtocol
    Url(protocol, url.getHost, url.getPort, splitPath(url.getPath), QueryParameters(url.getQuery), Option(url.getRef))
  }

  private def splitPath(path: String) = path.split("/").toList.filter(!_.isEmpty)



  // - String-based construction ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def unapply(url: String): Option[Url] = Try {apply(url)}.toOption
  def apply(url: String): Url = Url(new URL(url))
}

/**
 * @author Nicolas Rinaudo
 */
case class Url(protocol: Protocol, host: String, port: Int, path: List[String] = List(),
               query: QueryParameters = new QueryParameters(), ref: Option[String] = None) {
  // - Url building ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def protocol(value: Protocol): Url = copy(protocol = value)
  def host(value: String): Url = copy(host = value)
  def port(value: Int): Url = copy(port = value)
  def path(value: String): Url = copy(path = value.split("/").toList)
  def path(value: List[String]): Url = copy(path = value)
  def ref(value: Option[String]): Url = copy(ref = value)
  def query(value: QueryParameters): Url = copy(query = value)


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
    builder.append(protocol.name).append("://").append(host)

    // Appends the port if different from the default one.
    if(protocol.defaultPort.filter(_ == port).isEmpty) builder.append(':').append(port)

    // Path
    builder.append(path.map(URLEncoder.encode(_, "utf-8")).mkString("/", "/", ""))

    if(!query.values.isEmpty) builder.append("?").append(query.toString)

    ref.foreach {r => builder.append(URLEncoder.encode(r, "utf-8"))}

    builder.toString()
  }
}