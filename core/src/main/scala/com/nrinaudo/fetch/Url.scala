package com.nrinaudo.fetch

import java.net.URLEncoder
import scala.util.Try
import java.net.URL

object Url {
  // - URL-based construction ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def unapply(url: URL): Option[Url] = Some(apply(url))

  def apply(url: URL): Url =
    Url(Protocol(url.getProtocol), url.getHost, url.getPort, splitPath(url.getPath), QueryString(url.getQuery), Option(url.getRef))

  private def splitPath(path: String) = path.split("/").toList.filter(!_.isEmpty)


  // - String-based construction ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def unapply(url: String): Option[Url] = Try {apply(url)}.toOption
  def apply(url: String): Url = Url(new URL(url))
}

case class Url(protocol: Protocol, host: String, port: Int, path: List[String] = List(),
               query: QueryString = new QueryString(), fragment: Option[String] = None) {
  // - Url building ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def protocol(value: Protocol): Url = copy(protocol = value)
  def host(value: String): Url = copy(host = value)
  def port(value: Int): Url = copy(port = value)
  def path(value: String): Url = copy(path = value.split("/").toList)
  def path(value: List[String]): Url = copy(path = value)
  def fragment(value: Option[String]): Url = copy(fragment = value)
  def query(value: QueryString): Url = copy(query = value)
  def param[T: ValueWriter](name: String, values: T*): Url = query(query.set(name, values: _*))

  // TODO: maybe a list, with its slow appends, is not the best solution for storing paths?
  def /(segment: String): Url = path(path :+ segment)

  def ?(value: QueryString): Url = query(value)

  /** Appends the specified parameter to the request's [[Url]].
    *
    * This is purely a convenience method for [[Url.&]].
    */
  def &[T: ValueWriter](param: (String, T)): Url = query(query & param)



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

    // Query String.
    query.writeTo(builder)

    fragment.foreach {r => builder.append(URLEncoder.encode(r, "utf-8"))}

    builder.toString()
  }
}