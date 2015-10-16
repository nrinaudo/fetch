package fetch

import java.net.URI

import fetch.Request.HttpEngine

object Url {
  // - URI-based construction ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def fromUri(uri: URI): Option[Url] = for {
    protocolStr <- Option(uri.getScheme)
    protocol    <- Protocol.parse(protocolStr)
    host        <- Option(uri.getHost)

  } yield Url(protocol, host, if(uri.getPort == -1) protocol.defaultPort else uri.getPort,
      splitPath(uri.getRawPath), QueryString(uri.getRawQuery), Option(uri.getFragment))

  private def splitPath(path: String) =
    if(path == null) Nil
    else             path.split("/").toList.filter(!_.isEmpty).map(UrlEncoder.decode)


  // - String-based construction ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def parse(url: String): Option[Url] = fromUri(new URI(url))
}

case class Url(protocol: Protocol, host: String, port: Int, path: List[String] = List(),
               query: QueryString = QueryString(), fragment: Option[String] = None) {
  // - Url building ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def protocol(value: Protocol): Url = copy(protocol = value)
  def host(value: String): Url = copy(host = value)
  def port(value: Int): Url = copy(port = value)
  def path(value: String*): Url = path(value.toList)
  def path(value: List[String]): Url = copy(path = value)
  def fragment(value: Option[String]): Url = copy(fragment = value)
  // TODO: maybe a list, with its slow appends, is not the best solution for storing paths?
  def addSegment(value: String): Url = path(path :+ value :_*)
  def query(value: QueryString): Url = copy(query = value)
  def param[T: ValueWriter](name: String, values: T*): Url = query(query.set(name, values: _*))
  def resolve(address: String): Option[Url] =
    try { Url.fromUri(toURI.resolve(new URI(address))) }
    catch { case _: Exception => None }


  def /(segment: String): Url = addSegment(segment)

  def ?(value: QueryString): Url = query(value)

  // TODO: this is currently extremely cumbersome if T happens to be a List[T].
  // Implicit from ValueFormat[T] to ValueFormat[List[T]]?
  def &[T: ValueWriter](value: (String, T)): Url = param(value._1, value._2)



  // - Object methods --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Note: I wanted to implement this properly with the correct URI constructor and all, but it turns out this creates
  // a string and then parses it...
  def toURI: URI = new URI(toString)

  def toRequest(implicit engine: HttpEngine): Request[Response[Response.Entity]] = Request(this)

  override lazy val toString = {
    val builder = new StringBuilder

    // Protocol and host.
    builder.append(protocol.name).append("://").append(host)

    // Appends the port if different from the default one.
    if(protocol.defaultPort != port) builder.append(':').append(port)

    // Path
    builder.append(path.filter(!_.isEmpty).map(UrlEncoder.encode).mkString("/", "/", ""))

    // Query String.
    if(query.values.nonEmpty) query.writeTo(builder.append('?'))

    fragment.foreach {r => builder.append("#").append(UrlEncoder.encode(r))}

    builder.toString()
  }
}