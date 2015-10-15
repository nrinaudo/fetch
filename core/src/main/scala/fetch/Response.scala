package fetch

import java.io.{FilterInputStream, InputStream}
import java.util.Date

object Response {
  class Entity(private val input: InputStream, val mediaType: Option[MediaType]) {
    def as[A: EntityReader]: A = {
      try { implicitly[EntityReader[A]].read(input, mediaType) }
      finally { input.close() }
    }

    def empty(): Unit = as(EntityReader.emptyReader)
    def ignore(): Unit = as(EntityReader.ignoreReader)
  }

  // It appears that some stream implementation (decompressing ones, for instance, such as GZipInputStream) stop reading
  // before read returns -1 - which seems to prevent keeping connections alive.
  // The following is a nasty workaround, but it works.
  private def closing(stream: InputStream): InputStream = new FilterInputStream(stream) {
    override def close(): Unit = {
      in.read()
      super.close()
    }
  }

  def fromStream(status: Status, headers: Parameters, stream: InputStream): Response[Entity] =
    Response(status, headers,
      new Entity(headers.get[Seq[Encoding]]("Content-Encoding").fold(stream) { values =>
        values.foldRight(closing(stream)) { _ decode _ }
      }, headers.get[MediaType]("Content-Type")))
}

/** Represents an HTTP response. */
case class Response[A](status: Status, headers: Parameters, body: A) {
  def map[B](f: A => B): Response[B] = copy(body = f(body))



  // - Header helpers ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def date: Option[Date] = headers.get[Date]("Date")(HttpDate)
  def contentEncoding: Option[Seq[Encoding]] = headers.get[Seq[Encoding]]("Content-Encoding")
  def contentLanguage: Option[Seq[Language]] = headers.get[Seq[Language]]("Content-Language")
  def contentType: Option[MediaType] = headers.get[MediaType]("Content-Type")
  def lastModified: Option[Date] = headers.get[Date]("Last-Modified")(HttpDate)
  def expires: Option[Date] = headers.get[Date]("Expires")(HttpDate)
  def etag: Option[ETag] = headers.get[ETag]("ETag")
  def server: Option[String] = headers.get[String]("Server")
  def allow: Option[Seq[Method]] = headers.get[Seq[Method]]("Allow")
  def age: Option[Int] = headers.get[Int]("Age")
  def wwwAuthenticate: Option[String] = headers.get[String]("WWW-Authenticate")

  // TODO: implement Accept-Ranges
  // TODO: implement Cache-Control
  // TODO: implement Connection
  // TODO: implement Content-Length
  // TODO: implement Content-Location
  // TODO: implement Content-Disposition
  // TODO: implement Content-Range
  // TODO: implement Location
  // TODO: implement Pragma
  // TODO: implement Retry-After
  // TODO: implement Set-Cookie
  // TODO: implement Vary
}