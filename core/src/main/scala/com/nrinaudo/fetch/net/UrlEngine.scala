package com.nrinaudo.fetch.net

import java.net.{ProtocolException, HttpURLConnection}
import javax.net.ssl.HttpsURLConnection
import com.nrinaudo.fetch._
import com.nrinaudo.fetch.Response
import com.nrinaudo.fetch.Status
import scala.collection.JavaConverters._
import com.nrinaudo.fetch.Request.HttpEngine
import java.io.InputStream

object UrlEngine {
  /** Default chunk size (in bytes) when chunked transfer encoding is used. */
  val DefaultChunkSize = 4096

  private lazy val methodField = {
    val field = classOf[HttpURLConnection].getDeclaredField("method")
    field.setAccessible(true)
    field
  }
}

/**
 * `java.net` connector for fetch.
 */
case class UrlEngine(readTimeout: Int = 0, connectTimeout: Int = 0, followsRedirect: Boolean = false,
                      chunkSize: Int = UrlEngine.DefaultChunkSize) extends HttpEngine {
  /** Configures the specified connection to this client's preferences. */
  private def configure(con: HttpURLConnection) {
    con.setConnectTimeout(connectTimeout)
    con.setReadTimeout(connectTimeout)
    con.setInstanceFollowRedirects(followsRedirect)
  }

  /** Work around for some (all?) JREs not supporting all HTTP methods.
    * See https://java.net/jira/browse/JERSEY-639
    */
  private def setMethod(con: HttpURLConnection, method: String) {
    try {con.setRequestMethod(method)}
    catch {
      case _: ProtocolException =>
        con match {
          case https: HttpsURLConnection => https.getClass.getDeclaredFields.find {_.getName == "delegate"}.foreach {d =>
            d.setAccessible(true)
            UrlEngine.methodField.set(d.get(con), method)
          }
          case _ => UrlEngine.methodField.set(con, method)
        }
    }
  }

  /** Best effort attempt at finding a workable stream. If all else fails, use an empty stream. */
  private def responseStream(status: Status, con: HttpURLConnection) = {
    val stream = if(status.isError) con.getErrorStream else con.getInputStream

    if(stream == null) new InputStream {
      override def read(): Int = -1
    }
    else stream
  }

  private def process(con: HttpURLConnection, method: Method, body: Option[RequestEntity], headers: Headers) = {
    // Generic configuration.
    configure(con)
    setMethod(con, method.name)

    // Entity body configuration.
    body.foreach {b =>
      con.setDoOutput(true)
      // Note: this is currently somewhat broken because of Java 1.6 that does not support fixed-length streaming mode
      // as longs. If the entity's content length is larger than an int, we have an issue.
      // TODO: check against maxint and used chunked encoding if larger?
      b.contentLength.fold(con.setChunkedStreamingMode(chunkSize)) { length => con.setFixedLengthStreamingMode(length.toInt) }
    }

    // Headers.
    headers.values.foreach {case (name, value) => con.setRequestProperty(name, value)}
    con.connect()

    // Writes the request body if necessary.
    body.foreach {_(con.getOutputStream)}

    val status = Status(con.getResponseCode)
    new Response(status,
      new Headers(con.getHeaderFields.asScala.mapValues(_.asScala.mkString(", ")).toMap),
      new ResponseEntity(Option(con.getContentType) flatMap MimeType.unapply, responseStream(status, con)))
  }

  def apply(url: Url, method: Method, body: Option[RequestEntity], headers: Headers): Response[ResponseEntity] =
    url.toURI.toURL.openConnection() match {
      case con: HttpURLConnection => process(con, method, body, headers)
      case _                      => throw new AssertionError("An URL opened a non-URL HTTP connection.")
    }
}
