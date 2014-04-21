package com.nrinaudo.fetch.net

import java.net.{URL, ProtocolException, HttpURLConnection}
import javax.net.ssl.HttpsURLConnection
import com.nrinaudo.fetch._
import com.nrinaudo.fetch.Response
import com.nrinaudo.fetch.Status
import scala.collection.JavaConverters._
import com.nrinaudo.fetch.Request.Engine

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
 * @author Nicolas Rinaudo
 */
case class UrlEngine(readTimeout: Int = 0, connectTimeout: Int = 0, followsRedirect: Boolean = false,
                      chunkSize: Int = UrlEngine.DefaultChunkSize) extends Engine {
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

  private def process(con: HttpURLConnection, method: String, body: Option[RequestEntity], headers: Headers) = {
    // Generic configuration.
    configure(con)
    setMethod(con, method)

    // Entity body configuration.
    body.foreach {b =>
      con.setDoOutput(true)
      b.contentLength map con.setFixedLengthStreamingMode getOrElse con.setChunkedStreamingMode(chunkSize)
    }

    // Headers.
    headers.foreach {case (name, value) => con.setRequestProperty(name, value.mkString(", "))}

    con.connect()

    // Writes the request body if necessary.
    body.foreach {b => b(con.getOutputStream)}

    val status = Status(con.getResponseCode)
    new Response(status,
      con.getHeaderFields.asScala.mapValues(_.asScala.toList).toMap,
      new ResponseEntity(Option(con.getContentType).map {MimeType(_)},
      if(status.isError) con.getErrorStream else con.getInputStream)
    )
  }

  def apply(url: URL, method: String, body: Option[RequestEntity], headers: Headers): Response[ResponseEntity] =
    url.openConnection() match {
      case con: HttpURLConnection => process(con, method, body, headers)
      case _                      => throw new AssertionError("An URL opened a non-URL HTTP connection.")
    }
}
