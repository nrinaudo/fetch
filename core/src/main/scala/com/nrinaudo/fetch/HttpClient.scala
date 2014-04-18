package com.nrinaudo.fetch

import java.net.{ProtocolException, HttpURLConnection}
import javax.net.ssl.HttpsURLConnection
import scala.collection.JavaConverters._

object HttpClient {
  /** Default chunk size (in bytes) when chunked transfer encoding is used. */
  val DefaultChunkSize = 4096

  private lazy val methodField = {
    val field = classOf[HttpURLConnection].getDeclaredField("method")
    field.setAccessible(true)
    field
  }
}

/**
 * @author Nicolas Rinaudo
 */
case class HttpClient(readTimeout: Int = 0, connectTimeout: Int = 0, followsRedirect: Boolean = false,
                      chunkSize: Int = HttpClient.DefaultChunkSize) {
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
            HttpClient.methodField.set(d.get(con), method)
          }
          case _ => HttpClient.methodField.set(con, method)
        }
    }
  }

  private def process(con: HttpURLConnection, request: Request) = {
    // Generic configuration.
    configure(con)
    setMethod(con, request.method)

    // Entity body configuration.
    request.body.foreach {body =>
      con.setDoOutput(true)
      body.contentLength map con.setFixedLengthStreamingMode getOrElse con.setChunkedStreamingMode(chunkSize)
      con.setRequestProperty("Content-Type",     body.mimeType.toString)
      con.setRequestProperty("Content-Encoding", if(body.encoding == Encoding.Identity) null else body.encoding.name)
    }

    // Headers.
    request.headers.foreach {case (name, value) => con.setRequestProperty(name, value.mkString(", "))}

    con.connect()

    // Writes the request body if necessary.
    request.body.foreach {body =>
      val out = body.encoding.encode(con.getOutputStream)
      try {body.write(out)}
      finally {out.close()}
    }

    val status = Status(con.getResponseCode)
    new Response(status,
      con.getHeaderFields.asScala.mapValues(_.asScala.toList).toMap,
      new ResponseEntity(Option(con.getContentType).map {MimeType(_)},
      if(status.isError) con.getErrorStream else con.getInputStream)
    )
  }

  def apply(req: Request): Response[ResponseEntity] = req.url.openConnection() match {
    case con: HttpURLConnection => process(con, req)
    case _                      => throw new AssertionError("An URL opened a non-URL HTTP connection.")
  }
}
