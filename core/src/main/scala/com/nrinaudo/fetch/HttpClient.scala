package com.nrinaudo.fetch

import java.net.{ProtocolException, HttpURLConnection}
import javax.net.ssl.HttpsURLConnection

object HttpClient {
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
                      chunkSize: Int = 4096) {
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
      body.length map con.setFixedLengthStreamingMode getOrElse con.setChunkedStreamingMode(chunkSize)
      con.setRequestProperty("Content-Type", body.mimeType.toString)
    }

    // Headers.
    request.headers.foreach {case (name, value) => con.setRequestProperty(name, value)}

    con.connect()

    // Writes the request body if necessary.
    request.body.foreach {body => body.write(con.getOutputStream)}

    val status = Status(con.getResponseCode)
    new Response(status,
      new ResponseEntity(Option(con.getContentType).map {MimeType(_)},
      if(status.isError) con.getErrorStream else con.getInputStream)
    )
  }

  def apply(req: Request): Response[ResponseEntity] = req.url.openConnection() match {
    case con: HttpURLConnection => process(con, req)
    case _                      => throw new AssertionError("An URL opened a non-URL HTTP connection.")
  }
}
