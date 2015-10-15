package fetch

import java.io._
import java.util.zip.DeflaterOutputStream

import unfiltered.Cycle
import unfiltered.filter.Plan
import unfiltered.filter.Plan.Intent
import unfiltered.jetty.Server
import unfiltered.kit.Prepend
import unfiltered.request._
import unfiltered.response.ResponseFilter.Filtering
import unfiltered.response.{ResponseString, Status => SStatus, _}

import scala.io.Source

/** Web server used for unit tests. */
object TestPlan extends Plan {
  // - Helper classes --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private class ReaderResponse(val reader: Reader) extends ResponseWriter {
    override def respond(res: HttpResponse[Any]): Unit = {
      res.header("Content-Type", "text/plain;charset=\"" + res.charset.name() + "\"")
      super.respond(res)
    }

    override def write(writer: OutputStreamWriter): Unit = {
      var c = -1
      while({c = reader.read; c >= 0}) {
        writer.write(c)
      }
      reader.close()
    }
  }

  private class HeaderResponse(name: String, value: String) extends Responder[Any] {
    override def respond(res: HttpResponse[Any]): Unit = {
      res.header(name, value)
      res.status(200)
    }
  }

  /** Kit that automatically uses the GZIP or DEFLATE content-encoding if requested. */
  object Decoder extends Prepend {
    def intent = Cycle.Intent[Any,Any] {
      case Decodes.GZip(req) => ContentEncoding.GZip ~> ResponseFilter.GZip
      case Decodes.Deflate(req) => ContentEncoding("deflate") ~> new Filtering[DeflaterOutputStream] {
        override def filter(os: OutputStream): DeflaterOutputStream = new DeflaterOutputStream(os)
      }
    }
  }



  // - Startup / shutdown ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def create: Server = unfiltered.jetty.Server.anylocal.plan(TestPlan)
  def start(implicit server: Server) = server.start()
  def stop(implicit server: Server) = server.stop()



  // - Actual plan -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  override def intent: Intent = Decoder {
    case Path(Seg("empty" :: Nil)) => NotFound

    // Returns an empty response with the requested status.
    case Path(Seg("status" :: status :: Nil)) => SStatus(status.toInt)

    // Returns a response whose body is the requested HTTP method.
    case req @ Path(Seg("method" :: Nil)) => ResponseString(req.method)

     /* Returns the request entity body, applying the requested content-encodings if necessary.
      * Note that due to a bug in the current version of Unfiltered (fixed in github, not yet released), we
      * can't easily extract the request's charset and will be assuming UTF-8.
      */
    case req @ Path(Seg("body" :: Nil)) =>
      val in = req.headers("Content-Encoding").toList.reverse.foldLeft(req.inputStream) {(in, encoding) =>
        Encoding.DefaultEncodings.get(encoding) map {e => e.decode(in)} getOrElse {
          throw new IOException("Unsupported encoding: " + encoding)
        }
      }
      new ReaderResponse(new InputStreamReader(in, "UTF-8"))

    // Expects to find basic auth credentials. If found, returns them, otherwise fails.
    case req @ Path(Seg("auth" :: Nil))       => req match {
      case BasicAuth(user, pwd) => new ResponseString(user + "\n" + pwd)
    }

    // Returns the value(s) of the specified request header, separated by line breaks if the header has more than one
    // value.
    case req @ Path(Seg("header" :: header :: Nil)) =>
      req match {
        case GET(_) =>
          val value = req.headers(header)
          if(value.hasNext) ResponseString(value.map(_.trim).mkString("\n"))
          else NotFound

        case POST(_) => new HeaderResponse(header, Source.fromInputStream(req.inputStream, "UTF-8").mkString)
      }
  }
}
