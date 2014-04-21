package com.nrinaudo.fetch

import unfiltered.filter.Plan
import unfiltered.filter.Plan.Intent
import unfiltered.request.{Decodes, BasicAuth, Seg, Path}
import unfiltered.response.{Status => SStatus, _}
import java.util.zip.DeflaterOutputStream
import java.io._
import unfiltered.kit.Prepend
import unfiltered.Cycle
import unfiltered.response.ResponseFilter.Filtering
import unfiltered.response.ResponseString

/**
 * @author Nicolas Rinaudo
 */
object TestPlan extends Plan {
  class ReaderResponse(val reader: Reader) extends ResponseWriter {
    override def respond(res: HttpResponse[Any]): Unit = {
      res.header("Content-Type", "text/plain;charset=\"" + res.charset.name() + "\"")
      super.respond(res)
    }

    override def write(writer: OutputStreamWriter) {
      var c = -1
      while({c = reader.read; c >= 0}) {
        writer.write(c)
      }
      reader.close()
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


  override def intent: Intent = Decoder {
    /** Returns an empty response with the requested status. */
    case Path(Seg("status" :: status :: Nil)) => SStatus(status.toInt)

    /** Returns a response whose body is the requested HTTP method. */
    case req @ Path(Seg("method" :: Nil)) => ResponseString(req.method)

    /** Returns the request entity body, applying the requested content-encodings if necessary.
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

    /** Expects to find basic auth credentials. If found, returns them, otherwise fails. */
    case req @ Path(Seg("auth" :: Nil))       => req match {
      case BasicAuth(user, pwd) => new ResponseString(user + "\n" + pwd)
      case _                    => Unauthorized ~> WWWAuthenticate("""Basic realm="/"""")
    }
  }
}
