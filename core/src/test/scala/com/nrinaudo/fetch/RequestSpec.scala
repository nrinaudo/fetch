package com.nrinaudo.fetch

import org.scalatest.{BeforeAndAfterAll, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import unfiltered.filter.Planify
import unfiltered.request._
import unfiltered.response.{Status => SStatus, _}
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import java.io.{InputStreamReader, StringReader, Reader, OutputStreamWriter}
import java.util.zip.{InflaterInputStream, GZIPInputStream}
import unfiltered.response.ResponseString

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

class RequestSpec extends FunSpec with BeforeAndAfterAll with ShouldMatchers with GeneratorDrivenPropertyChecks {
  // - Test HTTP server ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val server = unfiltered.jetty.Http.anylocal.plan(Planify {
    unfiltered.kit.GZip {
      case Path(Seg("echo"   :: text   :: Nil)) => ResponseString(text)
      case Path(Seg("status" :: status :: Nil)) => SStatus(status.toInt)
      case req @ Path(Seg("method" :: Nil))     => ResponseString(req.method)
      case req @ Path(Seg("body" :: Nil))       => new ReaderResponse(req.reader)
      case req @ Path(Seg("auth" :: Nil))       => req match {
        case BasicAuth(user, pwd) => new ResponseString(user + "\n" + pwd)
        case _                    => Unauthorized ~> WWWAuthenticate("""Basic realm="/"""")
      }

      // Note: due to a bug in Unfiltered, we'll be assuming the charset is UTF-8.
      case req @ Path(Seg("compress" :: format :: Nil)) =>
        assert(req.headers("Content-Encoding").contains(format))
        val in = format match {
          case "gzip"    => new GZIPInputStream(req.inputStream)
          case "deflate" => new InflaterInputStream(req.inputStream)
        }

        new ReaderResponse(new InputStreamReader(in, "UTF-8"))
    }
  })

  val client = HttpClient()

  def request(path: String) = Request(server.url + path)

  override def beforeAll(conf: Map[String, Any]) {
    server.start()
  }

  override def afterAll(conf: Map[String, Any]) {
    server.stop()
  }

  def httpMethod = Gen.oneOf("GET", "POST", "PUT", "DELETE", "OPTIONS", "TRACE", "PATCH", "LINK", "UNLINK")

  def nonEmpty(gen: Gen[String]) = gen.suchThat {!_.isEmpty}

  def entity = nonEmpty(arbitrary[String])

  // Note that this is not entirely correct: according to the RFC, password are allowed to contain a ':'. This is not
  // properly handled in version 0.7.1 of unfiltered, however (the issue is fixed in github, but not yet released).
  def authCredentials = for {
    user <- nonEmpty(arbitrary[String]).suchThat {!_.contains(':')}
    pwd <- nonEmpty(arbitrary[String]).suchThat {!_.contains(':')}
  } yield (user, pwd)

  describe("A Request") {
    it("should send the correct HTTP method") {
      forAll(httpMethod) { method =>
        client(request("method").method(method)).body.text() should be(method)
      }
    }

    it("should receive the expected status code when querying a resource") {
      forAll(Gen.choose(200, 599)) {code =>
        client(request("status/" + code)).status should be(Status(code))
      }
    }

    it("should read simple entity bodies correctly") {
      forAll(nonEmpty(Gen.identifier)) { text =>
        client(request("echo/" + text)).body.text() should be(text)
      }
    }

    it("should submit entities whose size is known correctly") {
      forAll(entity) {text =>
        client(request("body").PUT.body(text)).body.text() should be(text)
      }
    }

    it("should submit entities whose size is not known correctly") {
      forAll(entity) {text =>
        client(request("body").PUT.body(new StringReader(text))).body.text() should be(text)
      }
    }

    it("should submit gzipped entities correctly") {
      forAll(entity) {text =>
        client(request("compress/gzip").PUT.body(new StringReader(text)).gzip).body.text() should be(text)
      }
    }

    it("should submit deflated entities correctly") {
      //forAll(entity) {text =>
      val text = "this is a test"
      client(request("compress/deflate").PUT.body(new StringReader(text)).deflate).body.text() should be(text)
      //}
    }

    it("should send basic auth credentials properly") {
      forAll(authCredentials) {case (user, pwd) =>
        client(request("auth").auth(user, pwd)).body.text() should be(user + "\n" + pwd)
      }
    }
  }
}
