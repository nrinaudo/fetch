package com.nrinaudo.fetch

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import com.nrinaudo.fetch.net.UrlEngine
import java.util.Date
import Headers._

object RequestSpec {
  private val connegValue = "([^;]+)(?:;q=(.*))?".r

  /** Generates random, legal HTTP methods. */
  def httpMethod = Gen.oneOf(Method.GET, Method.POST, Method.PUT, Method.DELETE, Method.OPTIONS, Method.TRACE,
    Method.PATCH, Method.LINK, Method.UNLINK)

  // Note that this is not entirely correct: according to the RFC, password are allowed to contain a ':'. This is not
  // properly handled in version 0.7.1 of unfiltered, however (the issue is fixed in github, but not yet released).
  def authCredentials = for {
    user <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
    pwd  <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
  } yield (user, pwd)

  def simpleDate = for(time <- Gen.choose(0, 253402300799000l)) yield new Date((time / 1000l) * 1000)
}

class RequestSpec extends FunSpec with BeforeAndAfterAll with Matchers with GeneratorDrivenPropertyChecks {
  import RequestSpec._
  import ByteRangeSpec._
  import RequestEntitySpec._
  import ConnegSpec._
  import EncodingSpec._



  // - Test HTTP server ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val server = unfiltered.jetty.Http.anylocal.plan(TestPlan)

  implicit val engine = UrlEngine()

  def request(path: String) = Request(server.url + path)

  override def beforeAll() {
    server.start()
  }

  override def afterAll() {
    server.stop()
  }



  // - Actual tests ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  describe("A Request") {
    // - Basic tests ---------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should use the specified HTTP method") {
      forAll(httpMethod) { method =>
        request("method").method(method).apply().body.as[String] should be(method.name)
      }
    }

    it("should extract the correct HTTP status codes") {
      forAll(Gen.choose(200, 599)) { code =>
        request("status/" + code)().status should be(Status(code))
      }
    }

    it("should correctly send custom HTTP headers when present") {
      forAll(Gen.identifier, Gen.identifier) { (name, value) =>
        request("header/" + name).header(name, value).GET().body.as[String] should be(value)
      }
    }



    // - Entity submission / reception ---------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should correctly read and write entity bodies, regardless of the request and response encoding") {
      forAll(entity, encoding, encoding) { (entity, reqEncoding, resEncoding) =>
        val response = request("body").acceptEncoding(resEncoding).PUT(entity.entity.encoding(reqEncoding))

        if(resEncoding == Encoding.Identity) response.headers.get[String]("Content-Encoding") should be(None)
        else                                 response.headers.get[String]("Content-Encoding") should be(Some(resEncoding.name))

        response.body.as[String] should be(entity.content)
      }
    }



    // - Header helpers ------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    def checkConneg[T](response: Response[ResponseEntity], values: List[Conneg[T]])(f: T => String) =
      response.body.as[String].split(",").zip(values).foreach {
        case (connegValue(param, q), header) =>
          param should be(f(header.value))
          if(q == null) header.q should be(1)
          else          (math.round(header.q * 1000) / 1000f) should be(q.toFloat)
      }

    it("should use the specified Accept header(s), discarding parameters if any is present") {
      forAll(connegs(MimeTypeSpec.mimeType)) { mimeTypes =>
        checkConneg(request("header/Accept").accept(mimeTypes :_*).GET(), mimeTypes) { mimeType =>
          mimeType.main + "/" + mimeType.sub
        }
      }
    }

    it("should send the default Accept header (*/*) when none is specified") {
      request("header/Accept").GET().body.as[String] should be("*/*")
      request("header/Accept").accept().GET().body.as[String] should be("*/*")
    }

    it("should use the specified Accept-Charset header") {
      forAll(connegs(charset)) { charsets =>
        checkConneg(request("header/Accept-Charset").acceptCharset(charsets :_*).GET(), charsets)(_.name())
      }
    }

    it("should not send an Accept-Charset header when none is specified") {
      request("header/Accept-Charset").GET().status should be(Status.NotFound)
      request("header/Accept-Charset").acceptCharset().GET().status should be(Status.NotFound)
    }

    it("should use the specified Accept-Language header") {
      forAll(connegs(language)) { languages =>
        checkConneg(request("header/Accept-Language").acceptLanguage(languages :_*).GET(), languages) { lang =>
          lang.getLanguage + (if(lang.getCountry.isEmpty) "" else "-" + lang.getCountry)
        }
      }
    }

    it("should not send an Accept-Language header when none is specified") {
      request("header/Accept-Language").GET().status should be(Status.NotFound)
      request("header/Accept-Language").acceptLanguage().GET().status should be(Status.NotFound)
    }

    it("should use the specified Accept-Encoding header") {
      forAll(connegs(encoding)) { encodings =>
        checkConneg(request("header/Accept-Encoding").acceptEncoding(encodings :_*).GET(), encodings)(_.name)
      }
    }

    it("should not send an Accept-Encoding header when none is specified") {
      request("header/Accept-Encoding").GET().status should be(Status.NotFound)
      request("header/Accept-Encoding").acceptEncoding().GET().status should be(Status.NotFound)
    }

    it("should send the default User-Agent header when none is specified") {
      request("header/User-Agent").GET().body.as[String] should be(Request.UserAgent)
    }

    it("should send the correct User-Agent header when specified") {
      forAll(Gen.identifier) { userAgent =>
        request("header/User-Agent").userAgent(userAgent).GET().body.as[String] should be(userAgent)
      }
    }

    it("should not send a Date header when none is specified") {
      request("header/Date").GET().status should be(Status.NotFound)
    }

    it("should send the correct Date header when specified") {
      forAll(simpleDate) { date =>
        Headers.DateFormat.parse(request("header/Date").date(date).GET().body.as[String]) should be(date)
      }
    }

    it("should not send a Range header when none is specified") {
      request("header/Range").GET().status should be(Status.NotFound)
      request("header/Range").range().GET().status should be(Status.NotFound)
    }

    it("should send the correct Range header when specified") {
      forAll(Gen.listOf(byteRange).suchThat(!_.isEmpty)) { ranges =>
        request("header/Range").range(ranges :_*).GET().body.as[String] should be("bytes=" + ranges.mkString(","))
      }
    }

    it("should send basic auth credentials properly") {
      forAll(authCredentials) {case (user, pwd) =>
        request("auth").auth(user, pwd)().body.as[String] should be(user + "\n" + pwd)
      }
    }
  }
}
