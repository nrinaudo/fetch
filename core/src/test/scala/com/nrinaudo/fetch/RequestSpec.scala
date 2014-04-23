package com.nrinaudo.fetch

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import com.nrinaudo.fetch.net.UrlEngine
import scala.Some

object RequestSpec {
  private val connegValue = "([^;]+)(?:;q=(.*))?".r

  /** Generates random, legal HTTP methods. */
  def httpMethod = Gen.oneOf("GET", "POST", "PUT", "DELETE", "OPTIONS", "TRACE", "PATCH", "LINK", "UNLINK")

  // Note that this is not entirely correct: according to the RFC, password are allowed to contain a ':'. This is not
  // properly handled in version 0.7.1 of unfiltered, however (the issue is fixed in github, but not yet released).
  def authCredentials = for {
    user <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
    pwd  <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
  } yield (user, pwd)
}

class RequestSpec extends FunSpec with BeforeAndAfterAll with Matchers with GeneratorDrivenPropertyChecks {
  import RequestSpec._
  import RequestEntitySpec._
  import ConnegSpec._



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
        request("method").method(method).apply().body.as[String] should be(method)
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

        if(resEncoding == Encoding.Identity) response.headers.get("Content-Encoding") should be(None)
        else                                 response.headers.get("Content-Encoding") should be(Some(List(resEncoding.name)))

        response.body.as[String] should be(entity.content)
      }
    }



    // - Header helpers ------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    def checkConneg[T](response: Response[ResponseEntity], values: List[Conneg[T]])(f: T => String) =
      response.body.as[String].split("\n").zip(values).foreach {
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

    it("should use the specified Accept-Charset header") {
      forAll(connegs(charset)) { charsets =>
        checkConneg(request("header/Accept-Charset").acceptCharset(charsets :_*).GET(), charsets)(_.name())
      }
    }

    it("should use the specified Accept-Language header") {
      forAll(connegs(language)) { languages =>
        checkConneg(request("header/Accept-Language").acceptLanguage(languages :_*).GET(), languages) { lang =>
          lang.getLanguage + (if(lang.getCountry.isEmpty) "" else "-" + lang.getCountry)
        }
      }
    }

    it("should use the specified Accept-Encoding header") {
      forAll(connegs(encoding)) { encodings =>
        checkConneg(request("header/Accept-Encoding").acceptEncoding(encodings :_*).GET(), encodings)(_.name)
      }
    }




    // - User agent ----------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should send the default User-Agent when not specified") {
      request("header/User-Agent").GET().body.as[String] should be(Request.UserAgent)
    }

    it("should send the correct User-Agent when specified") {
      forAll(Gen.identifier) { userAgent =>
        request("header/User-Agent").userAgent(userAgent).GET().body.as[String] should be(userAgent)
      }
    }


    // - BasicAuth -----------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should send basic auth credentials properly") {
      forAll(authCredentials) {case (user, pwd) =>
        request("auth").auth(user, pwd)().body.as[String] should be(user + "\n" + pwd)
      }
    }
  }
}
