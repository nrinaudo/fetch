package com.nrinaudo.fetch

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import com.nrinaudo.fetch.net.UrlEngine
import Headers._
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.util.Success
import unfiltered.jetty.Server
import com.nrinaudo.fetch.Request._
import scala.Some
import scala.util.Success

object EngineSpec {
  private val connegValue = "([^;]+)(?:;q=(.*))?".r
}

trait EngineSpec extends FunSpec with BeforeAndAfterAll with Matchers with GeneratorDrivenPropertyChecks {
  import EngineSpec._
  import RequestSpec._
  import ByteRangeSpec._
  import RequestEntitySpec._
  import ConnegSpec._
  import EncodingSpec._
  import HeadersSpec._
  import ETagSpec._
  import MethodSpec._
  import scala.concurrent.ExecutionContext.Implicits.global
  import TestPlan._


  implicit def engine: HttpEngine
  def name: String


  // - Test HTTP server ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val server: Server = TestPlan.create

  override def beforeAll() {
    TestPlan.start
  }

  override def afterAll() {
    TestPlan.stop
  }



  // - Actual tests ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  describe(name) {
    // - Basic tests ---------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should use the specified HTTP method") {
      forAll(httpMethod) { method =>
        request("method").method(method).apply().body.as[String] should be(method.name)
      }
    }

    it("should extract the correct HTTP status codes") {
      forAll(Gen.choose(200, 599)) { code =>
        request("status/" + code).apply().status should be(Status(code))
      }
    }

    it("should correctly send custom HTTP headers when present") {
      forAll(Gen.identifier, Gen.identifier) { (name, value) =>
        request("header/" + name).header(name, value).GET.apply().body.as[String] should be(value)
      }
    }



    // - Entity submission / reception ---------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should correctly read and write entity bodies, regardless of the request and response encoding") {
      forAll(knownEntity, encoding, encoding) { (entity, reqEncoding, resEncoding) =>
        val response = request("body").acceptEncoding(resEncoding).PUT(entity.entity.encoding(reqEncoding))

        if(resEncoding == Encoding.Identity) response.contentEncoding should be(None)
        else                                 response.contentEncoding should be(Some(List(resEncoding)))

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
        checkConneg(request("header/Accept").accept(mimeTypes :_*).GET.apply(), mimeTypes) { mimeType =>
          mimeType.main + "/" + mimeType.sub
        }
      }
    }

    it("should send the default Accept header (*/*) when none is specified") {
      request("header/Accept").GET.apply().body.as[String] should be("*/*")
      request("header/Accept").accept().GET.apply().body.as[String] should be("*/*")
    }

    it("should use the specified Accept-Charset header") {
      forAll(connegs(charset)) { charsets =>
        checkConneg(request("header/Accept-Charset").acceptCharset(charsets :_*).GET.apply(), charsets)(_.name())
      }
    }

    it("should not send an Accept-Charset header when none is specified") {
      request("header/Accept-Charset").GET.apply().status should be(Status.NotFound)
      request("header/Accept-Charset").acceptCharset().GET.apply().status should be(Status.NotFound)
    }

    it("should use the specified Accept-Language header") {
      forAll(connegs(language)) { languages =>
        checkConneg(request("header/Accept-Language").acceptLanguage(languages :_*).GET.apply(), languages) { lang =>
          lang.getLanguage + (if(lang.getCountry.isEmpty) "" else "-" + lang.getCountry)
        }
      }
    }

    it("should not send an Accept-Language header when none is specified") {
      request("header/Accept-Language").GET.apply().status should be(Status.NotFound)
      request("header/Accept-Language").acceptLanguage().GET.apply().status should be(Status.NotFound)
    }

    it("should use the specified Accept-Encoding header") {
      forAll(connegs(encoding)) { encodings =>
        checkConneg(request("header/Accept-Encoding").acceptEncoding(encodings :_*).GET.apply(), encodings)(_.name)
      }
    }

    it("should not send an Accept-Encoding header when none is specified") {
      request("header/Accept-Encoding").GET.apply().status should be(Status.NotFound)
      request("header/Accept-Encoding").acceptEncoding().GET.apply().status should be(Status.NotFound)
    }

    it("should send the default User-Agent header when none is specified") {
      request("header/User-Agent").GET.apply().body.as[String] should be(Request.UserAgent)
    }

    it("should send the correct User-Agent header when specified") {
      // This suchThat statement looks odd, but it's not half as odd as unfiltered's behaviour: when it encounters a
      // User-Agent whose value is "te", it'll silently transform it to "TE", failing this test.
      forAll(Gen.identifier.suchThat(!_.equalsIgnoreCase("te"))) { userAgent =>
        request("header/User-Agent").userAgent(userAgent).GET.apply().body.as[String] should be(userAgent)
      }
    }

    it("should not send a Date header when none is specified") {
      request("header/Date").GET.apply().status should be(Status.NotFound)
    }

    it("should send the correct Date header when specified") {
      forAll(date) { date =>
        DateFormat.read(request("header/Date").date(date).GET.apply().body.as[String]) should be(Success(date))
      }
    }

    it("should not send a Max-Forwards header when none is specified") {
      request("header/MaxForwards").GET.apply().status should be(Status.NotFound)
    }

    it("should send the correct Max-Forwards header when specified") {
      forAll(arbitrary[Int].suchThat(_ >= 0)) { value =>
        request("header/Max-Forwards").maxForwards(value).GET.apply().body.as[Int] should be(value)
      }
    }

    it("should not send a If-Modified-Since header when none is specified") {
      request("header/If-Modified-Since").GET.apply().status should be(Status.NotFound)
    }

    it("should send the correct If-Modified-Since header when specified") {
      forAll(date) { date =>
        DateFormat.read(request("header/If-Modified-Since").ifModifiedSince(date).GET.apply().body.as[String]) should be(Success(date))
      }
    }

    it("should not send a If-Unmodified-Since header when none is specified") {
      request("header/If-Unmodified-Since").GET.apply().status should be(Status.NotFound)
    }

    it("should send the correct If-Unmodified-Since header when specified") {
      forAll(date) { date =>
        DateFormat.read(request("header/If-Unmodified-Since").ifUnmodifiedSince(date).GET.apply().body.as[String]) should be(Success(date))
      }
    }

    it("should not send a Range header when none is specified") {
      request("header/Range").GET.apply().status should be(Status.NotFound)
      request("header/Range").range().GET.apply().status should be(Status.NotFound)
    }

    it("should send the correct Range header when specified") {
      forAll(byteRanges) { ranges =>
        request("header/Range").range(ranges :_*).GET.apply().body.as[String] should be("bytes=" + ranges.mkString(","))
      }
    }

    it("should send the correct If-Match header when specified") {
      forAll(etags) { tags =>
        request("header/If-Match").ifMatch(tags :_*).GET.apply().body.as[String] should be(tags.mkString(","))
      }
    }

    it("should not send an If-Match header when none is specified") {
      request("header/If-Match").GET.apply().status should be(Status.NotFound)
      request("header/If-Match").ifMatch().GET.apply().status should be(Status.NotFound)
    }

    it("should send the correct If-None-Match header when specified") {
      forAll(etags) { tags =>
        request("header/If-None-Match").ifNoneMatch(tags :_*).GET.apply().body.as[String] should be(tags.mkString(","))
      }
    }

    it("should not send an If-None-Match header when none is specified") {
      request("header/If-None-Match").GET.apply().status should be(Status.NotFound)
      request("header/If-None-Match").ifNoneMatch().GET.apply().status should be(Status.NotFound)
    }

    it("should send the correct If-Range header when specified") {
      forAll(etag) { tag =>
        ETag(request("header/If-Range").ifRange(tag).GET.apply().body.as[String]) should be(tag)
      }

      forAll(date) { date =>
        DateFormat.read(request("header/If-Range").ifRange(date).GET.apply().body.as[String]) should be(Success(date))
      }
    }

    it("should not send an If-Range header when none is specified") {
      request("header/If-Range").GET.apply().status should be(Status.NotFound)
    }

    it("should send basic auth credentials properly") {
      forAll(authCredentials) {case (user, pwd) =>
        request("auth").auth(user, pwd).apply().body.as[String] should be(user + "\n" + pwd)
      }
    }
  }
}
