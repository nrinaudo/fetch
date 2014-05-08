package com.nrinaudo.fetch

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import unfiltered.jetty.Server
import com.nrinaudo.fetch.net.UrlEngine
import org.scalacheck.Gen

class ResponseSpec extends FunSpec with BeforeAndAfterAll with Matchers with GeneratorDrivenPropertyChecks {
  import HeaderSpec._
  import TestPlan._
  import HeaderFormat._
  import ETagSpec._
  import scala.concurrent.ExecutionContext.Implicits.global
  import RequestSpec._
  import MethodSpec._
  import ConnegSpec._



  // - Test HTTP server ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val server: Server = TestPlan.create
  implicit val engine = UrlEngine()

  override def beforeAll() {
    TestPlan.start
  }

  override def afterAll() {
    TestPlan.stop
  }



  // - Actual tests ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  describe("A Response") {
    it("should have the expected Date header") {
      forAll(date) { date =>
        await(request("header/Date").POST.apply(DateFormat.write(date).get)).date should be(Some(date))
      }
    }

    it("should have the expected Last-Modified header") {
      forAll(date) { date =>
        await(request("header/Last-Modified").POST.apply(DateFormat.write(date).get)).lastModified should be(Some(date))
      }
    }

    it("should have the expected Expires header") {
      forAll(date) { date =>
        await(request("header/Expires").POST.apply(DateFormat.write(date).get)).expires should be(Some(date))
      }
    }

    it("should have the expected Content-Language header") {
      forAll(language) { lang =>
        await(request("header/Content-Language").POST.apply(LanguageFormat.write(lang).get)).contentLanguage should be(Some(List(lang)))
      }
    }

    it("should have the expected ETag header") {
      forAll(etag) { etag =>
        await(request("header/ETag").POST.apply(ETagFormat.write(etag).get)).etag should be(Some(etag))
      }
    }

    it("should have the expected Server header") {
      forAll(Gen.identifier) { id =>
        await(request("header/Server").POST.apply(id)).server should be(Some(id))
      }
    }

    it("should have the expected Allow header") {
      forAll(httpMethods) { methods =>
        await(request("header/Allow").POST.apply(methods.map(_.name).mkString(","))).allow should be(Some(methods))
      }
    }

    it("should have the expected Age header") {
      forAll(Gen.choose(1, 1000)) { age =>
        await(request("header/Age").POST.apply(age.toString)).age should be(Some(age))
      }
    }

    // TODO: this is a bit tricky, as the presence of these headers have an impact on response parsing.
    /*
    it("should have the expected wwwAuthenticate header") {
    }

    it("should have the expected Content-Encoding header") {
    }
    */
  }
}
