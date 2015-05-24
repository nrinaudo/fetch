package com.nrinaudo.fetch

import java.util.Date

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class ResponseSpec extends FunSpec with BeforeAndAfterAll with Matchers with GeneratorDrivenPropertyChecks {
  import HeadersSpec._
  import Headers._
  import ETagSpec._
  import MethodSpec._
  import LanguageSpec._
  import EncodingSpec._


  val response = new Response[String](Status.Ok, Headers.empty, "test")

  describe("A Response") {
    it("should have the expected Date header when set") {
      forAll { date: Date =>
        response.copy(headers = response.headers.set("Date", date)).date should be(Some(date))
      }
    }

    it("should not have a Date header when not set") { response.date should be(None) }

    it("should have the expected Last-Modified header") {
      forAll { date: Date =>
        response.copy(headers = response.headers.set("Last-Modified", date)).lastModified should be(Some(date))
      }
    }

    it("should not have a Last-Modified header when not set") { response.lastModified should be(None) }

    it("should have the expected Expires header") {
      forAll { date: Date =>
        response.copy(headers = response.headers.set("Expires", date)).expires should be(Some(date))
      }
    }

    it("should not have an Expire header when not set") { response.expires should be(None) }

    it("should have the expected Content-Language header") {
      forAll { lang: Language =>
        response.copy(headers = response.headers.set("Content-Language", lang)).contentLanguage should be(Some(List(lang)))
      }
    }

    it("should not have a Content-Language header when not set") { response.contentLanguage should be(None) }

    it("should have the expected ETag header") {
      forAll { etag: ETag =>
        response.copy(headers = response.headers.set("ETag", etag)).etag should be(Some(etag))
      }
    }

    it("should not have an Etag header when not set") { response.etag should be(None) }

    it("should have the expected Server header") {
      forAll(Gen.identifier) { id =>
        response.copy(headers = response.headers.set("Server", id)).server should be(Some(id))
      }
    }

    it("should not have a Server header when not set") { response.server should be(None) }

    it("should have the expected Allow header") {
      forAll { methods: List[Method] =>
        response.copy(headers = response.headers.set("Allow", methods.map(_.name).mkString(","))).allow should be(Some(methods))
      }
    }

    it("should not have an Allow header when not set") { response.allow should be(None) }

    it("should have the expected Age header") {
      forAll(Gen.choose(1, 1000)) { age =>
        response.copy(headers = response.headers.set("Age", age)).age should be(Some(age))
      }
    }

    it("should not have an Age header when not set") { response.age should be(None) }

    it("should have the expected Content-Encoding header") {
      forAll { encoding: Encoding =>
        response.copy(headers = response.headers.set("Content-Encoding", encoding)).contentEncoding should be(Some(List(encoding)))
      }
    }

    it("should not have a Content-Encoding header when not set") { response.contentEncoding should be(None) }


    // TODO: implement
    /*
    it("should have the expected wwwAuthenticate header") {
    }
    */
  }
}
