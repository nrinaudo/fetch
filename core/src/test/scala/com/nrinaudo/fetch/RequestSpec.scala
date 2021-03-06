package com.nrinaudo.fetch

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import Gen._
import com.nrinaudo.fetch.net.UrlEngine

object RequestSpec {
  // Note that this is not entirely correct: according to the RFC, password are allowed to contain a ':'. This is not
  // properly handled in version 0.7.1 of unfiltered, however (the issue is fixed in github, but not yet released).
  def authCredentials = for {
    user <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
    pwd  <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
  } yield (user, pwd)
}

class RequestSpec extends FunSpec with BeforeAndAfterAll with Matchers with GeneratorDrivenPropertyChecks {
  import MethodSpec._
  import UrlSpec._
  import QueryStringSpec._
  import HeadersSpec._
  import ByteRangeSpec._
  import ETagSpec._
  import ConnegSpec._
  import LanguageSpec._
  import EncodingSpec._
  import MediaTypeSpec._

  implicit val engine = UrlEngine()

  describe("A Request") {
    // - Method tests --------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should have a working 'method' method") {
      forAll(httpMethod, url) { (method, url) => Request(url).method(method).method should be(method) }
    }

    it("should have working method helpers") {
      forAll(url) { url =>
        val request = Request(url)

        request.GET.method should be(Method.GET)
        request.POST.method should be(Method.POST)
        request.PUT.method should be(Method.PUT)
        request.DELETE.method should be(Method.DELETE)
        request.HEAD.method should be(Method.HEAD)
        request.OPTIONS.method should be(Method.OPTIONS)
        request.TRACE.method should be(Method.TRACE)
        request.CONNECT.method should be(Method.CONNECT)
        request.PATCH.method should be(Method.PATCH)
        request.LINK.method should be(Method.LINK)
        request.UNLINK.method should be(Method.UNLINK)
      }
    }



    // - Url tests -----------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should have a working url method") {
      forAll(url, url) { (url1, url2) => Request(url1).url(url2).url should be(url2) }
    }

    it("should have a working / method") {
      forAll(url, segment) { (url, segment) => (Request(url) / segment).url should be(url / segment) }
    }

    it("should have a working ? method") {
      forAll(url, query) { (url, query) => (Request(url) ? query).url should be(url ? query) }
    }

    it("should have a working & method") {
      import QueryString._
      forAll(url, identifier, arbitrary[String]) { (url, name, value) =>
        (Request(url) & name -> value).url should be(url & name -> value)
      }
    }



    // - Headers tests -------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should return its Max-Forwards header when set") {
      forAll(url, posNum[Int]) { (url, max) => Request(url).maxForwards(max).maxForwards should be(Some(max)) }
    }

    it("should not return a Max-Forwards when the header is not set") {
      forAll(url) { url => Request(url).maxForwards should be(None) }
    }

    it("should return its Date header when set") {
      forAll(url, date) { (url, date) => Request(url).date(date).date should be(Some(date)) }
    }

    it("should not return a Date when the header is not set") {
      forAll(url) { url => Request(url).date should be(None) }
    }

    it("should return its Range header when set") {
      forAll(url, byteRanges) { (url, ranges) => Request(url).range(ranges: _*).range should be(Some(ranges)) }
    }

    it("should not return a Range when the header is not set") {
      forAll(url) { url => Request(url).range should be(None) }
    }

    it("should return its User-Agent header when set") {
      forAll(url, identifier) { (url, id) => Request(url).userAgent(id).userAgent should be(Some(id)) }
    }

    it("should not return a User-Agent when the header is not set") {
      forAll(url) { url => Request(url).userAgent should be(None) }
    }

    it("should return its If-Modified-Since header when set") {
      forAll(url, date) { (url, date) => Request(url).ifModifiedSince(date).ifModifiedSince should be(Some(date)) }
    }

    it("should not return a If-Modified-Since when the header is not set") {
      forAll(url) { url => Request(url).ifModifiedSince should be(None) }
    }

    it("should return its If-Unmodified-Since header when set") {
      forAll(url, date) { (url, date) => Request(url).ifUnmodifiedSince(date).ifUnmodifiedSince should be(Some(date)) }
    }

    it("should not return a If-Unmodified-Since when the header is not set") {
      forAll(url) { url => Request(url).ifUnmodifiedSince should be(None) }
    }

    it("should return its If-None-Match header when set") {
      forAll(url, etags) { (url, etags) => Request(url).ifNoneMatch(etags: _*).ifNoneMatch should be(Some(etags)) }
    }

    it("should not return a If-None-Match when the header is not set") {
      forAll(url) { url => Request(url).ifNoneMatch should be(None) }
    }

    it("should return its If-Match header when set") {
      forAll(url, etags) { (url, etags) => Request(url).ifMatch(etags: _*).ifMatch should be(Some(etags)) }
    }

    it("should not return a If-Match when the header is not set") {
      forAll(url) { url => Request(url).ifMatch should be(None) }
    }

    it("should return its Accept-Encoding header when set") {
      forAll(url, connegs(encoding)) { (url, encodings) =>
        Request(url).acceptEncoding(encodings: _*).acceptEncoding should be(Some(encodings))
      }
    }

    it("should have working acceptEncoding helpers") {
      forAll(url) { url =>
        Request(url).acceptGzip.acceptEncoding should be(Some(List(Conneg(Encoding.Gzip))))
        Request(url).acceptDeflate.acceptEncoding should be(Some(List(Conneg(Encoding.Deflate))))
      }
    }

    it("should not return a Accept-Encoding when the header is not set") {
      forAll(url) { url => Request(url).acceptEncoding should be(None) }
    }

    it("should return its Accept header when set") {
      forAll(url, connegs(mediaType)) { (url, types) => Request(url).accept(types: _*).accept should be(Some(types)) }
    }

    it("should not return a Accept when the header is not set") {
      forAll(url) { url => Request(url).accept should be(None) }
    }

    it("should return its Accept-Charset header when set") {
      forAll(url, connegs(charset)) { (url, charsets) =>
        Request(url).acceptCharset(charsets: _*).acceptCharset should be(Some(charsets))
      }
    }

    it("should not return a Accept-Charset when the header is not set") {
      forAll(url) { url => Request(url).acceptCharset should be(None) }
    }

    it("should return its Accept-Language header when set") {
      forAll(url, connegs(language)) { (url, languages) =>
        Request(url).acceptLanguage(languages: _*).acceptLanguage should be(Some(languages))
      }
    }

    it("should not return a Accept-Language when the header is not set") {
      forAll(url) { url => Request(url).acceptLanguage should be(None) }
    }
  }
}
