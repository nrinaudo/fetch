package com.nrinaudo.fetch

import java.util.Date

import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.nrinaudo.fetch.Headers._
import java.nio.charset.Charset

object HeaderFormatSpec {
  def cycle[T](format: ValueFormat[T], value: T) = format.read(format.write(value).get)
}

class HeaderFormatSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HeadersSpec._
  import HeaderFormatSpec._
  import MethodSpec._
  import ConnegSpec._
  import LanguageSpec._
  import MediaTypeSpec._
  import EncodingSpec._
  import ByteRangeSpec._

  def validate[T](format: ValueFormat[T], value: T) = cycle(format, value) should be(Some(value))

  describe("DateFormat") {
    it("should correctly serialize and parse dates") {
      forAll { date: Date => validate(dateHeader, date)}
    }

    it("should refuse illegal dates") {
      forAll(illegalDate) { str => dateHeader.read(str).isEmpty should be(true)}
    }
  }

  describe("LanguageFormat") {
    it("should correctly serialize and parse languages") {
      forAll { lang: Language => validate(languageHeader, lang)}
    }

    it("should correctly serialize and parse lists of languages") {
      forAll(nonEmptyListOf(arbitrary[Language])) { langs => validate(compositeFormat[Language], langs)}
    }

    it("should refuse illegal languages") {
      forAll(illegalLanguage) { str => languageHeader.read(str).isEmpty should be(true)}
    }
  }

  describe("CharsetFormat") {
    it("should correctly serialize and parse charsets") {
      forAll { charset: Charset => validate(charsetHeader, charset)}
    }

    it("should correctly serialize and parse lists of charsets") {
      forAll(nonEmptyListOf(arbitrary[Charset])) { charsets => validate(compositeFormat[Charset], charsets)}
    }

    it("should refuse illegal charsets") {
      forAll(illegalCharset) { str => charsetHeader.read(str).isEmpty should be(true) }
    }
  }

  describe("MediaTypeFormat") {
    it("should correctly serialize and parse media types") {
      forAll { mediaType: MediaType => validate(mediaTypeHeader, mediaType)}
    }

    it("should refuse illegal media types") {
      forAll(illegalMediaType) { str => mediaTypeHeader.read(str).isEmpty should be(true) }
    }
  }

  describe("EncodingFormat") {
    it("should correctly serialize and parse encodings") {
      forAll { encoding: Encoding => validate(encodingHeader, encoding)}
    }

    it("should correctly serialize and parse lists of encodings") {
      forAll(nonEmptyListOf(arbitrary[Encoding])) { encodings => validate(compositeFormat[Encoding], encodings)}
    }

    it("should refuse illegal encodings") {
      forAll(illegalEncoding) { str =>
        encodingHeader.read(str).isEmpty should be(true)
      }
    }
  }

  describe("ByteRangeFormat") {
    it("should correctly serialize and parse byte ranges") {
      forAll { range: ByteRange => validate(byteRangeHeader, range)}
    }

    it("should refuse illegal byte ranges") {
      forAll(illegalRange) { str => byteRangeHeader.read(str).isEmpty should be(true) }
    }
  }

  describe("ByteRangesFormat") {
    it("should correctly serialize and parse byte range lists") {
      forAll(nonEmptyListOf(arbitrary[ByteRange])) { ranges => validate(byteRangesHeader, ranges)}
    }

    it("should refuse illegal lists of byte ranges") {
      forAll(illegalRanges) { str => byteRangesHeader.read(str).isEmpty should be(true) }
    }
  }

  describe("MethodFormat") {
    it("should correctly serialize and parse methods") {
      forAll { method: Method => validate(methodHeader, method)}
    }

    it("should correctly serialize and parse lists of byte ranges") {
      forAll(nonEmptyListOf(arbitrary[Method])) { methods => validate(compositeFormat[Method], methods)}
    }

    it("should refuse illegal methods") {
      forAll(illegalMethod) { method =>
        methodHeader.read(method).isEmpty should be(true)
      }
    }
  }
}