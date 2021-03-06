package com.nrinaudo.fetch

import org.scalacheck.Gen._
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
      forAll(date) { date => validate(DateFormat, date)}
    }

    it("should refuse illegal dates") {
      forAll(illegalDate) { str => DateFormat.read(str).isEmpty should be(true)}
    }
  }

  describe("LanguageFormat") {
    it("should correctly serialize and parse languages") {
      forAll(language) { lang => validate(LanguageFormat, lang)}
    }

    it("should correctly serialize and parse lists of languages") {
      forAll(nonEmptyListOf(language)) { langs => validate(compositeFormat[Language], langs)}
    }

    it("should refuse illegal languages") {
      forAll(illegalLanguage) { str => LanguageFormat.read(str).isEmpty should be(true)}
    }
  }

  describe("CharsetFormat") {
    it("should correctly serialize and parse charsets") {
      forAll(charset) { charset => validate(CharsetFormat, charset)}
    }

    it("should correctly serialize and parse lists of charsets") {
      forAll(nonEmptyListOf(charset)) { charsets => validate(compositeFormat[Charset], charsets)}
    }

    it("should refuse illegal charsets") {
      forAll(illegalCharset) { str => CharsetFormat.read(str).isEmpty should be(true) }
    }
  }

  describe("MediaTypeFormat") {
    it("should correctly serialize and parse media types") {
      forAll(mediaType) { mediaType => validate(MediaTypeFormat, mediaType)}
    }

    it("should refuse illegal media types") {
      forAll(illegalMediaType) { str => MediaTypeFormat.read(str).isEmpty should be(true) }
    }
  }

  describe("EncodingFormat") {
    it("should correctly serialize and parse encodings") {
      forAll(encoding) { encoding => validate(EncodingFormat, encoding)}
    }

    it("should correctly serialize and parse lists of encodings") {
      forAll(nonEmptyListOf(encoding)) { encodings => validate(compositeFormat[Encoding], encodings)}
    }

    it("should refuse illegal encodings") {
      forAll(illegalEncoding) { str =>
        EncodingFormat.read(str).isEmpty should be(true)
      }
    }
  }

  describe("ByteRangeFormat") {
    it("should correctly serialize and parse byte ranges") {
      forAll(byteRange) { range => validate(ByteRangeFormat, range)}
    }

    it("should refuse illegal byte ranges") {
      forAll(illegalRange) { str => ByteRangeFormat.read(str).isEmpty should be(true) }
    }
  }

  describe("ByteRangesFormat") {
    it("should correctly serialize and parse byte range lists") {
      forAll(nonEmptyListOf(byteRange)) { ranges => validate(ByteRangesFormat, ranges)}
    }

    it("should refuse illegal lists of byte ranges") {
      forAll(illegalRanges) { str => ByteRangesFormat.read(str).isEmpty should be(true) }
    }
  }

  describe("MethodFormat") {
    it("should correctly serialize and parse methods") {
      forAll(httpMethod) { method => validate(MethodFormat, method)}
    }

    it("should correctly serialize and parse lists of byte ranges") {
      forAll(nonEmptyListOf(httpMethod)) { methods => validate(compositeFormat[Method], methods)}
    }

    it("should refuse illegal methods") {
      forAll(illegalMethod) { method =>
        MethodFormat.read(method).isEmpty should be(true)
      }
    }
  }
}