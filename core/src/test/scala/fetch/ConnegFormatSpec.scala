package fetch

import java.nio.charset.Charset

import fetch.Conneg._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import fetch.Generators._

class ConnegFormatSpec[T](override val illegalT: Gen[String])(implicit arb: Arbitrary[T],
                                                              writerT: ValueWriter[T],
                                                              override val reader: ValueReader[Seq[Conneg[T]]],
                                                              override val writer: ValueWriter[Seq[Conneg[T]]])
  extends ValueFormatSpec.WithErrors[Seq[Conneg[T]]] {
  override implicit def arbT = connegs[T]

  it("should refuse illegal value of q") {
    forAll(arbitrary[T], illegalQ) { (t, q) =>
      intercept[IllegalArgumentException] {Conneg(t, q)}
      ()
    }
  }

  it("should accept legal values of q") {
    forAll(arbitrary[T], q) { (t, q) => Conneg(t, q); () }
  }


  it("should not serialize q when it's equal to 1") {
    forAll { t: T => writer.write(Seq(Conneg(t, 1F))) should be(writerT.write(t)) }
  }

  it("should assume an absent q defaults to 1.0") {
    forAll { t: T => reader.read(writerT.write(t).get) should be(Some(List(Conneg(t, 1F)))) }
  }
}

class CharsetConnegFormatSpec extends ConnegFormatSpec[Charset](illegalCharsets)
class LanguageConnegFormatSpec extends ConnegFormatSpec[Language](illegalLanguages)
class EncodingConnegFormatSpec extends ConnegFormatSpec[Encoding](illegalEncodings)
class MediaTypeConnegFormatSpec extends ConnegFormatSpec[MediaType](illegalMediaTypes)
