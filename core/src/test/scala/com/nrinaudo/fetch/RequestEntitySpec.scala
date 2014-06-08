package com.nrinaudo.fetch

import java.io._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.io.Source

object RequestEntitySpec {
  private val tmpFiles = new ThreadLocal[File] {
    override def initialValue(): File = {
      val f = File.createTempFile("fetch", "request")
      f.deleteOnExit()
      f
    }
  }

  /** Generates a temporary file containing the specified content (UTF-8 encoded). */
  def tmpFile(content: String): File = {
    val file = tmpFiles.get

    val out = new OutputStreamWriter(new FileOutputStream(file), DefaultCharset)
    try {out.write(content)}
    finally {out.close()}

    file
  }

  /** Represents an entity and its expected content one serialized.
    *
    * This is meant for tests where an entity is written to and sent back by a remote server and needs to be compared
    * with its original value.
    */
  case class KnownEntity(content: String, entity: RequestEntity)

  def knownEntity: Gen[KnownEntity] = for {
    content <- arbitrary[String].suchThat(!_.isEmpty)
    impl    <- Gen.choose(0, 5)
  } yield KnownEntity(content, (impl match {
      case 0 => RequestEntity.bytes(out => out.write(content.getBytes(DefaultCharset)))
      case 1 => RequestEntity.chars(out => out.write(content))
      case 2 => RequestEntity(new ByteArrayInputStream(content.getBytes(DefaultCharset)))
      case 3 => RequestEntity(new StringReader(content))
      case 4 => RequestEntity(content)
      case 5 => RequestEntity(tmpFile(content))
      case e => throw new AssertionError("Unexpected rand(0, 5) value: " + e)
    }).mediaType(MediaType.PlainText.charset(DefaultCharset)))

  def entity: Gen[RequestEntity] = knownEntity.map(_.entity)
}

class RequestEntitySpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import RequestEntitySpec._

  describe("A StringEntity instance") {
    // This does not look terribly useful, but I've been known to mess my string patterns in toString methods and
    // discover it much too late.
    it("should have a working toString method") {
      forAll(arbitrary[String]) { s => RequestEntity(s).toString }
    }
  }

  describe("A FileEntity instance") {
    // This does not look terribly useful, but I've been known to mess my string patterns in toString methods and
    // discover it much too late.
    it("should have a working toString method") {
      forAll(arbitrary[String]) { s => RequestEntity(tmpFile(s)).toString }
    }
  }

  describe("A RequestEntity instance") {
    it("should have a working gzip method") {
      forAll(entity) { _.gzip.encoding should be(Encoding.Gzip) }
    }

    it("should have a working deflate method") {
      forAll(entity) { _.deflate.encoding should be(Encoding.Deflate) }
    }

    it("should have a working encoding method") {
      forAll(entity, EncodingSpec.encoding) { (entity, encoding) =>
        entity.encoding(encoding).encoding should be(encoding)
      }
    }

    it("should have a working mediaType method") {
      forAll(entity, MediaTypeSpec.mediaType) { (entity, mediaType) =>
        entity.mediaType(mediaType).mediaType should be(mediaType)
      }
    }

    it("should always have an unknown content length when using a non-identity encoding") {
      forAll(Gen.oneOf(Encoding.Gzip, Encoding.Deflate), Gen.identifier) { (encoding, content) =>
        val entity = RequestEntity(content)

        entity.contentLength.isDefined should be(true)
        entity.length.isDefined should be(true)

        entity.encoding(encoding).length.isDefined should be(true)
        entity.encoding(encoding).contentLength.isDefined should be(false)
      }
    }

    it("should write itself correctly") {
      forAll(knownEntity, EncodingSpec.encoding) { (entity, encoding) =>
        val out = new ByteArrayOutputStream()
        entity.entity.encoding(encoding).apply(out)

        Source.fromInputStream(encoding.decode(new ByteArrayInputStream(out.toByteArray)), DefaultCharset.name()).mkString should be(entity.content)
      }
    }
  }
}