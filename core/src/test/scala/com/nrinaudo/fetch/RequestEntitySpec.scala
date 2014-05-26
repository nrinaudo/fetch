package com.nrinaudo.fetch

import java.io._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.FunSpec

object RequestEntitySpec {
  // Temporary file used to store request entities.
  private lazy val tmpFile: File = {
    val f = File.createTempFile("fetch", "request")
    f.deleteOnExit()
    f
  }

  /** Generates a temporary file containing the specified content (UTF-8 encoded). */
  def tmpFile(content: String): File = {
    val out = new OutputStreamWriter(new FileOutputStream(tmpFile), DefaultCharset)
    try {out.write(content)}
    finally {out.close()}

    tmpFile
  }

  /** Represents an entity and its expected content one serialized.
    *
    * This is meant for tests where an entity is written to and sent back by a remote server and needs to be compared
    * with its original value.
    */
  case class KnownEntity(content: String, entity: RequestEntity)

  def entity: Gen[KnownEntity] = for {
    content <- arbitrary[String].suchThat(!_.isEmpty)
    impl    <- Gen.choose(0, 5)
  } yield KnownEntity(content, (impl match {
      case 0 => RequestEntity.bytes(out => out.write(content.getBytes(DefaultCharset)))
      case 1 => RequestEntity.chars(out => out.write(content))
      case 2 => RequestEntity(new ByteArrayInputStream(content.getBytes(DefaultCharset)))
      case 3 => RequestEntity(new StringReader(content))
      case 4 => RequestEntity(content)
      case 5 => RequestEntity(content)
      case e => throw new AssertionError("Unexpected rand(0, 5) value: " + e)
    }).mimeType(MimeType.TextPlain.charset(DefaultCharset)))
}
