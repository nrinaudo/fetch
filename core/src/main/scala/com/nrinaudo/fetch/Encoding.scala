package com.nrinaudo.fetch

import java.io.{InputStream, OutputStream}
import java.util.zip._

object Encoding {
  // - Standard implementations ----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Transparent, no-op encoding. */
  val Identity = apply("identity", out => out, in => in)

  /** Encoding that gzips and gunzips content. */
  val Gzip = apply("gzip", out => new GZIPOutputStream(out), in => new GZIPInputStream(in))

  /** Encoding that inflates and deflates content. */
  val Deflate = apply("deflate", out => new DeflaterOutputStream(out), in => new InflaterInputStream(in))

  /** Registry of known content encodings. */
  val DefaultEncodings: Map[String, Encoding] = Map(Identity.name -> Identity) + (Gzip.name -> Gzip) +
                                                (Deflate.name -> Deflate)



  // - Creation helpers ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private class EncodingImpl(override val name: String, e: OutputStream => OutputStream,
                             d: InputStream => InputStream) extends Encoding {
    override def decode(in: InputStream): InputStream = d(in)
    override def encode(out: OutputStream): OutputStream = e(out)
  }

  /** Creates a new content encoding with the specified decoding and encoding functions. */
  def apply(name: String, e: OutputStream => OutputStream, d: InputStream => InputStream): Encoding =
    new EncodingImpl(name, e, d)
}

/** Handles HTTP [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 content encoding]]. */
trait Encoding {
  /** Name of the encoding, as recognised by HTTP clients and servers. */
  val name: String

  /** Wraps the specified stream in an encoded one. */
  def encode(out: OutputStream): OutputStream

  /** Wraps the specified stream in a decoded one. */
  def decode(in: InputStream): InputStream

  override def toString: String = name
}

