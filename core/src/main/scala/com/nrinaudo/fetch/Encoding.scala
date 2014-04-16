package com.nrinaudo.fetch

import java.io.{InputStream, OutputStream}
import java.util.zip._

object Encoding {
  val Identity = apply("identity", (out) => out, (in) => in)
  val Gzip = apply("gzip", (out) => new GZIPOutputStream(out), (in) => new GZIPInputStream(in))
  val Deflate = apply("deflate", (out) => new DeflaterOutputStream(out), (in) => new InflaterInputStream(in))

  private class EncodingImpl(override val name: String, e: (OutputStream) => OutputStream,
                             d: (InputStream) => InputStream) extends Encoding {
    override def decode(in: InputStream): InputStream = d(in)
    override def encode(out: OutputStream): OutputStream = e(out)
    override def toString = name
  }

  def apply(name: String, e: (OutputStream) => OutputStream, d: (InputStream) => InputStream): Encoding =
    new EncodingImpl(name, e, d)
}

trait Encoding {
  def name: String
  def encode(out: OutputStream): OutputStream
  def decode(in: InputStream): InputStream
}

