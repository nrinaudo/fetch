package com.nrinaudo.fetch

import java.nio.charset.Charset

import fastparse._

/** Collection of implicit header formats for known content negotiation headers. */
object Conneg {
  /** Implicit format for the `Accept` content negotiation header. */
  implicit val MediaTypes: ValueFormat[Seq[Conneg[MediaType]]] = new ValueFormat[Seq[Conneg[MediaType]]] {
    override def write(value: Seq[Conneg[MediaType]]): Option[String] =
      if(value.isEmpty) None
      else Some(grammar.connegs(value.map {
        // TODO: this should not rely on toString but rather on serialization methods in grammar.
        case Conneg(t, q) => t.toString -> q
      }))
    override def read(value: String): Option[Seq[Conneg[MediaType]]] = parseFully(parser, value)
    val parser: Parser[List[Conneg[MediaType]]] = MediaType.parser.rep(",").map(_.map(m => Conneg(m.removeParam("q"), m.param[Float]("q").getOrElse(1F))).toList)
  }

  /** Implicit format for the `Accept-Encoding` content negotiation header.
    *
    * In its current version, this only supports known transfer encodings (`gzip`, `deflate` and `identity`).
    */
  implicit val Encodings: ValueFormat[Seq[Conneg[Encoding]]] = ConnegFormat(
    P(Encoding.Gzip.name).map(_ => Encoding.Gzip)       |
    P(Encoding.Deflate.name).map(_ => Encoding.Deflate) |
    P(Encoding.Identity.name).map(_ => Encoding.Identity), _.name
  )

  /** Implicit format for the `Accept-Charset` content negotiation header. */
  // TODO: check what happens when a valid token that does not map to a supported charset is passed.
  implicit val Charsets: ValueFormat[Seq[Conneg[Charset]]] = ConnegFormat(grammar.token.map(Charset.forName), _.name())

  /** Implicit format for the `Accept-Language` content negotiation header. */
  implicit val Languages: ValueFormat[Seq[Conneg[Language]]] = ConnegFormat(Language.parser,
    l => grammar.language(l.main, l.sub))


  private case class ConnegFormat[T](p: Parser[T], writer: T => String) extends ValueFormat[Seq[Conneg[T]]] {
    val parser: Parser[List[Conneg[T]]] = grammar.connegs(p).map(_.map { case (t, q) => Conneg(t, q) }.toList)

    override def write(value: Seq[Conneg[T]]): Option[String] =
      if(value.isEmpty) None
      else Some(grammar.connegs(value.map {
        case Conneg(t, q) => writer(t) -> q
      }))



    override def read(value: String): Option[Seq[Conneg[T]]] = parseFully(parser, value)
  }
}

/** Represents an acceptable value for content negotiation headers (`Accept*`).
  *
  * @param  value value of the header (see the companion object for header formats).
  * @param  q     weight of the value, as a float between 0 and 1 inclusive. Bigger weights tell remote servers that
  *               the corresponding value is more desirable than values associated with lower weights.
  */
case class Conneg[T](value: T, q: Float) {
  require(q >= 0F && q <= 1F, "q must be between 0 and 1, inclusive.")
}

