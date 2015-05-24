package com.nrinaudo.fetch

import java.nio.charset.Charset

import com.nrinaudo.fetch.MediaTypeParameters._

/** Defines [[MediaType]] implementations as well as known types. */
object MediaType {
  // - MediaType implementations ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Specific media type, with both a main- and sub-type (such as `text-plain`, for example). */
  final case class Specific(main: String, sub: String, params: MediaTypeParameters = MediaTypeParameters.empty)
    extends MediaType {
    override def rawType: String = "%s/%s" format (main, sub)
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)

    /** Matches any instance of [[Specific]] with the same main- and sub-type. */
    override def unapply(mediaType:  MediaType): Option[MediaType] = mediaType match {
      case Specific(m, s, _) if main == m && sub == s => Some(mediaType)
      case _                                          => None
    }
  }

  /** Media range, with a main-type only (such as `text/ *`, for example). */
  final case class Range(main: String, params: MediaTypeParameters = MediaTypeParameters.empty) extends MediaType {
    override def rawType: String = "%s/*" format main
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)

    /** Matches any instance of either [[Specific]] or [[Range]] that have the same main type. */
    override def unapply(mediaType: MediaType): Option[MediaType] = mediaType match {
      case Specific(m, s, _) if m == main => Some(mediaType)
      case Range(m, _) if m == main       => Some(mediaType)
      case _                              => None
    }

    def sub(value: String): MediaType = Specific(main, value, params)
    def /(value: String): MediaType = sub(value)
  }

  /** All media types: `* / *` */
  final case class All(params: MediaTypeParameters = MediaTypeParameters.empty) extends MediaType {
    override def rawType: String = "*/*"
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)

    /** Matches all media types. */
    override def unapply(mediaType: MediaType): Option[MediaType] = Some(mediaType)
  }



  // - Parsing ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Defines parses for the media type syntax. */
  trait Grammar extends HttpGrammar {
    /** Parses `* / *`. */
    def all: Parser[MediaType]      = "*" ~ "/" ~ "*" ^^ { case _ => Everything }
    /** Parses `main/ *`. */
    def range: Parser[MediaType]    = token <~ ("/" ~ "*") ^^ { case main => Range(main) }
    /** Parses `main/sub`. */
    def specific: Parser[MediaType] = (token <~ "/") ~ token ^^ { case (main ~ sub) => Specific(main, sub) }

    /** Parses a media type and its parameters. */
    def mediaType: Parser[MediaType] = (all | range | specific) ~ opt(paramSep ~> parameters) ^^ {
      case media ~ None         => media
      case media ~ Some(params) => media.params(new MediaTypeParameters(params))
    }
  }

  private object Format extends Grammar {
    def apply(string: String): Option[MediaType] = parseAll(mediaType, string).map(Some(_)).getOrElse(None)
  }

  /** Attempts to extract a media type from the specified string. */
  def parse(str: String): Option[MediaType] = Format(str)



  // - Known types -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val Everything  = All()

  // Application types
  val Application = Range("application")
  val Atom        = Application / "atom+xml"
  val EcmaScript  = Application / "ecmascript"
  val Json        = Application / "json"
  val JavaScript  = Application / "javascript"
  val OctetStream = Application / "octet-stream"
  val Pdf         = Application / "pdf"
  val PostScript  = Application / "postscript"
  val RdfXml      = Application / "rdf+xml"
  val RssXml      = Application / "rss+xml"
  val SoapXml     = Application / "soap+xml"
  val Woff        = Application / "font-woff"
  val Xhtml       = Application / "xhtml+xml"
  val Xml         = Application / "xml"
  val Dtd         = Application / "xml-dtd"
  val Zip         = Application / "zip"
  val Gzip        = Application / "gzip"

  // Image types
  val Image = Range("image")
  val Gif   = Image / "gif"
  val Jpeg  = Image / "jpeg"
  val Png   = Image / "png"
  val Svg   = Image / "svg+xml"

  // Text types
  val Text      = Range("text")
  val Css       = Text / "css"
  val Csv       = Text / "csv"
  val Html      = Text / "html"
  val PlainText = Text / "plain"
  val Rtf       = Text / "rtf"
  val Vcard     = Text / "vcard"
}

sealed trait MediaType {
  /** Parameters associated with this instance. */
  val params: MediaTypeParameters
  /** Raw string representation of the media type, ignoring parameters. */
  def rawType: String
  /** Creates a copy of the current instance with the specified parameters. */
  def params(values: MediaTypeParameters): MediaType

  override lazy val toString =
    if(params.values.isEmpty) rawType
    else                      rawType + ";" + params.toString

  /** Allows instances of [[MediaType]] to be used in pattern matching.
    *
    * Matches depend on the [[MediaType]] implementation. Instances of [[MediaType.Specific]], for example, will only
    * match parameters that have the same main- and sub-type, while instances of [[MediaType.Range]] will match any
    * type that shares a main-type.
    *
    * This method ignores media type parameters.
    */
  def unapply(mediaType: MediaType): Option[MediaType]

  /** Convenience method that behaves exactly as [[MediaType.unapply]] on instances of [[Response]].
    *
    * This is meant to make mapping on instances [[Request]] more convenient: {{{
    *  val req: Request[Response[ResponseEntity]] = ???
    *
    *  req.map {
    *    case MediaType.PlainText(res) => println("Plain text content: " + res.body.as[String])
    *    case MediaType.Text(res)      => println("Text content: " + res.body.as[String])
    *    case res                      => println(s"Unsupported media type: ${res.contentType}")
    *  }
    * }}}
    */
  def unapply[T](res: Response[T]): Option[Response[T]] = res.contentType.flatMap(unapply).map(_ => res)

  /** Removes the specified parameter. */
  def removeParam(name: String): MediaType =
    if(params.contains(name)) params(params.remove(name))
    else                      this

  /** Sets the specified parameter to the specified value. */
  def param[T: ValueWriter](name: String, value: T): MediaType = params(params.set(name, value))

  /** Returns the value of the requested parameter.
    *
    * This is strictly a convenience method and will simply call the underlying parameter's
    * [[MediaTypeParameters.get]] method.
    */
  def param[T: ValueReader](name: String): Option[T] = params.get[T](name)

  /** Sets the charset associated with this media type (`charset` parameter). */
  def charset: Option[Charset] = param[Charset]("charset")

  /** Returns the charset associated with this media type (`charset` parameter). */
  def charset(charset: Charset): MediaType = param("charset", charset)
}
