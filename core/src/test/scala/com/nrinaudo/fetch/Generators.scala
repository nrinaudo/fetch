package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.util.{Locale, Date}

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import scala.collection.JavaConverters._

/** Defines all the common generators used by our property tests. */
object Generators {
  // - Parameter generators --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Param(name: String, value: String)

  implicit val arbParam: Arbitrary[Param] = Arbitrary {
    for {
      name <-  identifier
      value <- nonEmptyListOf(oneOf(32.toChar to 126.toChar)).map(_.mkString)
    } yield Param(name, value)
  }

  // Note: we don't generate more than 5 parameters because this causes unrelated issues when testing - some HTTP
  // servers seem to have a limit to the size of a header value.
  implicit val arbParams: Arbitrary[Parameters] = Arbitrary {
    for {
      size <- choose(0, 5)
      ps   <- mapOfN(size, arbitrary[Param].map(p => p.name -> p.value))
    } yield Parameters(ps)
  }

  def illegalParams: Gen[String] = Arbitrary.arbitrary[String].suchThat(str => !str.contains('=') && !str.isEmpty)



  // - MediaType generators --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def mainType: Gen[String] = Gen.oneOf("text", "application", "video", "audio", "image", "message", "multipart")
  def subType: Gen[String] = Gen.oneOf("plain", "png", "jpg", "rdf", "html", "rdf+xml", "json", "x-fixed-field")

  implicit val arbMediaAll: Arbitrary[MediaType.All] = Arbitrary(Gen.const(MediaType.Everything))
  implicit val arbMediaRange: Arbitrary[MediaType.Range] = Arbitrary(for(main <- mainType) yield MediaType.Range(main))
  implicit val arbMediaSpecific: Arbitrary[MediaType.Specific] = Arbitrary {
    for {
      main   <- mainType
      sub    <- subType
    } yield MediaType.Specific(main, sub)
  }

  implicit val arbMediaType: Arbitrary[MediaType] = Arbitrary {
    for {
      mediaType <- Gen.oneOf(arbitrary[MediaType.All], arbitrary[MediaType.Range], arbitrary[MediaType.Specific])
      // Note: q is not considered a valid media type parameter, as it would conflict with the Accept header's q parameter.
      params    <- arbitrary[Parameters].map(p => p.remove("q"))
    } yield mediaType.params(params)
  }

  def illegalMediaType: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.indexOf('/') == -1)


  // - Protocol generators ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Generates a random supported protocol (http or https). */
  implicit val arbProtocol: Arbitrary[Protocol] = Arbitrary(Gen.oneOf(Protocol.Http, Protocol.Https))

  /** Generates invalid protocol names. */
  def invalidProtocol: Gen[String] = Arbitrary.arbitrary[String].suchThat(s => s != Protocol.Http.name && s != Protocol.Https.name)


  // - QueryString generators ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Returns a single query parameter. */
  def queryParam: Gen[(String, List[String])] = for {
    name   <- arbitrary[String].suchThat(!_.isEmpty)
    count  <- choose(1, 5)
    values <- listOfN(count, arbitrary[String].suchThat(!_.isEmpty))
  } yield (name, values)

  def queryParams: Gen[Map[String, List[String]]] = for {
    count  <- choose(1, 10)
    params <- listOfN(count, queryParam)
  } yield params.foldLeft(Map(): Map[String, List[String]]) { (map, param) => map + param}

  /** Returns a query string. */
  implicit val arbQueryString: Arbitrary[QueryString] = Arbitrary {
    for {
      query <- queryParams
    } yield QueryString(query)
  }


  // - URL generators --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def domainSeg: Gen[String] = for {
    first   <- alphaChar
    length  <- choose(1, 10)
    content <- listOfN(length, alphaLowerChar)
  } yield (first :: content).mkString

  /** Generates a valid host. */
  def host: Gen[String] = for {
    name <- domainSeg
    ext  <- oneOf("com", "fr", "es", "it", "co.uk", "co.jp", "io")
  } yield name + "." + ext

  /** Generates a valid port. */
  def port: Gen[Int] = choose(1, 65535)

  def segment: Gen[String] = arbitrary[String].suchThat(!_.isEmpty)

  /** Generates a valid path. */
  def path: Gen[List[String]] = for {
    count <- choose(0, 5)
    path <- listOfN(count, segment)
  } yield path

  def fragment: Gen[Option[String]] = oneOf(true, false) flatMap {b =>
    if(b) None
    else  arbitrary[String].map(Some(_))
  }

  implicit val arbUrl: Arbitrary[Url] = Arbitrary {
    for {
      pr <- arbitrary[Protocol]
      h  <- host
      p  <- port
      s  <- path
      q  <- arbitrary[QueryString]
      r  <- fragment
    } yield Url(pr, h, p, s, q, r)
  }


  // - ETag generators -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def weakTag: Gen[ETag] = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield ETag.Weak(tag)
  def strongTag: Gen[ETag] = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield ETag.Strong(tag)

  implicit val arbEtag: Arbitrary[ETag] = Arbitrary(Gen.oneOf(weakTag, strongTag))

  implicit val arbEtags: Arbitrary[List[ETag]] = Arbitrary(HeadersSpec.headers(arbitrary[ETag]))

  def invalidEtag: Gen[String] = Arbitrary.arbitrary[String].suchThat { str =>
    str.length == 0 || str.charAt(0) != 'W' || str.charAt(0) != '\"' || str.charAt(str.length - 1) != '\"'
  }


  // - Charset generators ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private lazy val charsets: List[Charset] = Charset.availableCharsets().values().asScala.toList

  implicit val arbCharset: Arbitrary[Charset] = Arbitrary(Gen.oneOf(charsets))

  def illegalCharset: Gen[String] = Arbitrary.arbitrary[String].suchThat(!Charset.availableCharsets().containsKey(_))


  // - Encoding generators ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val arbEncoding: Arbitrary[Encoding] = Arbitrary(Gen.oneOf(Encoding.Gzip, Encoding.Deflate, Encoding.Identity))
  def illegalEncoding: Gen[String] = arbitrary[String].suchThat(e => !Encoding.DefaultEncodings.contains(e))


  // - Date generators ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val arbDate: Arbitrary[Date] = Arbitrary(for(time <- choose(0, 253402300799000l)) yield new Date((time / 1000l) * 1000))

  def illegalDate: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.matches(".*[^0-9a-zA-Z,: ].*"))



  // - Language generators ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val locale: Arbitrary[Locale] = Arbitrary(Gen.oneOf(Locale.ENGLISH, Locale.FRENCH, Locale.GERMAN, Locale.ITALIAN, Locale.JAPANESE,
    Locale.KOREAN, Locale.CHINESE, Locale.SIMPLIFIED_CHINESE, Locale.TRADITIONAL_CHINESE, Locale.FRANCE, Locale.GERMANY,
    Locale.ITALY, Locale.JAPAN, Locale.KOREA, Locale.CHINA, Locale.PRC, Locale.TAIWAN, Locale.UK, Locale.US,
    Locale.CANADA, Locale.CANADA_FRENCH))

  implicit val arbLanguage: Arbitrary[Language] = Arbitrary(arbitrary[Locale].map(l => Language(l.getLanguage, List(l.getCountry).filter(_.nonEmpty))))

  def illegalLanguage: Gen[String] = Arbitrary.arbitrary[String].suchThat {_.matches(".*[^a-zA-Z_-].*")}



  // - Conneg generators -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit def conneg[A: Arbitrary]: Arbitrary[Conneg[A]] = Arbitrary {
    for {
      value <- arbitrary[A]
      q     <- Gen.choose(0, 1000)
    } yield Conneg(value, q / 1000f)
  }

  implicit def connegs[A: Arbitrary]: Arbitrary[List[Conneg[A]]] = Arbitrary(HeadersSpec.headers(arbitrary[Conneg[A]]))



  // - Method generators -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Generates random, legal HTTP methods. */
  implicit val arbMethod: Arbitrary[Method] = Arbitrary {
    Gen.oneOf(Method.GET, Method.POST, Method.PUT, Method.DELETE, Method.OPTIONS, Method.TRACE,
      Method.PATCH, Method.LINK, Method.UNLINK)
  }

  def illegalMethod: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.exists(!_.isLetter))

  implicit val arbMethods: Arbitrary[List[Method]] = Arbitrary {
    for {
      count <- Gen.choose(1, 5)
      set   <- Gen.containerOfN[Set, Method](count, arbitrary[Method])
    } yield set.toList
  }


  // - Status generators -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val success: Gen[Status]     = for(status <- Gen.choose(200, 299)) yield Status(status)
  val redirection: Gen[Status] = for(status <- Gen.choose(300, 399)) yield Status(status)
  val clientError: Gen[Status] = for(status <- Gen.choose(400, 499)) yield Status(status)
  val serverError: Gen[Status] = for(status <- Gen.choose(500, 599)) yield Status(status)

  implicit val arbStatus: Arbitrary[Status] = Arbitrary(Gen.oneOf(success, redirection, clientError, serverError))

  def invalidStatus: Gen[Int] = Arbitrary.arbitrary[Int].suchThat(i => i < 0 || i > 600)



  // - Credentials generators ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Note that this is not entirely correct: according to the RFC, password are allowed to contain a ':'. This is not
  // properly handled in version 0.7.1 of unfiltered, however (the issue is fixed in github, but not yet released).
  def authCredentials: Gen[(String, String)] = for {
    user <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
    pwd  <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
  } yield (user, pwd)


  // - ByteRange generators --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def illegalRange: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.matches(".*[^0-9-].*"))

  def illegalRanges: Gen[String] = Arbitrary.arbitrary[String].suchThat(!_.startsWith("bytes="))

  /** Generates valid byte range boundaries. */
  def rangeBoundary: Gen[Int] = Gen.choose(0, 1000)

  /** Generates invalid byte range boundaries. */
  def negRangeBoundary: Gen[Int] = Gen.choose(-1000, -1)

  /** Generate valid byte range boundaries. */
  def rangeBoundaries: Gen[(Int, Int)] = for {
    from <- Gen.choose(0, 1000)
    to   <- Gen.choose(from, from + 1000)
  } yield (from, to)


  implicit val arbPrefixRange: Arbitrary[PrefixRange] = Arbitrary(rangeBoundary.map(PrefixRange.apply))
  implicit val arbSuffixRange: Arbitrary[SuffixRange] = Arbitrary(rangeBoundary.map(SuffixRange.apply))
  implicit val arbFullRange: Arbitrary[FullRange] = Arbitrary {
    for {
      from <- rangeBoundary
      to   <- rangeBoundary
    } yield FullRange(math.min(from, to), math.max(from, to))
  }
  implicit val arbByteRange: Arbitrary[ByteRange] =
    Arbitrary(Gen.oneOf(arbitrary[PrefixRange], arbitrary[SuffixRange], arbitrary[FullRange]))

  implicit val arbByteRanges: Arbitrary[List[ByteRange]] = Arbitrary(HeadersSpec.headers(arbitrary[ByteRange]))
}
