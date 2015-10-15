# fetch

[![Build Status](https://travis-ci.org/nrinaudo/fetch.svg?branch=master)](https://travis-ci.org/nrinaudo/fetch) [![Coverage Status](https://coveralls.io/repos/nrinaudo/fetch/badge.svg)](https://coveralls.io/r/nrinaudo/fetch)

Fetch is a Scala wrapper for Java HTTP client implementations. It comes with a default `java.net.URLConnection`
connector, with more planned in the future.

It's meant to be simple, fluent and functional. Suggestions and constructive criticisms are more than welcome.



## Getting Fetch

The current version is 0.2.1, which can be added to your project with the following line in your SBT build file:

```scala
libraryDependencies += "com.nrinaudo" %% "fetch-core" % "0.2.1"
```


## Querying

The first step to performing a request is building the URL to connect to. This is typically done through the `Url`
companion object:

```scala
// String-based creation, returns None if the specified String is not a valid URL.
Url.parse("https://graph.facebook.com/me") // Option[Url]

// URI-based creation, can also fail (typically if the URI is relative).
Url.fromUri(new java.net.URI("https://graph.facebook.com/me")) // Option[Url]

// Step-by-step construction
(Protocol.Https :/ "graph.facebook.com") / "me"
```

Once an instance of `Url` is available, it can be turned into a `Request`, provided an implicit engine is in scope:
```scala
implicit val engine = net.UrlEngine(5000, 5000)

val req = Request((Protocol.Https :/ "graph.facebook.com") / "me")
```

Common HTTP headers can be manipulated through helper methods in `Request`:
```scala
def createRequest = ???

val req = createRequest.acceptGzip               // Will receive GZIPed content if the remote server supports it
                       .accept(MimeType.Json)    // Request JSON responses
                       .auth("user", "password") // "Basic-authentifies" as user
                       .userAgent("CustomAgent") // Sets the request's user agent.
                       .GET                      // Performs a GET request.
```

Applying a request will execute it and return an instance of `Response[ResponseEntity]`. Since `Request` is a functor,
however, it's usually more convenient to `map` to a better response type:

```scala
def createRequest = ???

// req is an instance of Request[String].
val req = createRequest.map {
  case res @ Status.Success(_) => res.body.as[String]
  case res @ Status(s)         =>
    res.empty()
    throw new Exception("Error " + s.code)
}
```

We were requesting `JSON` content, however, so an instance of `String` is not terribly useful. Fortunately, implicit
instances of `EntityReader` can be used to transform a `Response.Entity` into a more immediately useful type.

Using the [json4s-jackson](json4s-jackson) Fetch module, for example, one would write:

```scala
import fetch.json4s._

def createRequest = ???

// req is an instance of Request[JValue].
val req = createRequest.map {
  case Status.Success(res) => res.body.as[JValue]
  case res                 =>
      res.empty()
      throw new Exception("Error " + response.status.code)
}
```




## Submitting content

Content submission is done by calling a request's `apply(RequestEntity)` method and setting the appropriate HTTP method.

For example:
```scala
def createRequest = ???

createRequest.PUT.apply(RequestEntity("Some text content"))

// There's an implicit conversion from string to request entity, so the previous line can also be written as:
createRequest.PUT.apply("Some text content")

// Or, even if it's a bit misleading:
createRequest.PUT("Some text content")
```

It's of course possible to write custom type converters. The following (fairly useless) code will allow you to submit
instances of `Int`:
```scala
implicit def intToEntity(value: Int) = RequestEntity.chars { _.write(value.toString) }
```

An example of such a mechanism is the [json4s-jackson](json4s-jackson) Fetch module, which declares
implicit conversion from a `JValue` to a `RequestEntity`.
