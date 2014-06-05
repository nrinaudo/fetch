# fetch

[![Build Status](https://travis-ci.org/nrinaudo/fetch.svg?branch=master)](https://travis-ci.org/nrinaudo/fetch)

Fetch is a Scala wrapper for Java HTTP client implementations, and comes with a default `java.net.URLConnection`
connector.

It's meant to be simple, fluent and functional. Improvement suggestions and constructive criticisms are more than
welcome, however.

 

## Getting Fetch

The current version is 0.2.0, which can be added to your dependencies by adding the following line to your SBT build
file:

```scala
libraryDependencies += "com.nrinaudo" %% "fetch" % "0.2.0"
```


## Querying a remote server

The first step to performing a request is building the URL to connect to. This is typically done through `Url.parse`:

```scala
import com.nrinaudo._
Url.parse("http://graph.facebook.com").getOrElse {throw new IllegalArgumentException("Not a valid url")}
```

Once an instance of `Url` is available, it can be turned into a `Request`, provided an implicit engine is in scope:
```scala
val url = ???
implicit val engine = net.UrlEngine(5000, 5000)

val req = Request(url)
```

Common HTTP headers can be manipulated through helper methods in `Request`:
```scala
def createRequest = ???

val req = createRequest.acceptGzip               // Will receive GZIPed content is the remote server supports it
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
val req = createRequest.map { res =>
  if(res.status.isSuccess) res.body.as[String]
  else                     throw new Exception(status.code)
}
```

We were requesting `JSON` content, however, so an instance of `String` is not terribly useful. Fortunately, implicit
instances of `EntityParser` can be used to transform a `ResponseEntity` into a more immediately useful type.

Using the [json4s-jackson](json4s-jackson) Fetch module, for example, one would write:
 
```scala
import com.nrinaudo.fetch.json4s._

def createRequest = ???

// req is an instance of Request[JValue].
val req = createRequest.map { res =>
  if(res.status.isSuccess) res.body.as[JValue]
  else                     throw new Exception(status.code)
}
```


## Submitting content to a remote server

Content submission is done by calling a request's `apply(RequestEntity)` method and setting the appropriate HTTP method.

For example:
```scala
def createRequest = ???

req.PUT.apply(RequestEntity("Some text content"))

// There's an implicit conversion from string to request entity, so the previous line can also be written as:
req.PUT.apply("Some text content")

// Or, even if it's a bit misleading:
req.PUT("Some text content")
```

It's of course possible to write custom type converters. The following (fairly useless) code will allow you to submit
instances of `Int`:
```scala
implicit def intToEntity(value: Int) = RequestEntity.chars { _.write(value.toString) }
```

An example of such a mechanism is the [json4s-jackson](json4s-jackson) Fetch module, which declares
implicit conversion from a `JValue` to a `RequestEntity`.



