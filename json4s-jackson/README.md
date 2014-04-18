# fetch: json4s-jackson connector

Adds support for [json4s](https://github.com/json4s/json4s) request and response entity bodies.

Serialization / de-serialization is achieved through [jackson](https://github.com/FasterXML/jackson).

Sample usage:
```scala
import com.nrinaudo.fetch.json4s._

// Http client, http request and JSON object.
val http: HttpClient = ???
val req:  Request    = ???
val json: JValue     = ???

// Brings an implicit Format instance in scope.
implicit val formats = org.json4s.DefaultFormats

// Automatically serializes json upon submission.
val response = http(request.body(json))

// Automatically de-serializes the response into a JSON object.
response.body.as[JValue]
```