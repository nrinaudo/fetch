# fetch: json4s-native connector

Adds support for [json4s](https://github.com/json4s/json4s) request and response entity bodies.

Serialization / de-serialization is achieved through the native json4s library.


## Getting the module
The current version is 0.2.0, which can be added to your project with the following line in your SBT build file:

```scala
libraryDependencies += "com.nrinaudo" %% "fetch-json4s-native" % "0.2.0"
```


## Sample usage
```scala
import com.nrinaudo.fetch.json4s._

// Request and JSON object.
val req:  Request    = ???
val json: JValue     = ???

// Brings an implicit Format instance in scope.
implicit val formats = org.json4s.DefaultFormats

// Automatically serializes JSON upon submission.
val response = request(json)

// Automatically de-serializes the response into a JSON object.
response.body.as[JValue]
```