---
layout: default
title:  "Downloading remote data"
section: tutorial
---

```tut:silent
import com.nrinaudo.fetch._
import com.nrinaudo.fetch.net._

implicit val engine = net.UrlEngine(5000, 5000)

val root = Protocol.Http.host("httpbin.org").toRequest 
```

```tut
(root / "post").acceptGzip.POST("this is a test").body.as[String]
```
