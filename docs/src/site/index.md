---
layout: default
---

[![Build Status](https://travis-ci.org/nrinaudo/fetch.svg)](https://travis-ci.org/nrinaudo/fetch)
[![codecov.io](http://codecov.io/github/nrinaudo/fetch/coverage.svg)](http://codecov.io/github/nrinaudo/fetch)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.nrinaudo/fetch_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.nrinaudo/fetch_2.11)
[![Join the chat at https://gitter.im/nrinaudo/fetch](https://img.shields.io/badge/gitter-join%20chat-52c435.svg)](https://gitter.im/nrinaudo/fetch)

Fetch is an HTTP library  written in the [Scala programming language](http://www.scala-lang.org).

The following tutorials are available:
{% for x in site.tut %}
{% if x.status != "wip" %}
* [{{ x.title }}]({{ site.baseurl }}{{ x.url }})
{% endif %}
{% endfor %}
