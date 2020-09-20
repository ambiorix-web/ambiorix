# ambiorix

<img src="_assets/ambiorix.png" height = "350px"/>

<!-- badges: start -->
![](https://img.shields.io/badge/experimental--orange)
[![Travis build status](https://img.shields.io/travis/com/JohnCoene/ambiorix?style=flat-square)](https://travis-ci.com/JohnCoene/ambiorix)
<!-- badges: end -->

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express.js](https://github.com/expressjs/express).

<!-- panels:start -->
<!-- div:title-panel -->

## Features

<!-- div:left-panel -->

Ambiorix is unopinionated giving you flexibility. 

## Web Apps

Build multi-page or single-page web applications.

## APIs

Quickly build web RESTful APIs.

## Websocket

Support for bidirectional websocket communication.

<!-- div:right-panel -->

Basic example:

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Using {ambiorix}!")
})

app$get("/about", function(req, res){
  res$send("About page")
})

app$start()
```

<!-- panels:end -->

## Contributing

Please note that the ambiorix project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.