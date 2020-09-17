<div align="center">

# ambiorix

<img src="man/figures/ambiorix.png" height = "200px"/>

<!-- badges: start -->
![](https://img.shields.io/badge/experimental--orange)
[![Travis build status](https://img.shields.io/travis/com/JohnCoene/ambiorix?style=flat-square)](https://travis-ci.com/JohnCoene/ambiorix)
<!-- badges: end -->

[Website](https://ambiorix.john-coene.com) | [Quick start](http://ambiorix.john-coene.com/#/guide/hello-world)

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express.js](https://github.com/expressjs/express).

</div>


## Example

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello!")
})

app$start()
```

Then visit: `http://localhost:3000`, stop the server with `app$stop()`.

## Install

Ambiorix is an R package than can be installed from github.

```r
# install.packages("ambiorix")
remotes::install_github("JohnCoene/ambiorix")
```
