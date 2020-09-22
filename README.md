<div align="center">

# ambiorix

<img src="man/figures/ambiorix.png" height = "200px"/>

<!-- badges: start -->
![](https://img.shields.io/badge/experimental--orange)
[![Travis build status](https://img.shields.io/travis/com/JohnCoene/ambiorix?style=flat-square)](https://travis-ci.com/JohnCoene/ambiorix)
<!-- badges: end -->

[Website](https://ambiorix.john-coene.com) | [CLI](https://github.com/JohnCoene/ambiorix-cli) | [Docker](https://hub.docker.com/r/jcoenep/ambiorix)

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express.js](https://github.com/expressjs/express).

</div>


## Example

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello!")
})

app$get("/about", function(req, res){
  res$send("About page")
})

app$start()
```

## Install

Ambiorix is an R package than can be installed from github.

```r
# install.packages("ambiorix")
remotes::install_github("JohnCoene/ambiorix")
```

## Contributing

Please note that the ambiorix project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
