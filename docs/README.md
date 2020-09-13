<img src="_assets/ambiorix.png" height = "350px" align="right"/>

# ambiorix

<!-- badges: start -->
<!-- badges: end -->

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express.js](https://github.com/expressjs/express).

## Install

Ambiorix is an R package than can be installed from github.

```r
# install.packages("ambiorix")
remotes::install_github("JohnCoene/ambiorix")
```

## Example

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello!")
})

app$start()
```
