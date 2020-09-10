<!-- badges: start -->
<!-- badges: end -->

# ambiorix

## Project

The easiest way to get setup is by creating an ambiorix project with `create_ambiorix("path/to/project")`

## Basic Usage

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req){
  response(htmltools::h1("Homepage!"))
})

app$get("/hello", function(req){
  # ?name=John
  response(htmltools::h3("Your name is:", req$params$name))
})

app$start()
```
