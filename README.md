<div align="center">

# ambiorix

<!-- badges: start -->
<!-- badges: end -->

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express](https://github.com/expressjs/express).

</div>

## Project

The easiest way to get setup is by creating an ambiorix project with `create_ambiorix("path/to/project")`

## Basic Usage

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req){
  response(htmltools::h1("Homepage!"))
})

app$get("/hello$", function(req){
  # ?firstname=John&lastname=Coene
  response(htmltools::h3("Hi", req$query$firstname, req$query$lastname))
})

app$get("/books/:category", function(req){
  response(htmltools::h3("Books of", req$params$category))
})

app$get("/books/:category/book/:id", function(req){
  response(htmltools::h3("Books of category", req$params$category, "has id", req$params$id))
})

app$get("/books$", function(req){
  response(htmltools::h2("List of Books!"))
})

app$start()
```


