<div align="center">

# ambiorix

<!-- badges: start -->
<!-- badges: end -->

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express](https://github.com/expressjs/express).

</div>


## Hello World

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req){
  response("Hello!")
})

app$start()
```

## Usage

Use `:<param>` to indicate a parameter which can then be accessed with `req$params$<name>`.

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

app$get("/books$", function(req){
  response(htmltools::h2("List of Books! (coming soon)"))
})

app$get("/books/:category", function(req){
  response(htmltools::h3("Books of", req$params$category))
})

app$start()
```

Then try a few paths, e.g.:

```bash
http://127.0.0.1:3000/
http://127.0.0.1:3000/hello?firstname=John&lastname=Coene
http://127.0.0.1:3000/books/fiction
```

## Advanced Usage

The easiest way to get setup is by creating an ambiorix project with `create_ambiorix("path/to/project")`. This allows using templates and rendering them with `response_render`.

```r
ambiorix::create_ambiorix("myapp")
```
