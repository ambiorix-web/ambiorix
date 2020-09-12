<div align="center">

:construction:

# ambiorix

<!-- badges: start -->
<!-- badges: end -->

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express](https://github.com/expressjs/express).

</div>


## :wave: Hello World

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello!")
})

app$start()
```

Kill the server with `app$stop()`.

## :crystal_ball: Usage

Use `:<param>` to indicate a parameter which can then be accessed with `req$params$<name>`.

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send(htmltools::h1("Homepage!"))
})

app$get("/hello", function(req, res){
  # ?firstname=John&lastname=Coene
  res$send(htmltools::h3("Hi", req$query$firstname, req$query$lastname))
})

app$get("/books/:category", function(req, res){
  res$send(htmltools::h3("Books of", req$params$category))
})

app$start()
```

Then try a few paths, e.g.:

```bash
http://localhost:3000/
http://localhost:3000/hello?firstname=John&lastname=Coene
http://localhost:3000/books/fiction
```

## :microscope: Advanced

The easiest way to get setup is by creating an ambiorix project with `create_ambiorix("path/to/project")`. 

```r
ambiorix::create_ambiorix("myapp")
```

This allows using templates and rendering them with `res$render`. These templates can make use of `[% tags %]` which are replaced with item found in data.

The following template

```r
# templates/home.R
library(htmltools)

tags$html(
  lang = "en",
  tags$head(
    tags$meta(charset= "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "static/style.css"),
    tags$title("Ambiorix")
  ),
  tags$body(
    tags$h1("[% title %]") # tag
  )
)

```

The `[% title %]` can then be replaced with.

```r
res$render("home", data = list(title = "Hello from R"))
```

Lists and dataframes are `dput` in the template.

One can also use HTML templates (`.html` files) in which case the data is serialised to JSON.
