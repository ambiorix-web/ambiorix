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

app$get("/", function(req, res){
  res$send("Hello!")
})

app$start()
```

## Usage

Use `:<param>` to indicate a parameter which can then be accessed with `req$params$<name>`.

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send(htmltools::h1("Homepage!"))
})

app$get("/hello$", function(req, res){
  # ?firstname=John&lastname=Coene
  res$send(htmltools::h3("Hi", req$query$firstname, req$query$lastname))
})

app$get("/books$", function(req, res){
  res$send(htmltools::h2("List of Books! (coming soon)"))
})

app$get("/books/:category", function(req, res){
  res$send(htmltools::h3("Books of", req$params$category))
})

app$start()
```

Then try a few paths, e.g.:

```bash
http://127.0.0.1:3000/
http://127.0.0.1:3000/hello?firstname=John&lastname=Coene
http://127.0.0.1:3000/books/fiction
```

## Advanced

The easiest way to get setup is by creating an ambiorix project with `create_ambiorix("path/to/project")`. 

```r
ambiorix::create_ambiorix("myapp")
```

This allows using templates and rendering them with `res$render`. These templates can make use of `[% tags %]` which are replaced with item found in data.

The following template

```html
<!-- templates/home.html -->
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="static/style.css">
  <title>Ambiorix</title>
</head>
<body>
  <h1>[% title %]</h1>
</body>
</html>
```

The `[% title %]` can then be replaced with.

```r
res$render("home", data = list(title = "Hello from R"))
```
