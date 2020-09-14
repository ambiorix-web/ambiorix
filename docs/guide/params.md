# Parameters & Query

Ambiorix allows using parameters in the URL and parses the query string for convenience.

## Parameters

Use `:<param>` to indicate a parameter which can then be accessed with `req$params$<name>`.

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/books/:category", function(req, res){
  res$send(htmltools::h3("Books of", req$params$category))
})

app$start()
```

```bash
http://localhost:3000/books/fiction
```

## Query

Parsed query string also can be accessed from the `req` object.

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/hello", function(req, res){
  res$send(htmltools::h3("Hi", req$query$firstname, req$query$lastname))
})

app$start()
```

```bash
http://localhost:3000/hello?firstname=John&lastname=Coene
```

