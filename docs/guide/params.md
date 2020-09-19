# Parameters & Query

Ambiorix allows using parameters in the URL and also parses the query string for convenience. 

- Parameters are accessible at `req$params$param_name`
- Query are accessible at `req$query$query_name` or `req$query[[index]]` if unnamed.

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

Visiting `/books/fiction` produces:

![](../_assets/parameters.png)

## Query

The parsed query string can also be accessed from the `req` object.

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/hello", function(req, res){
  res$send(htmltools::h3("Hi", req$query$firstname, req$query$lastname))
})

app$start()
```

Visiting `/hello?firstname=John&lastname=Coene` produces:

![](../_assets/query.png)

