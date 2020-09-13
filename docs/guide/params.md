# Parameters & Query

Use `:<param>` to indicate a parameter which can then be accessed with `req$params$<name>`. Parsed query string can be accessed from the `req` object too.

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
