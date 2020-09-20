# Routing

When a client (web browser) points to a path (e.g.: `/about`) a request is made to the server (`GET` in this case), ambiorix then looks through the handlers __in the order they are were__ and when it finds one that matches the requested path runs the handler function (`function(req, res)`). This function should return a response (using the `res` object) or a future (see [asynchronous programming](/guide/async)).

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$text("Home!")
})

app$get("/about", function(req, res){
  res$send("About me!")
})

app$start()
```

## Handler

The handler function used for every route __must take 2 arguments_: the request, and the response. The first holds data on the request that is made to the server, which contains many things but importantly includes `parameters` and the parsed `query` string. You can learn more about these the [parameters and query ](/guide/params) section.

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/?name", function(req, res){
  msg <- htmltools::h1("Hello", req$query$name)
  res$send(msg)
})

app$get("/users/:id", function(req, res){
  msg <- sprintf("This is user id: #%s", req$params$id)
  res$text(msg)
})

app$start()
```

## Forward

Since routes are checked in a certain order one can use `forward` to indicate that the next route should be checked instead.

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/next", function(req, res){
  forward()
})

app$get("/next", function(req, res){
  res$send("Hello")
})

app$start()
```

If no route are specified then ambiorix runs the `404`/`not_found` handler, see [not found](guide/not-found). 

Routing is crucial to ambiorix, it therefore also comes with a [router](guide/router) to better structure complex routing for large applications.
