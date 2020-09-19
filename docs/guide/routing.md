# Routing

Routes that are specified are checked in order, meaning one can use `forward` to indicate that the next route should be checked.

In the application below, when a user points their browser to `/next` ambiorix checks the routes that match when found runs the handler function, when this handler returns `forward` ambiorix checks if another route matches this and so on.

```r
library(ambiorix)

app <- ambiorix$new()

app$get("/next", function(res, req){
  forward()
})

app$get("/next", functions(res, req){
  res$send("Hello")
})

app$start()
```

If not route are specified then ambiorix runs the `404`/`not_found` handler, see [not found](guide/not-found). Routing is crucial to ambiorix, it therefore also comes with a [router](guide/router) to better handle complex routing.
