# Hello World

By default ambiorix will serve the application on port `3000`, this can be changed, along with other things, when instantiating the class.

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello!")
})

app$start()
```

Kill the server with `app$stop()`.
