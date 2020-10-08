# Middleware

You can also employ middleware with `use`: these are run first at every request. Note that unlike other methods (e.g.: `get`) those may return a response but do not have to.

Below we add a middleware that simply print the time at which the request is recevied.

```r
library(ambiorix)

app <- Ambiorix$new()

app$use(function(req, res){
  print(Sys.time())
})

app$get("/", function(req, res){
  res$send("Using {ambiorix}!")
})

app$get("/about", function(req, res){
  res$text("About")
})

app$start()
```

Multiple middleware can also be used. These can be used to modify add parameters to the request.

```r
library(ambiorix)

app <- Ambiorix$new()

app$use(function(req, res){
  req$set(x, 1) # set x to 1
})

app$get("/", function(req, res){
  print(req$get(x)) # retrieve x from the request
  res$send("Using {ambiorix}!")
})

app$get("/about", function(req, res){
  res$text("About")
})

app$use(function(req, res){
  req$get(x)
})

app$start()
```
