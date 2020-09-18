# Routing

In order to better structure the app ambiorix comes with a router. These allow having a base path prepended to every route added to it.

```r
library(ambiorix)

# route
route <- Router$new("/hello")

route$get("/", function(req, res){
  res$send("Hello!")
})

# core app
app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Home!")
})

# mount the router
app$use(route)

app$start()
```

Navigating to `/hello` will show the "Hello!" response.
