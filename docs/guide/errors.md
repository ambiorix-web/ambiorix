# Error Handling

This details how to handle errors in ambiorix.

## Default

By default ambiorix uses the follow handler on error.

```r
function(req, res){
  response_500()
}
```

## Global

One can specify the default response when an error occurs across the entire application.

```r
app$error <- function(req, res){
  res$status(500L)
  res$send("There was a server error :(")
}
```

## Route Specific

Alternatively one can specify errors specific to certain routes, if these are not specified the global handler (above) is used.

```r
app$get("error", function(req, res){
  print(eRrOr)
}, function(req, res){
  res$send("This is an error on /error", status = 500)
})
```
