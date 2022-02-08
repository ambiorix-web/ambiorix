# Error Handling

This details how to handle errors in ambiorix. 

When an error occurs server-side it should send the client a response with a status starting in `5` to indicates that was the case.

## Default

By default ambiorix uses the following handler on error.

```r
\(req, res){
  response_500()
}
```

## Global

One can specify the handler to use when an error occurs anywhere in the application.

```r
app$error <- \(req, res){
  res$status(500L)
  res$send("There was a server error :(")
}
```

## Route Specific

Alternatively one can specify errors specific to certain routes, if these are not specified the global handler (above) is used.

```r
app$get("/error", \(req, res){
  print(eRrOr)
}, \(req, res){
  res$send("This is an error on /error", status = 500L)
})
```
