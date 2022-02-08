# Asynchronous Programming

Ambiorix supports asynchronous programming so requests do not block R's single thread, allowing the server to serve other requests in the meantime. This is done by having the `handler` return a promise: _this promise should output a valid response._

## Example

The application below demonstrates asynchronous programming and its benefit. One can visit `/async` then `/sync` and get a response on the latter, despite the fact that `/async` is still processing (`Sys.sleep(10)` = 10 seconds). Were `/async` not returning a promise, `/sync` would have to wait until `/async` had stopped processing (10 seconds) before the server could return a response.

```r
library(future)
library(ambiorix)

plan(multisession)

app <- Ambiorix$new()

app$get("/async", \(req, res){
  future({
    Sys.sleep(10)
    res$send(Sys.time())
  })
})

app$get("/sync", \(req, res){
  res$send(Sys.time())
})

app$start()
```
