# `responses`

Plain Responses


## Description

Plain HTTP Responses.


## Usage

```r
response(body, headers = list(`Content-Type` = "text/html"), status = 200L)
response_404(
  body = "404: Not found",
  headers = list(`Content-Type` = "text/html"),
  status = 404L
)
response_500(
  body = "500: Server Error",
  headers = list(`Content-Type` = "text/html"),
  status = 500L
)
```


## Arguments

Argument      |Description
------------- |----------------
`body`     |     Body of response.
`headers`     |     HTTP headers.
`status`     |     Response status


## Examples

```r
app <- Ambiorix$new()

# html
app$get("/", function(req, res){
res$send("hello!")
})

# text
app$get("/text", function(req, res){
res$text("hello!")
})

if(interactive())
app$start()
```


