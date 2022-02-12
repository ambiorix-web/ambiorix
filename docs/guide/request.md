# Request

This details the request object, generally the first argument
of the functions passed to paths (`app$get("/", \(req, res){})`).

## Object

Easiest to see what is available is to `print` the object.

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", \(req, res){
  print(req)
  res$send("Using {ambiorix}!")
})

app$start()
```

```
✔ GET "/"
• HEADERS: "image/avif,image/webp,*/*", "gzip, deflate", "en-US,en;q=0.5", "keep-alive",
"localhost:9345", "http://localhost:9345/", "image", "no-cors", "same-origin", and "Mozilla/5.0 (X11;
Linux x86_64; rv:97.0) Gecko/20100101 Firefox/97.0"
• HTTP_ACCEPT: "image/avif,image/webp,*/*"
• HTTP_ACCEPT_ENCODING: "gzip, deflate"
• HTTP_ACCEPT_LANGUAGE: "en-US,en;q=0.5"
• HTTP_CACHE_CONTROL:
• HTTP_CONNECTION: "keep-alive"
• HTTP_COOKIE:
• HTTP_DNT:
• HTTP_HOST: "localhost:9345"
• HTTP_SEC_FETCH_DEST: "image"
• HTTP_SEC_FETCH_MODE: "no-cors"
• HTTP_SEC_FETCH_SITE: "same-origin"
• HTTP_SEC_FETCH_USER:
• HTTP_UPGRADE_INSECURE_REQUESTS:
• HTTP_USER_AGENT: "Mozilla/5.0 (X11; Linux x86_64; rv:97.0) Gecko/20100101 Firefox/97.0"
• httpuv.version 1.6.5
• PATH_INFO: "/favicon.ico"
• QUERY_STRING: ""
• REMOTE_ADDR: "127.0.0.1"
• REMOTE_PORT: "59462"
• REQUEST_METHOD: "GET"
• SCRIPT_NAME: ""
• SERVER_NAME: "127.0.0.1"
• SERVER_PORT: "127.0.0.1"
• CONTENT_LENGTH:
• CONTENT_TYPE:
• HTTP_REFERER: "http://localhost:9345/"
• rook.version: "1.1-0"
• rook.url_scheme: "http"
```

To access the `HEADERS` for instance, simple do `req$HEADERS`.

## Set & Get

This is mainly useful with middlewares, you can `set` and `get` values
on requests.

For instance below we use the middleware to set a variable `x` on the
request object, and we retrieve it when someone accesses the homepage.

```r
app <- Ambiorix$new()

app$use(\(req, res){
  # set
  req$set(x, "John")
})

app$get("/", \(req, res){
  # get
  print(req$get(x))
  res$sen("Hello {ambiorix}")
})

app$start()
```

__Lock__

You can also lock some of these values (internally these are stored in an
environment), this ensures the value cannot be modified later on.
Therefore the example below fails, `x` cannot be retrived.

```r
app <- Ambiorix$new()

app$use(\(req, res){
  # set
  req$set(x, "John", lock = TRUE)
})

app$get("/", \(req, res){
  # will fail
  req$set(x, "Bob")

  # get
  print(req$get(x))
  res$sen("Hello {ambiorix}")
})

app$start()
```

__Counter__

This is an example of creating a counter; every refresh bumps the counter.

```r
app <- Ambiorix$new()

val <- 0L

app$use(\(req, res){
  val <<- val + 1L
  req$set(x, val)
})

app$get("/", \(req, res){
  res$send_sprintf(
    "Count %s",
    req$get(x)
  )
})

app$start()
```
