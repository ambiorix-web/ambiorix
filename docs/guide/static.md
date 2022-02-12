# Static Files

This details how to server static files such as images.

The underlying logic is inherited from httpuv which is also
used by Shiny, therefore this is very similar to Shiny's
`addResourcePath`.

Use the method `static`, it takes, as first argument, the path
to the _directory_ containing the static files, and as
second argument the path where those files will be available,
e.g.: `static("path", "www")` will make the files in `path`
accessible at the `www`: `www/myImage.png`.

See the example below.

```r
library(ambiorix)

app <- Ambiorix$new()

app$static("path/to/static/assets", "assets")

app$get("/", \(req, res){
  res$send(
    "<h1>Hello everyone!</h1>
    <img src='assets/image.png' />"
  )
})

app$start()
```