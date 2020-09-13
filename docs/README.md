<div align="center">

# ambiorix

<img src="man/figures/ambiorix.png" height = "200px"/>

<!-- badges: start -->
<!-- badges: end -->

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express](https://github.com/expressjs/express).

</div>


## :inbox_tray: Install

Ambiorix is an R package than can be installed from github.

```r
# install.packages("ambiorix")
remotes::install_github("JohnCoene/ambiorix")
```

## :bookmark_tabs: Parameters & Query

Use `:<param>` to indicate a parameter which can then be accessed with `req$params$<name>`. Parsed query string can be accessed from the `req` object too.

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send(htmltools::h1("Homepage!"))
})

app$get("/hello", function(req, res){
  # ?firstname=John&lastname=Coene
  res$send(htmltools::h3("Hi", req$query$firstname, req$query$lastname))
})

app$get("/books/:category", function(req, res){
  res$send(htmltools::h3("Books of", req$params$category))
})

app$start()
```

Then try a few paths, e.g.:

```bash
http://localhost:3000/
http://localhost:3000/hello?firstname=John&lastname=Coene
http://localhost:3000/books/fiction
```

## :hammer_and_wrench: Project & Templates

The easiest way to get setup is by creating an ambiorix project with `create_ambiorix`. 

```r
ambiorix::create_ambiorix("myapp")
```

This allows using templates and rendering them with `res$render`. These templates can make use of `[% tags %]` which are replaced with item found in data.

The following template

```r
# templates/home.R
library(htmltools)

tags$html(
  lang = "en",
  tags$head(
    tags$meta(charset= "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "static/style.css"),
    tags$title("Ambiorix")
  ),
  tags$body(
    tags$h1("[% title %]") # tag
  )
)

```

The `[% title %]` can then be replaced with.

```r
res$render("home", data = list(title = "Hello from R"))
```

Lists and dataframes are `dput` in the template.

One can also use HTML templates (`.html` files) in which case the data is serialised to JSON.

## :electric_plug: Websocket

You can listen to incoming messages with the `receive` method which takes 1) the name of the message to handle and 2) a callback function to run when message with `name` is recevied. The callback function must accept the message as first argument and optionally the socket as second argument.

Below a handler listening to the message `hello`, prints the message and uses the websocket to send a response.

```r
# websocket 
app$receive("hello", function(msg, ws){
  print(msg)
  ws$send("hello", "Hello back! (sent from R)")
})
```

These can be handled server side with the JavaScript websocket library or using the `Ambiorix` class. It provides a static method to send messages through the websocket, like other method in R it accepts 1) the `name` of the message and 2) the `message` itself: `Ambiorix.send('hello', 'Hello from the client')`.

One can also instantiate the class to add handlers with `receive` method then run `start` to listen to the incoming messages.

```js
var wss = new Ambiorix();
wss.receive("hello", function(msg){
  alert(msg);
});
wss.start();
```

## :mag_right: 404

You can set the 404 page in two ways.

```r
app$not_found <- function(req, res){
  res$send(htmltools::h2("404"), status = 404L)
}

app$set_404(function(req, res){
  res$send(htmltools::h2("Not found"), status = 404L)
})
```

## :heavy_multiplication_x: Stop

You can stop one or all servers as well as check whether it is running.

```r
# is server running
app$is_running

# stop server
app$stop()

# stop all servers
stop_all()
```
