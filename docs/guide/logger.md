# Logger

Ambiorix' logger is built with [log](https://log.opifex.org).
The original `Logger` class found in early versions of ambiorix 
has been deprecated in favour of the `Logger` class from 
the [log](https://log.opifex.org) package.

<!-- panels:start -->
<!-- div:title-panel -->
## Auto
<!-- div:left-panel -->

You can switch on the logging either with the `ambiorix.logger` option or when instantiating the application.

```r
library(ambiorix)

app <- Ambiorix$new(log = TRUE)

app$get("/", \(req, res){
  res$send("hello!")
})

app$get("/about", \(req, res){
  res$send("Me me me")
})

app$start()
```

<!-- div:right-panel -->

Visiting both routes gives the following log.

```
> 2020-09-20 13:47:40 - Listening on http://localhost:17094
> 2020-09-20 13:47:41 - GET on / by 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36'
> 2020-09-20 13:47:42 - GET on /favicon.ico - Not found
> 2020-09-20 13:47:46 - GET on /about by 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.102 Safari/537.36'
> 2020-09-20 13:47:48 - Server stopped
```

<!-- panels:end -->


<!-- panels:start -->
<!-- div:title-panel -->
## Manual
<!-- div:left-panel -->
The logger used internally (above) is exported and can be used by developers: this will work regardless of whether the internal logger is on or off. Note that it will automatically prepend every event logged with the time at which it happened.

```r
library(ambiorix)

app <- Ambiorix$new()

# create logger
logger <- new_log()

app$get("/", \(req, res){
  logger$log("Home", "was visited")
  res$send("hello!")
})

app$get("/about", \(req, res){
  logger$log("About page", "was just viewed")
  res$send("Me me me")
})

app$start()
```
<!-- div:right-panel -->
Visiting both routes gives the following log.

```
> 2020-09-20 13:48:34 - Home was visited
> 2020-09-20 13:48:41 - About page was just viewed
```
<!-- panels:end -->

<!-- panels:start -->
<!-- div:title-panel -->
## Log package
<!-- div:left-panel -->
The logger used internally (above) is exported and can be used by developers: this will work regardless of whether the internal logger is on or off. Note that it will automatically prepend every event logged with the time at which it happened.

```r
library(log)
library(ambiorix)

app <- Ambiorix$new()

# create logger with the log package
logger <- Logger$new()

app$get("/", \(req, res){
  log$log("Home", "was visited")
  res$send("hello!")
})

app$get("/about", \(req, res){
  log$log("About page", "was just viewed")
  res$send("Me me me")
})

app$start()
```
<!-- div:right-panel -->
Visiting both routes gives the following log.

```
> Home was visited
> About page was just viewed
```
<!-- panels:end -->

