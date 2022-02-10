# `Ambiorix`

Ambiorix


## Description

Web server.


## Value

An object of class `Ambiorix` from which one can
 add routes, routers, and run the application.


## Examples

```r
app <- Ambiorix$new()

app$get("/", function(req, res){
res$send("Using {ambiorix}!")
})

app$on_stop <- function(){
cat("Bye!\n")
}

if(interactive())
app$start()


## ------------------------------------------------
## Method `Ambiorix$listen`
## ------------------------------------------------

app <- Ambiorix$new()

app$listen(3000L)

app$get("/", function(req, res){
res$send("Using {ambiorix}!")
})

if(interactive())
app$start()

## ------------------------------------------------
## Method `Ambiorix$get`
## ------------------------------------------------

app <- Ambiorix$new()

app$get("/", function(req, res){
res$send("Using {ambiorix}!")
})

if(interactive())
app$start()

## ------------------------------------------------
## Method `Ambiorix$set_404`
## ------------------------------------------------

app <- Ambiorix$new()

app$set_404(function(req, res){
res$send("Nothing found here")
})

app$get("/", function(req, res){
res$send("Using {ambiorix}!")
})

if(interactive())
app$start()

## ------------------------------------------------
## Method `Ambiorix$start`
## ------------------------------------------------

app <- Ambiorix$new()

app$get("/", function(req, res){
res$send("Using {ambiorix}!")
})

if(interactive())
app$start()

## ------------------------------------------------
## Method `Ambiorix$receive`
## ------------------------------------------------

app <- Ambiorix$new()

app$get("/", function(req, res){
res$send("Using {ambiorix}!")
})

app$receive("hello", function(msg, ws){
print(msg) # print msg received

# send a message back
ws$send("hello", "Hello back! (sent from R)")
})

if(interactive())
app$start()

## ------------------------------------------------
## Method `Ambiorix$serialiser`
## ------------------------------------------------

app <- Ambiorix$new()

app$serialiser(function(data, ...){
jsonlite::toJSON(x, ..., pretty = TRUE)
})

app$get("/", function(req, res){
res$send("Using {ambiorix}!")
})

if(interactive())
app$start()
```


