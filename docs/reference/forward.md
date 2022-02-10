# `forward`

Forward Method


## Description

Makes it such that the web server skips this method and uses the next one in line instead.


## Usage

```r
forward()
```


## Value

An object of class `forward` .


## Examples

```r
app <- Ambiorix$new()

app$get("/next", function(req, res){
forward()
})

app$get("/next", function(req, res){
res$send("Hello")
})

if(interactive())
app$start()
```


