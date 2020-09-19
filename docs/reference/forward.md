# Forward

## Description

Skips forward to the next matching method and path.

## Usage

```r
forward()
```

## Example

```r
app <- Ambiorix$new()

app$get("/", function(req, res){
  forward()
})

app$get("/", function(req, res){
  res$send("NEXT!")
})

app$start()
```
