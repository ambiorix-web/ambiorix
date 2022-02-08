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

app$get("/", \(req, res){
  forward()
})

app$get("/", \(req, res){
  res$send("NEXT!")
})

app$start()
```
