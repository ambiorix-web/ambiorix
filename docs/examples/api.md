# API

One is not limited to sending HTML responses and can thus build APIs with ambiorix.

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/cars", function(req, res){
  res$json("cars")
})

app$get("/dataset", function(req, res){
  dataset <- req$query[[1]]

  if(dataset == "iris")
    res$json(iris)
  else 
    res$json(mtcars)
  
})

app$run()
```

Note that you can change the serialiser with the `serialiser` method: pass it a function that accepts the data, it should return the JSON.

```r
app$serialiser(function(data){
  jsonify::to_json(data)
})
```
