library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello {ambiorix}!")
})

app$start()
