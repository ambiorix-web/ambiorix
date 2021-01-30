library(ambiorix)

app <- Ambiorix$new(log = TRUE)

app$get("/", function(req, res){
  res$send("Hello {ambiorix}!")
})

app$start()
