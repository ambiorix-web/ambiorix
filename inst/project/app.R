library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req){
  response_render("home", list(title = "Hello from R"))
})

app$get("/about", function(req){
  response("About!")
})

app$start()
