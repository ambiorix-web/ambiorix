library(ambiorix)

app <- Ambiorix$new()

app$serve_static("assets", "static")

app$get("/", function(req){
  response_render("home", list(title = "Hello from R"))
})

app$get("/about", function(req){
  response("About!")
})

app$start()
