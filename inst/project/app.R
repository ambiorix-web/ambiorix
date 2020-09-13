library(ambiorix)
import("views")

app <- Ambiorix$new()

# 404 page
app$not_found <- render_404

# serve static files
app$serve_static("assets", "static")

# homepage
app$get("/", render_home)

# about
app$get("/about", render_about)

# websocket 
app$receive("hello", function(msg, ws){
  print(msg)
  ws$send("hello", "Hello back! (sent from R)")
})

app$start()
