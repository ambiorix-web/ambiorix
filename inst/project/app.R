library(ambiorix)
import("views")

app <- Ambiorix$new()

app$serve_static("assets", "static")

app$get("/", render_home)

app$get("/about", render_about)

app$start()
