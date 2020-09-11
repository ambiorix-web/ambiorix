render_home <- function(req){
  response_render("home", list(title = "Hello from R"))
}

render_about <- function(req){
  response("About!")
}
