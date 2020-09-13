# render homepage
render_home <- function(req, res){
  res$render("home", list(title = "Hello from R"))
}

# render about
render_about <- function(req, res){
  res$render("about", list(title = I("About"), name = req$query$name))
}

# 404: not found
render_404 <- function(req, res){
  res$render("404", status = 404L)
}