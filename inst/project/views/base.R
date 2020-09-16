# render homepage
render_home <- function(req, res){
  res$render("home", list(title = "Hello from R", subtitle = "This is rendered with {glue}"))
}

# render about
render_about <- function(req, res){
  res$render("about", list(title = "About", name = robj(req$query$name)))
}

# 404: not found
render_404 <- function(req, res){
  res$render("404", status = 404L)
}