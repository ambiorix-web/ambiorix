#' Forward Method
#' 
#' Makes it such that the web server skips this method and uses the next one in line instead.
#' 
#' @return An object of class `forward`.
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$get("/next", function(req, res){
#'  forward()
#' })
#' 
#' app$get("/next", function(req, res){
#'  res$send("Hello")
#' })
#' 
#' if(interactive())
#'  app$start()
#' 
#' @export
forward <- function(){
  structure("next", class = "forward")
}

#' @export 
print.forward <- function(x, ...){
  cat("Using next method")
}

#' @keywords internal
is_forward <- function(x) {
  inherits(x, "forward")
}