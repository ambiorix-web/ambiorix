#' Forward Method
#' 
#' Makes it such that the web server skips this method and uses the next one in line instead.
#' 
#' @return An object of class `forward`.
#' 
#' @export
forward <- function(){
  structure("next", class = "forward")
}

#' @export 
print.forward <- function(x, ...){
  cat("Using next method")
}