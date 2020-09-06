valid_path <- function(x) {
  if(missing(x)) return(FALSE)
  if(!inherits(x, "character")) return(FALSE)
  return(TRUE)
}

assertthat::on_failure(valid_path) <- function(call, env) {
  paste0(deparse(call$x), " is not valid")
}

not_missing <- function(x){
  !missing(x)
}

assertthat::on_failure(not_missing) <- function(call, env){
  paste0("Missing", deparse(call$x))
}