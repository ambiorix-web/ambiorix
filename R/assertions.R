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
  paste("Missing", deparse(call$x))
}

has_dir <- function(x){
  fs::dir_exists(x)
}

assertthat::on_failure(has_dir) <- function(call, env){
  paste("Cannot find", deparse(call$x))
}