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

has_file <- function(x){
  fs::file_exists(x)
}

assertthat::on_failure(has_file) <- function(call, env){
  paste("Cannot find", deparse(call$x))
}

is_handler <- function(x){
  is_fun <- is.function(x)
  has_args <- length(formalArgs(x)) == 2

  all(is_fun, has_args)
}

assertthat::on_failure(is_handler) <- function(call, env){
  paste("`handler` must be a function that accepts: `req`, and `res`")
}