valid_path <- function(x) {
  if (missing(x)) {
    return(FALSE)
  }

  if (!inherits(x, "character")) {
    return(FALSE)
  }

  return(TRUE)
}

assertthat::on_failure(valid_path) <- function(call, env) {
  paste0(deparse(call$x), " is not valid")
}

not_missing <- function(x) {
  !missing(x)
}

assertthat::on_failure(not_missing) <- function(call, env) {
  paste("Missing", deparse(call$x))
}

has_file <- function(x) {
  file.exists(x)
}

assertthat::on_failure(has_file) <- function(call, env) {
  paste("Cannot find", deparse(call$x))
}

is_handler <- function(x) {
  is_fun <- is.function(x)
  has_args <- length(formalArgs(x)) == 2

  all(is_fun, has_args)
}

assertthat::on_failure(is_handler) <- function(call, env) {
  paste("`handler` must be a function that accepts: `req`, and `res`")
}

is_param_handler <- function(x) {
  is_fun <- is.function(x)
  has_args <- length(formalArgs(x)) == 4
  all(is_fun, has_args)
}

assertthat::on_failure(is_param_handler) <- function(call, env) {
  paste(
    "`handler` must be a function that accepts: `req`, `res`, `value` and `name`"
  )
}

is_error_handler <- function(x) {
  is_fun <- is.function(x)
  has_args <- length(formalArgs(x)) == 3

  all(is_fun, has_args)
}

assertthat::on_failure(is_error_handler) <- function(call, env) {
  "`handler` must be a function that accepts: `req`, `res` and `error`"
}

is_logger <- function(x) {
  inherits(x, "Logger")
}

assertthat::on_failure(is_logger) <- function(call, env) {
  sprintf(
    "%s must be an object of class `Logger` from the `log` package",
    deparse(call$x)
  )
}

is_function <- function(x) {
  is.function(x)
}

assertthat::on_failure(is_function) <- function(call, env) {
  sprintf(
    "`%s` is not a function",
    deparse(call$x)
  )
}

is_renderer <- function(x) {
  length(formalArgs) != 2L
}

assertthat::on_failure(is_renderer) <- function(call, env) {
  sprintf(
    "`%s` must accept two arguments: `file` and `data`",
    deparse(call$x)
  )
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L
}

assertthat::on_failure(is_string) <- function(call, env) {
  sprintf("`%s` must be a string (character of length 1)", deparse(call$x))
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

assertthat::on_failure(is_flag) <- function(call, env) {
  sprintf("`%s` must be `TRUE` or `FALSE`", deparse(call$x))
}

is_openapi_status <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }

  if (is.numeric(x)) {
    return(x >= 100 && x <= 599 && x == as.integer(x))
  }

  if (is.character(x)) {
    return(x == "default" || grepl("^[1-5](XX|[0-9]{2})$", x))
  }

  FALSE
}

assertthat::on_failure(is_openapi_status) <- function(call, env) {
  sprintf(
    paste0(
      "`%s` must be an HTTP status code (100-599), ",
      "a status range such as \"2XX\", or \"default\""
    ),
    deparse(call$x)
  )
}

has_names <- function(x) {
  nms <- names(x)
  !is.null(nms) && all(nzchar(nms))
}

assertthat::on_failure(has_names) <- function(call, env) {
  sprintf("all elements of `%s` must be named", deparse(call$x))
}

is_openapi_schema <- function(x) {
  inherits(x, "ambiorix_openapi_schema")
}

assertthat::on_failure(is_openapi_schema) <- function(call, env) {
  sprintf(
    "`%s` must be an OpenAPI schema, see `?openapi-schemas`",
    deparse(call$x)
  )
}

is_openapi_parameter <- function(x) {
  inherits(x, "ambiorix_openapi_parameter")
}

assertthat::on_failure(is_openapi_parameter) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_param()`", deparse(call$x))
}

is_openapi_parameters <- function(x) {
  inherits(x, "ambiorix_openapi_parameters")
}

assertthat::on_failure(is_openapi_parameters) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_parameters()`", deparse(call$x))
}

is_openapi_request_body <- function(x) {
  inherits(x, "ambiorix_openapi_request_body")
}

assertthat::on_failure(is_openapi_request_body) <- function(call, env) {
  sprintf(
    "`%s` must be created with `openapi_request_body()`",
    deparse(call$x)
  )
}

is_openapi_response <- function(x) {
  inherits(x, "ambiorix_openapi_response")
}

assertthat::on_failure(is_openapi_response) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_response()`", deparse(call$x))
}

is_openapi_responses <- function(x) {
  inherits(x, "ambiorix_openapi_responses")
}

assertthat::on_failure(is_openapi_responses) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_responses()`", deparse(call$x))
}

is_openapi_docs <- function(x) {
  inherits(x, "ambiorix_openapi_docs")
}

assertthat::on_failure(is_openapi_docs) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_docs()`", deparse(call$x))
}
