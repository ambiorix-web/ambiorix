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

#' Is x a Valid OpenAPI Response Status?
#'
#' The specification allows three spellings for the key of a response: a
#' status code, a range such as `"2XX"` covering every code in that class, and
#' `"default"` for everything not documented explicitly. A numeric code must
#' be a whole number in 100-599.
#'
#' Paired with an `assertthat::on_failure()` message naming all three forms.
#'
#' @param x Object /// Required. \cr
#'          The value to check.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_status(200)
#'
#' is_openapi_status("2XX")
#'
#' is_openapi_status("default")
#'
#' is_openapi_status(999)
#'
#' is_openapi_status(200.5)
#'
#' @keywords internal
#' @noRd
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

#' Is Every Element of x Named?
#'
#' Used on the `...` of the OpenAPI constructors, where an unnamed value has
#' nowhere to go: a keyword or field is only meaningful with its name. Note
#' that partially named input fails, as does an empty name.
#'
#' Paired with an `assertthat::on_failure()` message.
#'
#' @param x List or Vector /// Required. \cr
#'          The object whose elements are checked.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' has_names(list(a = 1, b = 2))
#'
#' has_names(list(a = 1, 2))
#'
#' has_names(list(1, 2))
#'
#' @keywords internal
#' @noRd
has_names <- function(x) {
  nms <- names(x)
  !is.null(nms) && all(nzchar(nms))
}

assertthat::on_failure(has_names) <- function(call, env) {
  sprintf("all elements of `%s` must be named", deparse(call$x))
}

#' Is x an OpenAPI Schema?
#'
#' A class check. Guards every argument that takes a schema, so that a bare
#' `list(type = "string")` is rejected: it would render into the document
#' looking correct while skipping the keyword checking the constructors do.
#'
#' Paired with an `assertthat::on_failure()` message pointing at
#' `?openapi-schemas`.
#'
#' @param x Object /// Required. \cr
#'          The value to check.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_schema(openapi_schema_string())
#'
#' # a bare reference is still a schema
#' is_openapi_schema(openapi_schema_ref("User"))
#'
#' is_openapi_schema(list(type = "string"))
#'
#' @keywords internal
#' @noRd
is_openapi_schema <- function(x) {
  inherits(x, "ambiorix_openapi_schema")
}

assertthat::on_failure(is_openapi_schema) <- function(call, env) {
  sprintf(
    "`%s` must be an OpenAPI schema, see `?openapi-schemas`",
    deparse(call$x)
  )
}

#' Is Every Element of x an OpenAPI Schema?
#'
#' For the arguments that take several schemas at once: an object's
#' `properties` and a response's `headers`. An empty list passes, having no
#' element that is not a schema.
#'
#' Paired with an `assertthat::on_failure()` message pointing at
#' `?openapi-schemas`.
#'
#' @param x List /// Required. \cr
#'          The schemas to check, e.g. an object's `properties`.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_schema_list(list(id = openapi_schema_integer()))
#'
#' is_openapi_schema_list(list(id = openapi_schema_integer(), name = "string"))
#'
#' is_openapi_schema_list(list())
#'
#' @keywords internal
#' @noRd
is_openapi_schema_list <- function(x) {
  all(vapply(X = x, FUN = is_openapi_schema, FUN.VALUE = logical(1)))
}

assertthat::on_failure(is_openapi_schema_list) <- function(call, env) {
  sprintf(
    "every element of `%s` must be an OpenAPI schema, see `?openapi-schemas`",
    deparse(call$x)
  )
}

#' Is x a Valid Name for a Component?
#'
#' A named schema is reached by a `$ref` built from its name, so the name must
#' survive being pasted into a JSON pointer. The specification restricts
#' component keys to letters, digits, `.`, `_`, and `-`; a `/` or a space
#' would produce a reference that resolves to nothing.
#'
#' Paired with an `assertthat::on_failure()` message listing the allowed
#' characters.
#'
#' @param x String /// Required. \cr
#'          The component name to check.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_component_name("User")
#'
#' is_openapi_component_name("New_User.v2")
#'
#' is_openapi_component_name("New User")
#'
#' is_openapi_component_name("users/new")
#'
#' @keywords internal
#' @noRd
is_openapi_component_name <- function(x) {
  grepl("^[A-Za-z0-9._-]+$", x)
}

assertthat::on_failure(is_openapi_component_name) <- function(call, env) {
  sprintf(
    "`%s` may only contain letters, digits, `.`, `_`, and `-`",
    deparse(call$x)
  )
}

#' Is x an OpenAPI Parameter?
#'
#' A class check. Also used by `as_openapi_list()` to tell a single parameter
#' from a `list` of them, which matters because the object is itself a `list`.
#'
#' Paired with an `assertthat::on_failure()` message naming the constructor.
#'
#' @param x Object /// Required. \cr
#'          The value to check.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_parameter(openapi_param("id"))
#'
#' is_openapi_parameter(list(name = "id"))
#'
#' @keywords internal
#' @noRd
is_openapi_parameter <- function(x) {
  inherits(x, "ambiorix_openapi_parameter")
}

assertthat::on_failure(is_openapi_parameter) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_param()`", deparse(call$x))
}

#' Is x an OpenAPI Request Body?
#'
#' A class check, guarding the `request_body` argument of [openapi_docs()].
#'
#' Paired with an `assertthat::on_failure()` message naming the constructor.
#'
#' @param x Object /// Required. \cr
#'          The value to check.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_request_body(openapi_request_body(openapi_schema_object()))
#'
#' # a schema is not a request body
#' is_openapi_request_body(openapi_schema_object())
#'
#' @keywords internal
#' @noRd
is_openapi_request_body <- function(x) {
  inherits(x, "ambiorix_openapi_request_body")
}

assertthat::on_failure(is_openapi_request_body) <- function(call, env) {
  sprintf(
    "`%s` must be created with `openapi_request_body()`",
    deparse(call$x)
  )
}

#' Is x an OpenAPI Response?
#'
#' A class check. Also used by `as_openapi_list()` to tell a single response
#' from a `list` of them, which matters because the object is itself a `list`.
#'
#' Paired with an `assertthat::on_failure()` message naming the constructor.
#'
#' @param x Object /// Required. \cr
#'          The value to check.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_response(openapi_response(200, "OK"))
#'
#' is_openapi_response(list(status = 200))
#'
#' @keywords internal
#' @noRd
is_openapi_response <- function(x) {
  inherits(x, "ambiorix_openapi_response")
}

assertthat::on_failure(is_openapi_response) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_response()`", deparse(call$x))
}

#' Is x a Set of OpenAPI Route Docs?
#'
#' A class check, guarding the `docs` argument of every routing method. Also
#' the test `build_openapi()` and `.validate_request()` use to decide whether
#' a route is documented at all.
#'
#' Paired with an `assertthat::on_failure()` message naming the constructor.
#'
#' @param x Object /// Required. \cr
#'          The value to check.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' is_openapi_docs(openapi_docs(summary = "Get a user"))
#'
#' # what an undocumented route holds
#' is_openapi_docs(NULL)
#'
#' @keywords internal
#' @noRd
is_openapi_docs <- function(x) {
  inherits(x, "ambiorix_openapi_docs")
}

assertthat::on_failure(is_openapi_docs) <- function(call, env) {
  sprintf("`%s` must be created with `openapi_docs()`", deparse(call$x))
}
