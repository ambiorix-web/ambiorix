#' Cookie Parser
#'
#' Parses the cookie string.
#'
#' @param req Request /// Required. \cr
#'            The [Request] whose cookie string is parsed. The string itself
#'            is on the request at `req$HTTP_COOKIE`.
#'
#' @examples
#' if (interactive()) {
#'   library(ambiorix)
#'
#'   #' Handle GET at '/greet'
#'   #'
#'   #' @export
#'   say_hello <- function(req, res) {
#'     cookies <- default_cookie_parser(req)
#'     print(cookies)
#'
#'     res$send("hello there!")
#'   }
#'
#'   app <- Ambiorix$new()
#'   app$get("/greet", say_hello)
#'   app$start()
#' }
#'
#' @return A `list` of key value pairs or cookie values.
#' @export
default_cookie_parser <- function(req) {
  cookie_new <- list()

  if (is.null(req$HTTP_COOKIE)) {
    return(cookie_new)
  }

  if (req$HTTP_COOKIE == "") {
    return(list())
  }

  split <- strsplit(req$HTTP_COOKIE, ";")[[1]]
  split <- strsplit(split, "=")
  for (i in seq_along(split)) {
    value <- trimws(split[[i]])

    if (length(value) < 2) {
      next
    }

    if (value[1] == "") {
      next
    }

    cookie_new[[value[1]]] <- value[2]
  }

  return(cookie_new)
}

#' Define a Cookie Parser
#'
#' Identifies a function as a cookie parser (see example).
#'
#' @param fn Function /// Required. \cr
#'           A function that accepts a single argument, `req` the [Request],
#'           and returns the parsed cookie string, generally a `list`. \cr
#'           Note that the original cookie string is available on the
#'           [Request] at the `HTTP_COOKIE` field, get it with
#'           `req$HTTP_COOKIE`.
#'
#' @examples
#' func <- function(req) {
#'  req$HTTP_COOKIE
#' }
#'
#' parser <- as_cookie_parser(func)
#'
#' app <- Ambiorix$new()
#' app$use(parser)
#'
#' @return Object of class "cookieParser".
#' @export
as_cookie_parser <- function(fn) {
  assert_that(not_missing(fn))
  assert_that(is_function(fn))

  fn <- structure(
    fn,
    class = c(
      "cookieParser",
      class(fn)
    )
  )

  invisible(fn)
}

#' @export
print.cookieParser <- function(x, ...) {
  cli::cli_alert_info("A cookie parser")
}

#' @keywords internal
#' @noRd
is_cookie_parser <- function(obj) {
  inherits(obj, "cookieParser")
}

#' Define a Cookie Preprocessor
#'
#' Identifies a function as a cookie preprocessor.
#'
#' @param fn Function /// Required. \cr
#'           A function that accepts the same arguments as the `cookie`
#'           method of the [Response] class (name, value, ...), and returns
#'           a modified `value`.
#'
#' @examples
#' func <- function(name, value, ...) {
#'  sprintf("prefix.%s", value)
#' }
#'
#' prep <- as_cookie_preprocessor(func)
#'
#' app <- Ambiorix$new()
#' app$use(prep)
#'
#' @return Object of class "cookiePreprocessor".
#' @export
as_cookie_preprocessor <- function(fn) {
  assert_that(not_missing(fn))
  assert_that(is_function(fn))

  fn <- structure(
    fn,
    class = c(
      "cookiePreprocessor",
      class(fn)
    )
  )

  invisible(fn)
}

#' @export
print.cookiePreprocessor <- function(x, ...) {
  cli::cli_alert_info("A cookie pre-processor")
}

#' @keywords internal
is_cookie_preprocessor <- function(obj) {
  inherits(obj, "cookiePreprocessor")
}

#' Cookie
#'
#' Create a cookie object.
#'
#' @param name String /// Required. \cr
#'             Name of the cookie.
#'
#' @param value String /// Required. \cr
#'              Value of the cookie.
#'
#' @param expires Integer, POSIXct, Date, or String /// Optional. \cr
#'                Expiry. An integer is taken as the number of seconds from
#'                now; a `character` string is set as-is and not
#'                pre-processed. \cr
#'                Defaults to `NULL`, which makes it a session cookie. A
#'                session finishes when the client shuts down, after which
#'                the session cookie is removed.
#'
#' @param max_age Integer /// Optional. \cr
#'                The number of seconds until the cookie expires. A zero or
#'                negative number expires it immediately. \cr
#'                Defaults to `NULL`. If both `expires` and `max_age` are
#'                set, the latter has precedence.
#'
#' @param domain String /// Optional. \cr
#'               The host to which the cookie will be sent. \cr
#'               Defaults to `NULL`, the host of the current document URL,
#'               not including subdomains.
#'
#' @param path String /// Optional. \cr
#'             The path that must exist in the requested URL for the browser
#'             to send the Cookie header. \cr
#'             Defaults to `NULL`.
#'
#' @param secure Logical /// Optional. \cr
#'               Whether the cookie is sent to the server only when a request
#'               is made with the https: scheme (except on localhost), and
#'               therefore is more resistant to man-in-the-middle attacks.
#'               Either `TRUE` (default) or `FALSE`.
#'
#' @param http_only Logical /// Optional. \cr
#'                  Whether to forbid JavaScript from accessing the cookie,
#'                  for example through the document.cookie property. Either
#'                  `TRUE` (default) or `FALSE`.
#'
#' @param same_site String /// Optional. \cr
#'                  Whether a cookie is sent with cross-origin requests,
#'                  providing some protection against cross-site request
#'                  forgery attacks (CSRF). Either `"Strict"`, `"Lax"`, or
#'                  `"None"`. \cr
#'                  Defaults to `NULL`.
#'
#' @keywords internal
#' @noRd
cookie <- function(
  name,
  value,
  expires = NULL,
  max_age = NULL,
  domain = NULL,
  path = NULL,
  secure = TRUE,
  http_only = TRUE,
  same_site = NULL
) {
  opts <- as.list(environment())
  structure(
    opts,
    class = c(
      "cookie",
      class(opts)
    )
  )
}

#' @export
print.cookie <- function(x, ...) {
  cli::cli_alert_info("A cookie: {.field {x$name}} = {.val  {x$value}}")
}
