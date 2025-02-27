#' Cookie Parser
#' 
#' Parses the cookie string.
#' 
#' @param req A [Request].
#' 
#' @return A `list` of key value pairs or cookie values.
#' 
#' @export 
default_cookie_parser <- function(req) {
  cookie_new <- list()

  if(is.null(req$HTTP_COOKIE))
    return(cookie_new)

  if(req$HTTP_COOKIE == "")
    return(list())

  split <- strsplit(req$HTTP_COOKIE, ";")[[1]]
  split <- strsplit(split, "=")
  for(i in 1:length(split)) {
    value <- trimws(split[[i]])

    if(length(value) < 2)
      next

    if(value[1] == "")
      next

    cookie_new[[value[1]]] <- value[2]
  }

  return(cookie_new)
}

#' Define a Cookie Parser
#' 
#' Identifies a function as a cookie parser (see example).
#' 
#' @param fn A function that accepts a single argument,
#' `req` the [Request] and returns the parsed cookie string,
#' generally a `list`. 
#' Note that the original cookie string is available on the
#' [Request] at the `HTTP_COOKIE` field, get it with:
#' `req$HTTP_COOKIE`
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
is_cookie_parser <- function(obj) {
  inherits(obj, "cookieParser")
}

#' Define a Cookie Preprocessor
#' 
#' Identifies a function as a cookie preprocessor.
#' 
#' @param fn A function that accepts the same arguments
#' as the `cookie` method of the [Response] class
#' (name, value, ...), and returns a modified 
#' `value`.
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
#' @param name Name of the cookie.
#' @param value value of the cookie.
#' @param expires Expiry, if an integer assumes it's the number of seconds
#' from now. Otherwise accepts an object of class `POSIXct` or `Date`.
#' If a `character` string then it is set as-is and not pre-processed.
#' If unspecified, the cookie becomes a session cookie. A session finishes 
#' when the client shuts down, after which the session cookie is removed. 
#' @param max_age Indicates the number of seconds until the cookie expires. 
#' A zero or negative number will expire the cookie immediately. 
#' If both `expires` and `max_age` are set, the latter has precedence.
#' @param domain Defines the host to which the cookie will be sent.
#' If omitted, this attribute defaults to the host of the current document URL,
#' not including subdomains.
#' @param path Indicates the path that must exist in the requested URL for the 
#' browser to send the Cookie header.
#' @param secure Indicates that the cookie is sent to the server only when a
#' request is made with the https: scheme (except on localhost), and therefore, 
#' is more resistant to man-in-the-middle attacks.
#' @param http_only Forbids JavaScript from accessing the cookie, for example,
#' through the document.cookie property.
#' @param same_site Controls whether or not a cookie is sent with cross-origin
#' requests, providing some protection against cross-site request forgery
#' attacks (CSRF). Accepts `Strict`, `Lax`, or `None`.
#' 
#' @keywords internal
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
