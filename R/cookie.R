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
    return(cookie_new)

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
