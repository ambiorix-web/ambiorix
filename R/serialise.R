#' Serialise
#'
#' Serialise data to JSON.
#'
#' @param data Data to serialise.
#' @param ... Named options to pass to [yyjsonr::write_json_str].
#'
#' @noRd
#' @keywords internal
default_serialiser <- function(data, ...) {
  dots <- list(...)

  # `yyjsonr::write_json_str()` accepts both `opts` & `...` but
  # `...` should override `opts`.
  # ensure that happens and use `opts` only:
  opts <- dots$opts
  if (is.null(opts)) {
    opts <- list()
  }

  dots$opts <- NULL
  opts[names(dots)] <- dots

  if (is.null(opts$auto_unbox)) {
    opts$auto_unbox <- TRUE
  }

  yyjsonr::write_json_str(data, opts = opts)
}

#' Retrieve Serialiser
#'
#' Retrieve the serialiser to use, either the default or that defined by user.
#'
#' @noRd
#' @keywords internal
get_serialise <- function() {
  getOption("AMBIORIX_SERIALISER", default_serialiser)
}

#' Serialise an Object to JSON
#'
#' @details
#' Ambiorix uses [yyjsonr::write_json_str()] by default for serialization.
#'
#' ### Custom Serialiser
#'
#' To override the default, set the `AMBIORIX_SERIALISER` option to a function that accepts:
#' - `data`: Object to serialise.
#' - `...`: Additional arguments passed to the function.
#'
#' For example:
#'
#' ```r
#' my_serialiser <- function(data, ...) {
#'  jsonlite::toJSON(x = data, ...)
#' }
#'
#' options(AMBIORIX_SERIALISER = my_serialiser)
#' ```
#'
#' @param data Data to serialise.
#' @param ... Passed to serialiser.
#'
#' @examples
#' if (interactive()) {
#'   # a list:
#'   response <- list(code = 200L, msg = "hello, world!")
#'
#'   serialise(response)
#'   #> {"code":200,"msg":"hello, world"}
#'
#'   serialise(response, auto_unbox = FALSE)
#'   #> {"code":[200],"msg":["hello, world"]}
#'
#'   # data.frame:
#'   serialise(cars)
#' }
#'
#' @export
serialise <- function(data, ...) {
  get_serialise()(data, ...)
}
