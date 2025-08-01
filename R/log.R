#' Log Predicate
#'
#' Log predicate to bridge the previous log with the new
#' [log::Logger] package.
#'
#' @noRd
#' @keywords internal
logPredicate <- function(log) {
  function() log
}

#' Logger
#'
#' Returns a new logger using the `log` package.
#'
#' @param prefix String to prefix all log messages.
#' @param write Whether to write the log to the `file`.
#' @param file Name of the file to dump the logs to,
#' only used if `write` is `TRUE`.
#' @param sep Separator between `prefix` and other
#' flags and messages.
#'
#' @examples
#' log <- new_log()
#' log$log("Hello world")
#'
#' @return An R& of class `log::Logger`.
#'
#' @export
new_log <- function(
  prefix = ">",
  write = FALSE,
  file = "ambiorix.log",
  sep = ""
) {
  log::Logger$new(
    prefix = prefix,
    write = write,
    file = file,
    sep = sep
  )$date()$time()
}

#' Customise logs
#'
#' Customise the internal logs used by Ambiorix.
#'
#' @param log An object of class `Logger`, see
#' [log::Logger].
#'
#' @name set_log
#' @return The `log` object.
#' @examples
#' # define custom loggers:
#' info_logger <- log::Logger$new("INFO")
#' success_logger <- log::Logger$new("SUCCESS")
#' error_logger <- log::Logger$new("ERROR")
#'
#' info_logger$log("This is an info message.")
#' success_logger$log("This is a success message.")
#' error_logger$log("This is an error message.")
#'
#' # set custom loggers for Ambiorix:
#' set_log_info(info_logger)
#' set_log_success(success_logger)
#' set_log_error(error_logger)
#' @export
set_log_info <- function(log) {
  assert_that(not_missing(log))
  assert_that(is_logger(log))

  .globals$infoLog <- log
}

#' @rdname set_log
#' @export
set_log_success <- function(log) {
  assert_that(not_missing(log))
  assert_that(is_logger(log))

  .globals$successLog <- log
}

#' @rdname set_log
#' @export
set_log_error <- function(log) {
  assert_that(not_missing(log))
  assert_that(is_logger(log))

  .globals$errorLog <- log
}

#' CLI symbols
#'
#' CLI Symbols for log
#'
#' @keywords internal
#' @noRd
success <- function() {
  cli::col_green(cli::symbol$tick)
}

#' @keywords internal
#' @noRd
error <- function() {
  cli::col_red(cli::symbol$cross)
}

#' @keywords internal
#' @noRd
info <- function() {
  cli::col_blue(cli::symbol$info)
}

#' @keywords internal
#' @noRd
warn <- function() {
  cli::col_yellow(cli::symbol$warning)
}
