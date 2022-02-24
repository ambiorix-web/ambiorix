#' Log Predicate
#' 
#' Log predicate to bridge the previous log with the new
#' [log::Logger] package.
#' 
#' @noRd 
#' @keywords internal
logPredicate <- function(log){
  \() log
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
new_log <- function(prefix = ">", write = FALSE, 
  file = "ambiorix.log", sep = ""){

  log::Logger$new(
    prefix = prefix,
    write = write,
    file = file,
    sep = sep
  )$
    date()$
    time()
}

success <- \() {
  cli::col_green(cli::symbol$tick)
}

danger <- \() {
  cli::col_red(cli::symbol$cross)
}

info <- \() {
  cli::col_blue(cli::symbol$info)
}

warn <- \() {
  cli::col_yellow(cli::symbol$warning)
}
