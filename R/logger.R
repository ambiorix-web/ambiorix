#' Logger
#' 
#' Log events to `ambiorix.log`.
#' 
#' @field run Whether to actually log events to the file.
#' 
#' @examples 
#' if(interactive()){
#' logger <- Logger$new()
#' 
#' log$write("Event", "happened!")
#' }
#' 
#' @details The logger prepends every `write` with the current timestamp obtained with [Sys.time()].
#' Every `write` is a single line in the log.
#' 
#' @export
Logger <- R6::R6Class(
  "Logger",
  public = list(
    run = FALSE,
#' @details Initialise
#' @param log Whether to log events, if `FALSE` the `write` method does not have any effect.
    initialize = function(log = TRUE){
      self$run <- log

      if(!log) return(self)

      exists <- fs::file_exists(here::here(private$.file))
      if(exists) return(self)
      file.create(here::here(private$.file))
      invisible(self)
    },
#' @details Write events to log file - deprecated
#' @param label Label of event to log.
#' @param ... Any other text to write alongside.
    write = function(label,...){
      .Deprecated("log", package = "ambiorix")
      if(!self$run) return(invisible())

      label <- paste0("> ", Sys.time(), " - ", label)
      text <- paste(label, ..., collapse = " ")
      write(text, here::here(private$.file), append = TRUE)
    },
#' @details Write events to log file
#' @param label Label of event to log.
#' @param ... Any other text to write alongside.
    log = function(label, ...){
      if(!self$run) return(invisible())

      label <- paste0("> ", Sys.time(), " - ", label)
      text <- paste(label, ..., collapse = " ")
      write(text, here::here(private$.file), append = TRUE)
    },
#' @details Print the logger state
    print = function(){
      status <- ifelse(self$run, "on", "off")
      cli::cli_alert_info("Ambiorix logger: {.strong {status}}")
    }
  ),
  private = list(
    .file = "ambiorix.log"
  )
)

#' Log Predicate
#' 
#' Log predicate to bridge the previous log with the new
#' [log::Logger] package.
#' 
#' @noRd 
#' @keywords internal
logPredicate <- function(log){
  return(
    function(){
      log
    }
  )
}