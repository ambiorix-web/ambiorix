#' Logger
#' 
#' Log events to `ambiorix.log`.
#' 
#' @field log Whether to actually log events to the file.
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
    log = FALSE,
#' @details Initialise
#' @param log Whether to log events, if `FALSE` the `write` method does not have any effect.
    initialize = function(log = TRUE){
      self$log <- log

      if(!log) return(self)

      exists <- fs::file_exists(here::here(private$.file))
      if(exists) return(self)
      file.create(here::here(private$.file))
      invisible(self)
    },
#' @details Write events to log file
#' @param label Label of event to log.
#' @param ... Any other text to write alongside.
    write = function(label,...){
      if(!self$log) return(invisible())

      label <- paste0("> ", Sys.time(), " - ", label)
      text <- paste(label, ..., collapse = " ")
      write(text, here::here(private$.file), append = TRUE)
    },
#' @details Print the logger state
    print = function(){
      status <- ifelse(self$log, "on", "off")
      cli::cli_alert_info("Ambiorix logger: {.strong {status}}")
    }
  ),
  private = list(
    .file = "ambiorix.log"
  )
)