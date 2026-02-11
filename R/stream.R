#' Stream Connection
#'
#' Connection object for streaming HTTP responses (SSE, NDJSON, etc.).
#'
#' @field id Unique connection identifier.
#'
#' @return A StreamConnection object.
#' @examples
#' if (interactive()) {
#'   library(ambiorix)
#'
#'   app <- Ambiorix$new()
#'
#'   app$stream("/events", function(req, conn) {
#'     conn$sse(data = "connected", event = "open")
#'   })
#'
#'   app$start()
#' }
#'
#' @export
StreamConnection <- R6::R6Class(
  "StreamConnection",
  public = list(
    id = NULL,
    #' @details Constructor
    #' @param conn The nanonext connection object.
    initialize = function(conn) {
      private$.conn <- conn
      self$id <- conn$id
    },
    #' @details Send an SSE-formatted message
    #' @param data The data payload.
    #' @param event Optional event type.
    #' @param id Optional event ID for client reconnection.
    #' @param retry Optional retry interval in milliseconds.
    sse = function(data, event = NULL, id = NULL, retry = NULL) {
      if (is.list(data) || is.data.frame(data)) {
        data <- serialise(data)
      }
      msg <- nanonext::format_sse(
        data = data,
        event = event,
        id = id,
        retry = retry
      )
      private$.conn$send(msg)
      invisible(self)
    },
    #' @details Send raw data (for NDJSON, custom formats)
    #' @param data Data to send.
    send = function(data) {
      private$.conn$send(data)
      invisible(self)
    },
    #' @details Close the connection
    close = function() {
      private$.conn$close()
      invisible(self)
    },
    #' @details Set HTTP status code (before first send)
    #' @param code HTTP status code.
    set_status = function(code) {
      private$.conn$set_status(as.integer(code))
      invisible(self)
    },
    #' @details Set a response header (before first send)
    #' @param name Header name.
    #' @param value Header value.
    set_header = function(name, value) {
      private$.conn$set_header(name, value)
      invisible(self)
    },
    #' @details Print
    print = function() {
      cli::cli_h3("StreamConnection")
      cli::cli_li("id: {.val {self$id}}")
    }
  ),
  private = list(
    .conn = NULL
  )
)

#' Get Stream Connections
#'
#' Retrieve active stream connections.
#'
#' @return List of StreamConnection objects.
#' @export
get_stream_connections <- function() {
  .globals$stream_connections
}
