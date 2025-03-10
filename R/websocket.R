WebsocketHandler <- R6::R6Class(
  "WebsocketHandler",
  public = list(
    initialize = function(name, fun){
      assert_that(not_missing(fun))
      assert_that(not_missing(name))

      private$.fun <- fun
      private$.name <- name
    },
    receive = function(message, ws){
      args <- formalArgs(private$.fun)

      if(length(args) > 1) {
        ws <- Websocket$new(ws)
        private$.fun(message$message, ws)
      } else {
        private$.fun(message$message)
      }
    },
    is_handler = function(message){
      if(is.null(message$isAmbiorix))
        return(FALSE)

      private$.name == message$name
    },
    print = function(){
      foo <- paste0(deparse(private$.fun), collapse = "\n")
      cli::cli_alert_info("receive: {.code receive(message, ws)}")
      cli::cli_ul("Listening on message:")
      cli::cli_li("name: {.val {private$.name}}")
      cli::cli_end()
    }
  ),
  private = list(
    .name = NULL,
    .fun = NULL
  )
)

#' Websocket
#' 
#' Handle websocket messages.
#' 
#' @return A Websocket object.
#' @examples
#' # create an Ambiorix app with websocket support:
#' if (interactive()) {
#'   library(ambiorix)
#' 
#'   home_get <- function(req, res) {
#'     res$send("hello, world!")
#'   }
#' 
#'   greeting_ws_handler <- function(msg, ws) {
#'     cat("Received message:", "\n")
#'     print(msg)
#'     ws$send("greeting", "Hello from the server!")
#'   }
#' 
#'   app <- Ambiorix$new(port = 8080)
#'   app$get("/", home_get)
#'   app$receive("greeting", greeting_ws_handler)
#'   app$start()
#' }
#' 
#' # create websocket client from another R session:
#' if (interactive()) {
#'   client <- websocket::WebSocket$new("ws://127.0.0.1:8080", autoConnect = FALSE)
#' 
#'   client$onOpen(function(event) {
#'     cat("Connection opened\n")
#' 
#'     msg <- list(
#'       isAmbiorix = TRUE, # __MUST__ be set!
#'       name = "greeting",
#'       message = "Hello from the client!"
#'     )
#' 
#'     # serialise:
#'     msg <- yyjsonr::write_json_str(msg, auto_unbox = TRUE)
#' 
#'     client$send(msg)
#'   })
#' 
#'   client$onMessage(function(event) {
#'     cat("Received message from server:", event$data, "\n")
#'   })
#' 
#'   client$connect()
#' }
#' 
#' @export 
Websocket <- R6::R6Class(
  "Websocket",
  public = list(
    #' @details Constructor
    #' @param ws 
    initialize = function(ws){
      private$.ws <- ws
    },
    #' @details Send a message
    #' @param name Name, identifier, of the message.
    #' @param message Content of the message, anything that can be
    #' serialised to JSON.
    send = function(name, message){
      message <- list(
        name = name,
        message = message,
        isAmbiorix = TRUE
      )
      private$.ws$send(serialise(message))
    },
    #' @details Print
    print = function(){
      cli::cli_li("send: {.code send(name, message)}")
    }
  ),
  private = list(
    .ws = NULL
  )
)

#' Websocket Client
#' 
#' Handle ambiorix websocket client.
#' 
#' @param path Path to copy the file to.
#' 
#' @section Functions:
#' - `copy_websocket_client` Copies the websocket client file, useful when
#'   ambiorix was not setup with the ambiorix generator.
#' - `get_websocket_client_path` Retrieves the full path to the local websocket client.
#' - `get_websocket_clients` Retrieves clients connected to the server.
#' 
#' @name websocket_client
#' @return 
#' - `copy_websocket_client`: String. The new path (invisibly).
#' - `get_websocket_client_path`: String. The path to the local websocket client.
#' - `get_websocket_clients`: List. Websocket clients.
#' @examples
#' chat_ws <- function(msg, ws) {
#'   lapply(
#'     X = get_websocket_clients(),
#'     FUN = function(c) {
#'       c$send("chat", msg)
#'     }
#'   )
#' }
#' @export 
copy_websocket_client <- function(path){
  assert_that(not_missing(path))

  lib <- get_websocket_client_path()
  fs::file_copy(lib, path)
}

#' @rdname websocket_client
get_websocket_client_path <- function(){
  system.file("ambiorix.js", package = "ambiorix")
}

#' @rdname websocket_client
get_websocket_clients <- function() {
  return(.globals$wsc)
}
