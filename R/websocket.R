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
      private$.name == message$name
    },
    print = function(){
      foo <- paste0(deparse(private$.fun), collapse = "\n")
      cli::cli_alert_info("receive: {.code receive(message, ws)}")
      cli::cli_ul("Listening on message:")
      cli::cli_li("name: {.val {private$.name}}")
      cli::cli_li("fun: {.code {foo}}")
    }
  ),
  private = list(
    .name = NULL,
    .fun = NULL
  )
)

Websocket <- R6::R6Class(
  "Websocket",
  public = list(
    initialize = function(ws){
      private$.ws <- ws
    },
    send = function(name, message){
      message <- list(
        name = name,
        message = message
      )
      private$.ws$send(serialise(message))
    },
    print = function(){
      cli::cli_li("send: {.code send(name, message)}")
    }
  ),
  private = list(
    .ws = NULL
  )
)

#' Copy Websocket
#' 
#' Copies the websocket client file, useful when ambiorix was not setup with [create_ambiorix()].
#' 
#' @param path Path to copy the file to.
#' 
#' @export 
copy_websocket_client <- function(path){
  assert_that(not_missing(path))

  lib <- system.file("project/assets/ambiorix.js", package = "ambiorix")
  fs::file_copy(lib, path)
}