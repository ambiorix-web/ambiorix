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
    }
  ),
  private = list(
    .ws = NULL
  )
)
