#' Ambiorix
#' 
#' Web server.
#' 
#' @field not_found 404 Response, must be a handler function that accepts the request and the response, by default uses [response_404()].
#' @field is_running Boolean indicating whether the server is running.
#' @field error 500 response when the route errors, must a handler function that accepts the request and the response, by default uses [response_500()].
#' @field websocket A handler function that accepts a websocket which overrides ambiorix internal websocket handling.
#' @field on_stop Callback function to run when the app stops, takes no argument.
#' 
#' @importFrom assertthat assert_that
#' @importFrom utils browseURL
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#' 
#' app$on_stop <- function(){
#'  cat("Bye!\n")
#' }
#' 
#' if(interactive())
#'  app$start()
#' 
#' @export 
Ambiorix <- R6::R6Class(
  "Ambiorix",
  public = list(
    not_found = NULL,
    is_running = FALSE,
    error = NULL,
    on_stop = NULL,
#' @details Define the webserver.
#' 
#' @param host A string defining the host.
#' @param port Integer defining the port, defaults to `ambiorix.port` option: uses a random port if `NULL`.
    initialize = function(host = getOption("ambiorix.host", "0.0.0.0"), port = getOption("ambiorix.port", NULL)){

      if(is.null(port))
        port <- httpuv::randomPort()

      private$.host <- host
      private$.port <- as.integer(port)
      self$not_found <- function(res, req){
        response_404()
      }
      self$error <- function(res, req){
        response_500()
      }
      invisible(self)
    },
#' @details Specifies the port to listen on.
#' @param port Port number.
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$listen(3000L)
#' 
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#' 
#' if(interactive())
#'  app$start()
    listen = function(port){
      assert_that(not_missing(port))
      private$.port <- as.integer(port)
      invisible(self)
    },
#' @details GET Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to, `:` defines a parameter.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
#' @param error Handler function to run on error.
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#' 
#' if(interactive())
#'  app$start()
    get = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "GET",
        res = Response$new(),
        error = error %error% self$error
      )

      invisible(self)
    },
#' @details PUT Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to, `:` defines a parameter.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
#' @param error Handler function to run on error.
    put = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "PUT",
        res = Response$new(),
        error = error %error% self$error
      )

      invisible(self)
    },
#' @details PATCH Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to, `:` defines a parameter.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
#' @param error Handler function to run on error.
    patch = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "PATCH",
        res = Response$new(),
        error = error %error% self$error
      )

      invisible(self)
    },
#' @details DELETE Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to, `:` defines a parameter.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
#' @param error Handler function to run on error.
    delete = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "DELETE",
        res = Response$new(),
        error = error %error% self$error
      )

      invisible(self)
    },
#' @details POST Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
#' @param error Handler function to run on error.
    post = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "POST",
        res = Response$new(),
        error = error %error% self$error
      )

      invisible(self)
    },
#' @details Sets the 404 page.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$set_404(function(req, res){
#'  res$send("Nothing found here")
#' })
#' 
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#' 
#' if(interactive())
#'  app$start()
    set_404 = function(handler){
      self$not_found <- handler
      invisible(self)
    },
#' @details Static directories
#' 
#' @param path Local path to directory of assets.
#' @param uri URL path where the directory will be available.
    static = function(path, uri = "www"){
      assert_that(not_missing(uri))
      assert_that(not_missing(path))

      lst <- list(path)
      names(lst) <- uri
      private$.static <- append(private$.static, lst)
      invisible(self)
    },
#' @details Start 
#' Start the webserver.
#' @param auto_stop Whether to automatically stop the server when the functon exits.
#' @param open Whether to open the app the browser.
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#' 
#' if(interactive())
#'  app$start()
    start = function(auto_stop = TRUE, open = interactive()){
      
      if(self$is_running){
        cli::cli_alert_warning("Server is already running")
        return()
      }

      private$.server <- httpuv::startServer(host = private$.host, port = private$.port,
        app = list(call = private$.call, staticPaths = private$.static, onWSOpen = private$.wss)
      )

      url <- sprintf("http://localhost:%s", private$.port)
      
      # msg
      cli::cli_alert_success("Listening on {url}")

      # runs
      self$is_running <- TRUE

      # open
      browse_ambiorix(open, url)

      # stop the server
      if(auto_stop){
        on.exit({
          self$stop()
        })
      }

      # keep R "alive"
      while (TRUE) {
        httpuv::service()
      }

      invisible(self)
    },
#' @details Receive Websocket Message
#' @param name Name of message.
#' @param handler Function to run when message is received.
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#' 
#' app$receive("hello", function(msg, ws){
#'  print(msg) # print msg received
#'  
#'  # send a message back
#'  ws$send("hello", "Hello back! (sent from R)")
#' })
#' 
#' if(interactive())
#'  app$start()
    receive = function(name, handler){
      private$.receivers[[uuid()]] <- WebsocketHandler$new(name, handler)
      invisible(self)
    },
#' @details Define Serialiser
#' @param handler Function to use to serialise. 
#' This function should accept two arguments: the object to serialise and `...`.
#' 
#' @examples 
#' app <- Ambiorix$new()
#' 
#' app$serialiser(function(data, ...){
#'  jsonlite::toJSON(x, ..., pretty = TRUE)
#' })
#' 
#' app$get("/", function(req, res){
#'  res$send("Using {ambiorix}!")
#' })
#' 
#' if(interactive())
#'  app$start()
    serialiser = function(handler){
      options(AMBIORIX_SERIALISER = handler)
      invisible(self)
    },
#' @details Stop
#' Stop the webserver.
    stop = function(){

      if(!self$is_running){
        cli::cli_alert_warning("Server is not running")
        return(invisible())
      }

      # run on stop
      if(!is.null(self$on_stop))
        self$on_stop()

      private$.server$stop()
      cli::cli_alert_danger("Server Stopped")
      self$is_running <- FALSE

      invisible(self)
    },
#' @details Print
    print = function(){
      cli::cli_rule("Ambiorix", right = "web server")
      cli::cli_li("routes: {.val {private$.nRoutes()}}")
    },
#' @details Use a router
#' @param router The router as returned by [Router].
    use = function(router){
      if(!inherits(router, "Router"))
        stop("Must be a router, see Router")

      private$.routes <- append(private$.routes, router$routes())
      private$.receivers <- append(private$.routes, router$receivers())

      invisible(self)
    }
  ),
  active = list(
    websocket = function(value){
      private$.wss <- value
    }
  ),
  private = list(
    .host = "0.0.0.0",
    .port = 3000,
    .app = list(),
    .quiet = TRUE,
    .server = NULL,
    .calls = NULL,
    .routes = list(),
    .static = list(),
    .receivers = list(),
    .call = function(req){

      # loop over routes
      for(i in 1:length(private$.routes)){
        # if path matches pattern and method
        if(grepl(private$.routes[[i]]$route$pattern, req$PATH_INFO) && private$.routes[[i]]$method == req$REQUEST_METHOD){
          
          cli::cli_alert_success("{req$REQUEST_METHOD} {.val {req$PATH_INFO}}")

          # parse request
          req <- Request$new(req, private$.routes[[i]]$route)

          # get response
          response <- tryCatch(
            private$.routes[[i]]$fun(req, private$.routes[[i]]$res),
            error = function(error){
              message(error)
              private$.routes[[i]]$error(req, private$.routes[[i]]$res)
            }
          )

          if(promises::is.promising(response)){
            return(
              promises::then(
                response, 
                onFulfilled = function(response){
                  return(
                    response %response% response("Must return a response", status = 206L)
                  )
                },
                onRejected = function(error){
                  message(error)
                  private$.routes[[i]]$error(req, private$.routes[[i]]$res)
                }
              )
            )
          }

          if(inherits(response, "forward"))
            next

          # if not a response return something that is
          return(
            response %response% response("Must return a response", status = 206L)
          )
        }
      }

      cli::cli_alert_warning("{req$REQUEST_METHOD} {.val {req$PATH_INFO}} - Not Found")

      # return 404
      return(self$not_found(Request$new(req, Route$new(req$PATH_INFO)), Response$new()))
    },
    .wss = function(ws){

      # receive
      ws$onMessage(function(binary, message) {

        message <- jsonlite::fromJSON(message)

        # don't run if not
        if(length(private$.receivers) == 0) return(NULL)

        for(i in 1:length(private$.receivers)){
          if(private$.receivers[[i]]$is_handler(message)){
            return(private$.receivers[[i]]$receive(message, ws))
          }
        }

      })
    }, 
    nRoutes = function(){
      length(private$.routes)
    }
  )
)
