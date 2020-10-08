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
#' @param log Whether to generate a log of events.
    initialize = function(host = getOption("ambiorix.host", "0.0.0.0"), port = getOption("ambiorix.port", NULL),
      log = getOption("ambiorix.logger", FALSE)){

      private$.logger <- Logger$new(log)
      private$.host <- host
      private$.port <- get_port(port)
      self$not_found <- function(req, res){
        response_404()
      }
      self$error <- function(req, res){
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
#' @details All Methods
#' 
#' Add routes to listen to for all methods `GET`, `POST`, `PUT`, `DELETE`, and `PATCH`.
#' 
#' @param path Route to listen to.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
#' @param error Handler function to run on error.
    all = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      for(method in c("GET", "POST", "PUT", "DELETE", "PATCH")){
      private$.routes[[uuid()]] <- list(
          route = Route$new(path), 
          path = path, 
          fun = handler, 
          method = method,
          res = Response$new(),
          error = error %error% self$error
        )
      }

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
      private$.logger$write("Listening on", url)

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
        private$.logger$write("Server not running")
        cli::cli_alert_warning("Server is not running")
        return(invisible())
      }

      # run on stop
      if(!is.null(self$on_stop))
        self$on_stop()

      private$.server$stop()
      private$.logger$write("Server stopped")
      cli::cli_alert_danger("Server Stopped")
      self$is_running <- FALSE

      invisible(self)
    },
#' @details Print
    print = function(){
      cli::cli_rule("Ambiorix", right = "web server")
      cli::cli_li("routes: {.val {private$.nRoutes()}}")
    },
#' @details Use a router or middleware
#' @param use Either a router as returned by [Router] or a function to use as middleware.
#' If a function is passed, it must accept two arguments (the request, and the response): 
#' this function will be executed every time the server receives a request.
#' _Middleware may but does not have to return a response, unlike other methods such as `get`_
#' Note that multiple routers and middlewares can be used.
    use = function(use){

      assert_that(not_missing(use))
      
      # mount router
      if(inherits(use, "Router")){
        private$.routes <- append(private$.routes, use$routes())
        private$.receivers <- append(private$.routes, use$receivers())
      } else if(is.function(use)) { # pass middleware
        args <- formalArgs(use)
        assert_that(length(args) == 2, msg = "Use function must accept two arguments: the request, and the response")
        private$.middleware <- append(private$.middleware, use)
      }

      invisible(self)
    }
  ),
  active = list(
    websocket = function(value){
      private$.wss <- value
    }
  ),
  private = list(
    .logger = FALSE,
    .host = "0.0.0.0",
    .port = 3000,
    .app = list(),
    .quiet = TRUE,
    .server = NULL,
    .calls = NULL,
    .routes = list(),
    .static = list(),
    .receivers = list(),
    .middleware = list(),
    .call = function(req){

      # empty requests environment
      rm(list = ls(envir = .requests), envir = .requests) 

      request <- Request$new(req)

      if(!is.null(private$.middleware)){
        args <- list(request, Response$new())
        res <- lapply(private$.middleware, do.call, args = args)

        if(inherits(res, "ambiorixResponse"))
          return(res)

        if(inherits(response, "forward"))
          return()
      }

      # loop over routes
      for(i in 1:length(private$.routes)){
        # if path matches pattern and method
        if(grepl(private$.routes[[i]]$route$pattern, req$PATH_INFO) && private$.routes[[i]]$method == req$REQUEST_METHOD){
          
          cli::cli_alert_success("{req$REQUEST_METHOD} {.val {req$PATH_INFO}}")
          private$.logger$write(req$REQUEST_METHOD, "on", req$PATH_INFO, "by", paste0("'", req$HTTP_USER_AGENT, "'"))

          # parse request
          request$params <- set_params(request$PATH_INFO, private$.routes[[i]]$route)

          # get response
          response <- tryCatch(private$.routes[[i]]$fun(request, private$.routes[[i]]$res),
            error = function(error){
              warning(error)
              private$.routes[[i]]$error(request, private$.routes[[i]]$res)
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
                  private$.logger$write(req$REQUEST_METHOD, "on", req$PATH_INFO, "-", "Server error")
                  private$.routes[[i]]$error(request, private$.routes[[i]]$res)
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

      cli::cli_alert_warning("{req$REQUEST_METHOD} {.val {req$PATH_INFO}} - Not found")
      private$.logger$write(request$REQUEST_METHOD, "on", request$PATH_INFO, "- Not found")

      # return 404
      request$params <- set_params(request$PATH_INFO, Route$new(request$PATH_INFO))
      return(self$not_found(request, Response$new()))
    },
    .wss = function(ws){

      # receive
      ws$onMessage(function(binary, message) {

        message <- jsonlite::fromJSON(message)

        # don't run if not
        if(length(private$.receivers) == 0) return(NULL)

        for(i in 1:length(private$.receivers)){
          if(private$.receivers[[i]]$is_handler(message)){
            cli::cli_alert_info("Received websocket message: {.val {message$name}}")
            private$.logger$write("Received message from websocket:",)
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
