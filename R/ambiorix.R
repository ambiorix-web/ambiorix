#' Ambiorix
#' 
#' Web server.
#' 
#' @field not_found 404 Response, must be a handler function that accepts the request and the response, by default uses [response_404()].
#' @field error 500 response when the route errors, must a handler function that accepts the request and the response, by default uses [response_500()].
#' @field websocket A handler function that accepts a websocket which overrides ambiorix internal websocket handling.
#' @field on_stop Callback function to run when the app stops, takes no argument.
#' @field port Port to run the application.
#' @field host Host to run the application.
#' 
#' @importFrom assertthat assert_that
#' @importFrom utils browseURL
#' @importFrom methods formalArgs
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
#' @return An object of class `Ambiorix` from which one can
#' add routes, routers, and run the application.
#' 
#' @export 
Ambiorix <- R6::R6Class(
  "Ambiorix",
  public = list(
    not_found = NULL,
    error = NULL,
    on_stop = NULL,
#' @details Define the webserver.
#' 
#' @param host A string defining the host.
#' @param port Integer defining the port, defaults to `ambiorix.port` option: uses a random port if `NULL`.
#' @param log Whether to generate a log of events.
    initialize = function(
      host = getOption("ambiorix.host", "0.0.0.0"), 
      port = getOption("ambiorix.port", NULL),
      log = getOption("ambiorix.logger", TRUE)
    ){

      .globals$infoLog$predicate <- logPredicate(log)
      .globals$errorLog$predicate <- logPredicate(log)
      .globals$successLog$predicate <- logPredicate(log)

      private$.host <- host
      private$.port <- get_port(host, port)

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
      assert_that(is_handler(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "GET",
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
      assert_that(is_handler(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "PUT",
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
      assert_that(is_handler(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "PATCH",
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
      assert_that(is_handler(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "DELETE",
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
      assert_that(is_handler(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "POST",
        error = error %error% self$error
      )

      invisible(self)
    },
#' @details OPTIONS Method
#'
#' Add routes to listen to.
#'
#' @param path Route to listen to.
#' @param handler Function that accepts the request and returns an object
#' describing an httpuv response, e.g.: [response()].
#' @param error Handler function to run on error.
    options = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))
      assert_that(is_handler(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path),
        path = path,
        fun = handler,
        method = "OPTIONS",
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
      assert_that(is_handler(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = c("GET", "POST", "PUT", "DELETE", "PATCH"),
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
      assert_that(not_missing(handler))
      assert_that(is_handler(handler))
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
#' @param host A string defining the host.
#' @param port Integer defining the port, defaults to `ambiorix.port` option: uses a random port if `NULL`.
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
#'  app$list(posrt = 3000L)
    start = function(
      port = NULL, host = NULL, open = interactive()) {
      if(private$.is_running){
        cli::cli_alert_warning("Server is already running")
        return()
      }

      if(is.null(port))
        port <- private$.port

      if(is.null(host))
        host <- private$.host

      private$.server <- httpuv::startServer(
        host = host, 
        port = port,
        app = list(
          call = private$.call, 
          staticPaths = private$.static, 
          onWSOpen = private$.wss,
          staticPathOptions = httpuv::staticPathOptions(
            html_charset = "utf-8",
            headers = list(
              "X-UA-Compatible" = "IE=edge,chrome=1"
            )
          ),
          onHeaders = function(req) {
            req$x <- 1L
            return(NULL)
          }
        )
      )

      url <- sprintf("http://localhost:%s", private$.port)
      
      .globals$successLog$log("Listening on", url)

      # runs
      private$.is_running <- TRUE

      # open
      browse_ambiorix(open, url)

      on.exit({
        self$stop()
      })

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

      if(!private$.is_running){
        .globals$errorLog$log("Server not running")
        return(invisible())
      }

      # run on stop
      if(!is.null(self$on_stop))
        self$on_stop()

      private$.server$stop()
      .globals$errorLog$log("Server stopped")

      private$.is_running <- FALSE

      invisible(self)
    },
#' @details Print
    print = function(){
      cli::cli_rule("Ambiorix", right = "web server")
      cli::cli_li("routes: {.val {private$.nRoutes()}}")
    },
#' @details Use a router or middleware
#' @param use Either a router as returned by [Router], a function to use as middleware,
#' or a `list` of functions.
#' If a function is passed, it must accept two arguments (the request, and the response): 
#' this function will be executed every time the server receives a request.
#' _Middleware may but does not have to return a response, unlike other methods such as `get`_
#' Note that multiple routers and middlewares can be used.
    use = function(use){
      assert_that(not_missing(use))
      
      # recurse through items
      if(is.list(use)) {
        for(i in 1:length(use)) {
          self$use(use[[i]])
        }
      }
      
      # mount router
      if(inherits(use, "Router")){
        private$.routes <- append(private$.routes, use$routes())
        private$.receivers <- append(private$.routes, use$receivers())
      } 
      
      if(is_cookie_parser(use)) {
        .globals$cookieParser <- use
        return(invisible(self))
      }

      if(is_cookie_preprocessor(use)) {
        .globals$cookiePreprocessors <- append(
          .globals$cookiePreprocessors,
          use
        )
        return(invisible(self))
      }

      # pass middleware
      if(is.function(use)) { 
        assert_that(is_handler(use))
        private$.middleware <- append(private$.middleware, use)
        return(invisible(self))
      }

      invisible(self)
    }
  ),
  active = list(
    websocket = function(value){
      if(missing(value))
        stop("This is a setter only")

      private$.wss <- value
    },
    port = function(value) {
      if(missing(value))
        return(value)

      private$.port <- value
    },
    host = function(value) {
      if(missing(value))
        return(value)

      private$.host <- value
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
    .middleware = list(),
    .is_running = FALSE,
    .call = function(req){

      request <- Request$new(req)
      res <- Response$new()

      if(length(private$.middleware) > 0){
        for(i in 1:length(private$.middleware)) {
          mid_res <- private$.middleware[[i]](request, res)

          if(is_response(mid_res))
            res <- mid_res
        }
      }

      # loop over routes
      for(i in 1:length(private$.routes)){
        # if path matches pattern and method
        if(grepl(private$.routes[[i]]$route$pattern, req$PATH_INFO) && req$REQUEST_METHOD %in% private$.routes[[i]]$method){
          
          .globals$infoLog$log(req$REQUEST_METHOD, "on", req$PATH_INFO)

          # parse request
          request$params <- set_params(request$PATH_INFO, private$.routes[[i]]$route)

          # get response
          response <- tryCatch(private$.routes[[i]]$fun(request, res),
            error = function(error){
              warning(error)
              private$.routes[[i]]$error(request, res)
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
                  .globals$errorLog$log(req$REQUEST_METHOD, "on", req$PATH_INFO, "-", "Server error")
                  private$.routes[[i]]$error(request, res)
                }
              )
            )
          }

          if(is_forward(response))
            next

          # if not a response return something that is
          return(
            response %response% response("Must return a response", status = 206L)
          )
        }
      }

      .globals$errorLog$log(request$REQUEST_METHOD, "on", request$PATH_INFO, "- Not found")

      # return 404
      request$params <- set_params(request$PATH_INFO, Route$new(request$PATH_INFO))
      return(self$not_found(request, res))
    },
    .wss = function(ws){

      # receive
      ws$onMessage(function(binary, message) {
        # don't run if no receiver
        if(length(private$.receivers) == 0) return(NULL)

        message <- jsonlite::fromJSON(message)

        for(i in 1:length(private$.receivers)){
          if(private$.receivers[[i]]$is_handler(message)){
            .globals$infoLog$log("Received message from websocket:", message$name)
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
