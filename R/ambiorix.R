#' Ambiorix
#' 
#' Web server.
#' 
#' @field not_found 404 Response, defaults to [response_404()].
#' @field is_running Boolean indicating whether the server is running.
#' 
#' @importFrom assertthat assert_that
#' @importFrom utils browseURL
#' 
#' @export 
Ambiorix <- R6::R6Class(
  "Ambiorix",
  public = list(
    not_found = NULL,
    is_running = FALSE,
#' @details Define the webserver.
#' 
#' @param host A string defining the host.
#' @param port Integer defining the port.
    initialize = function(host = "0.0.0.0", port = 3000L){
      private$.host <- host
      private$.port <- as.integer(port)
      self$not_found <- function(res, req){
        response_404()
      }
    },
#' @details GET Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to, `:` defines a parameter.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
    get = function(path, handler){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "GET",
        res = Response$new()
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
    put = function(path, handler){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "PUT",
        res = Response$new()
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
    patch = function(path, handler){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "PATCH",
        res = Response$new()
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
    delete = function(path, handler){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "DELETE",
        res = Response$new()
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
    post = function(path, handler){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = handler, 
        method = "POST",
        res = Response$new()
      )

      invisible(self)
    },
#' @details Sets the 404 page.
#' @param handler Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
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
#' @param open Whether to open the app the browser.
    start = function(open = interactive()){
      
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

      invisible(self)
    },
#' @details Receive Websocket Message
#' @param name Name of message.
#' @param handler Function to run when message is received.
    receive = function(name, handler){
      private$.receivers[[uuid()]] <- WebsocketHandler$new(name, handler)
      invisible(self)
    },
#' @details Define Serialiser
#' @param handler Function to use to serialise. 
#' This function should accept a single argument: the object to serialise.
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

      private$.server$stop()
      cli::cli_alert_danger("Server Stopped")
      self$is_running <- FALSE
      invisible(self)
    },
#' @details Print
    print = function(){
      cli::cli_rule("Ambiorix", right = "web server")
      cli::cli_li("routes: {.val {private$.nRoutes()}}")
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

          return(
            private$.routes[[i]]$fun(req, private$.routes[[i]]$res) %response% response("Must return a response", status = 206L)
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
    },
    getRoutes = function(){
      sapply(private$.routes, function(x){
        x$path
      })
    }
  )
)