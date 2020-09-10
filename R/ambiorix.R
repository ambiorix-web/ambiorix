#' Ambiorix
#' 
#' Web server.
#' 
#' @field not_found 404 Response, defaults to [response_404()].
#' 
#' @importFrom assertthat assert_that
Ambiorix <- R6::R6Class(
  "Ambiorix",
  public = list(
    not_found = NULL,
#' @details Define the webserver.
#' 
#' @param host A string defining the host.
#' @param port Integer defining the port.
    initialize = function(host = "0.0.0.0", port = 3000L){
      private$.host <- host
      private$.port <- port
      self$not_found <- response_404
    },
#' @details GET Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to.
#' @param fun Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
    get = function(path, fun){
      assert_that(valid_path(path))
      assert_that(not_missing(fun))
      private$.routes[[uuid()]] <- list(path = path, fun = fun, method = "GET")
      invisible(self)
    },
#' @details POSTT Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to.
#' @param fun Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
    post = function(path, fun){
      assert_that(valid_path(path))
      assert_that(not_missing(fun))
      private$.routes[[uuid()]] <- list(path = path, fun = fun, method = "POST")
      invisible(self)
    },
#' @details Start 
#' Start the webserver.
    start = function(){
      private$.server <- httpuv::startServer(host = private$.host, port = private$.port,
        app = list(call = private$.call)
      )
      msg <- sprintf("Listening on http://127.0.0.1:%d", private$.port)
      cli::cli_alert_success(msg)
    },
#' @details Stop
#' Stop the webserver.
    stop = function(){
      private$.server$stop()
      cli::cli_alert_danger("Server Stopped")
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
    .call = function(req){

      # loop over routes
      for(i in 1:length(private$.routes)){
        if(private$.routes[[i]]$path == req$PATH_INFO && private$.routes[[i]]$method == req$REQUEST_METHOD){
          cli::cli_alert_success("GET 127.0.0.1:{private$.port}{req$PATH_INFO}")
          return(private$.routes[[i]]$fun(req))
        }
      }

      cli::cli_alert_warning("GET 127.0.0.1:{private$.port}{req$PATH_INFO} - Not Found")

      # return 404
      return(self$not_found())
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