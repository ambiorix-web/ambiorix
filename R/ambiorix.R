#' Ambiorix
#' 
#' Web server.
#' 
#' @field not_found 404 Response, defaults to [response_404()].
#' @field is_running Boolean indicating whether the server is running.
#' 
#' @importFrom assertthat assert_that
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

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = fun, 
        method = "GET",
        res = Response$new()
      )

      invisible(self)
    },
#' @details POST Method
#' 
#' Add routes to listen to.
#' 
#' @param path Route to listen to.
#' @param fun Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
    post = function(path, fun){
      assert_that(valid_path(path))
      assert_that(not_missing(fun))

      private$.routes[[uuid()]] <- list(
        route = Route$new(path), 
        path = path, 
        fun = fun, 
        method = "POST",
        res = Response$new()
      )

      invisible(self)
    },
#' @details Sets the 404 page.
#' @param fun Function that accepts the request and returns an object 
#' describing an httpuv response, e.g.: [response()].
    set_404 = function(fun){
      self$not_found <- fun
      invisible(self)
    },
#' @details Static directories
#' 
#' @param path Local path to directory of assets.
#' @param uri URL path where the directory will be available.
    serve_static = function(path, uri){
      assert_that(not_missing(uri))
      assert_that(not_missing(path))

      lst <- list(path)
      names(lst) <- uri
      private$.static <- append(private$.static, lst)
      invisible(self)
    },
#' @details Start 
#' Start the webserver.
    start = function(){
      private$.server <- httpuv::startServer(host = private$.host, port = private$.port,
        app = list(call = private$.call, staticPaths = private$.static)
      )
      msg <- sprintf("Listening on http://localhost:%d", private$.port)
      cli::cli_alert_success(msg)
      self$is_running <- TRUE
      invisible(self)
    },
#' @details Stop
#' Stop the webserver.
    stop = function(){
      private$.server$stop()
      cli::cli_alert_danger("Server Stopped")
      self$is_running <- FALSE
      invisible(self)
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
    .call = function(req){

      # loop over routes
      for(i in 1:length(private$.routes)){
        # if path matches pattern and method
        if(grepl(private$.routes[[i]]$route$pattern, req$PATH_INFO) && private$.routes[[i]]$method == req$REQUEST_METHOD){
          
          cli::cli_alert_success("{req$REQUEST_METHOD} {.val {req$PATH_INFO}}")

          # parse request
          req <- Request$new(req, private$.routes[[i]]$route)

          return(
            private$.routes[[i]]$fun(req, private$.routes[[i]]$res)
          )
        }
      }

      cli::cli_alert_warning("{req$REQUEST_METHOD} {.val {req$PATH_INFO}} - Not Found")

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