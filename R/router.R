#' Router
#' 
#' Web server.
#' 
#' @field error 500 response when the route errors, must a handler function that accepts the request and the response, by default uses [response_500()].
#' 
#' @examples 
#' # log
#' logger <- new_log()
#' # router
#' # create router
#' router <- Router$new("/users")
#' 
#' router$get("/", function(req, res){
#'  res$send("List of users")
#' })
#' 
#' router$get("/:id", function(req, res){
#'  logger$log("Return user id:", req$params$id)
#'  res$send(req$params$id)
#' })
#' 
#' router$get("/:id/profile", function(req, res){
#'  msg <- sprintf("This is the profile of user #%s", req$params$id)
#'  res$send(msg)
#' })
#' 
#' # core app
#' app <- Ambiorix$new()
#' 
#' app$get("/", function(req, res){
#'  res$send("Home!")
#' })
#' 
#' # mount the router
#' app$use(router)
#' 
#' if(interactive())
#'  app$start()
#' 
#' @importFrom assertthat assert_that
#' @importFrom utils browseURL
#' 
#' @export
Router <- R6::R6Class(
  "Router",
  public = list(
    error = NULL,
#' @details Define the base route.
#' 
#' @param path The base path of the router.
    initialize = function(path){
      assert_that(not_missing(path))

      self$error <- function(res, req){
        response_500()
      }
      private$.basepath <- path
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
    get = function(path, handler, error = NULL){
      assert_that(valid_path(path))
      assert_that(not_missing(handler))

      private$.routes[[uuid()]] <- list(
        route = Route$new(private$.make_path(path)), 
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
        route = Route$new(private$.make_path(path)), 
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
        route = Route$new(private$.make_path(path)), 
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
        route = Route$new(private$.make_path(path)), 
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
        route = Route$new(private$.make_path(path)), 
        path = path, 
        fun = handler, 
        method = "POST",
        res = Response$new(),
        error = error %error% self$error
      )

      invisible(self)
    },
#' @details Receive Websocket Message
#' @param name Name of message.
#' @param handler Function to run when message is received.
    receive = function(name, handler){
      private$.receivers[[uuid()]] <- WebsocketHandler$new(name, handler)
      invisible(self)
    },
#' @details Print
    print = function(){
      cli::cli_rule("Ambiorix", right = "router")
      cli::cli_li("routes: {.val {private$.nRoutes()}}")
    },
#' @details Get the routes
    routes = function(){
      invisible(private$.routes)
    },
#' @details Get the receivers
    receivers = function(){
      invisible(private$.receivers)
    }
  ),
  private = list(
    .basepath = NULL,
    .routes = list(),
    .receivers = list(),
    .nRoutes = function(){
      length(private$.routes)
    },
    .make_path = function(path){
      paste0(private$.basepath, path)
    }
  )
)
