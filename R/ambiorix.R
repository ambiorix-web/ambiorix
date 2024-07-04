#' Ambiorix
#' 
#' Web server.
#' 
#' @field not_found 404 Response, must be a handler function that accepts the request and the response, by default uses [response_404()].
#' @field error 500 response when the route errors, must a handler function that accepts the request and the response, by default uses [response_500()].
#' @field on_stop Callback function to run when the app stops, takes no argument.
#' @field port Port to run the application.
#' @field host Host to run the application.
#' @field limit Max body size, defaults to `5 * 1024 * 1024`.
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
  inherit = Routing,
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
      super$initialize()
      .globals$infoLog$predicate <- logPredicate(log)
      .globals$errorLog$predicate <- logPredicate(log)
      .globals$successLog$predicate <- logPredicate(log)

      private$.host <- host
      private$.port <- get_port(host, port)

      self$not_found <- function(req, res){
        response_404()
      }

      invisible(self)
    },
    #' @details Cache templates in memory instead of reading
    #' them from disk.
    cache_templates = \(){
      .globals$cache_tmpls <- TRUE
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
#' @details Sets the error handler.
#' @param handler Function that accepts a request, response and an error object.
#' 
#' @examples 
#' # my custom error handler:
#' error_handler <- \(req, res, error) {
#'   if (!is.null(error)) {
#'     error_msg <- conditionMessage(error)
#'     cli::cli_alert_danger("Error: {error_msg}")
#'   }
#'   response <- list(
#'     code = 500L,
#'     msg = "Uhhmmm... Looks like there's an error from our side :("
#'   )
#'   res$
#'     set_status(500L)$
#'     json(response)
#' }
#'
#' # handler for GET at /whoami:
#' whoami <- \(req, res) {
#'   # simulate error (object 'Pikachu' is not defined)
#'   print(Pikachu)
#' }
#'
#' app <- Ambiorix$
#'   new()$
#'   set_error(error_handler)$
#'   get("/whoami", whoami)
#'
#' if (interactive()) {
#'   app$start(open = FALSE)
#' }
    set_error = function(handler) {
      assert_that(not_missing(handler))
      assert_that(is_error_handler(handler))
      self$error <- handler
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
#'  app$start(port = 3000L)
    start = function(
      port = NULL, 
      host = NULL, 
      open = interactive()
    ) {
      if(private$.is_running){
        cli::cli_alert_warning("Server is already running")
        return()
      }

      if(private$n_routes() == 0L)
        stop("No routes specified")

      if(is.null(port))
        port <- private$.port

      if(is.null(host))
        host <- private$.host

      super$reorder_routes()

      private$.server <- httpuv::startServer(
        host = host, 
        port = port,
        app = list(
          call = super$.call, 
          staticPaths = private$.static, 
          onWSOpen = super$websocket,
          staticPathOptions = httpuv::staticPathOptions(
            html_charset = "utf-8",
            headers = list(
              "X-UA-Compatible" = "IE=edge,chrome=1"
            )
          ),
          onHeaders = function(req) {
            size <- 0L
            if (private$.limit <= 0)
              return(NULL)

            if (length(req$CONTENT_LENGTH) > 0)
              size <- as.numeric(req$CONTENT_LENGTH)
            else if (length(req$HTTP_TRANSFER_ENCODING) > 0)
              size <- Inf

            if (size > private$.limit){
              .globals$errorLog$log("Request size exceeded, see app$limit")

              return(
                response(
                  "Maximum upload size exceeded",
                  status = 413L,
                  headers = list("Content-Type" = "text/plain")
                )
              )
            }

            return(NULL)
          }
        )
      )

      url <- sprintf("http://%s:%s", host, port)
      
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
      assert_that(is_function(handler))
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
      cli::cli_li("routes: {.val {private$n_routes()}}")
    }
  ),
  active = list(
    port = function(value) {
      if(missing(value))
        return(private$.port)

      private$.port <- as.integer(value)
    },
    host = function(value) {
      if(missing(value))
        return(private$.host)

      private$.host <- value
    },
    limit = function(value){
      if(missing(value))
        return(private$.limit)

      private$.limit <- as.integer(limit)
    }
  ),
  private = list(
    .host = "0.0.0.0",
    .port = 3000,
    .server = NULL,
    .static = list(),
    .is_running = FALSE,
    .limit = 5 * 1024 * 1024,
    n_routes = function(){
      length(private$.routes)
    },
    .make_path = function(path){
      paste0(private$.basepath, path)
    }
  )
)
