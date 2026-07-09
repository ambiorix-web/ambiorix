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
    ) {
      super$initialize()
      .globals$infoLog$predicate <- logPredicate(log)
      .globals$errorLog$predicate <- logPredicate(log)
      .globals$successLog$predicate <- logPredicate(log)

      private$.host <- host
      private$.port <- get_port(host, port)

      self$not_found <- function(req, res) {
        response_404()
      }

      self$error <- function(req, res, error) {
        message(conditionMessage(error))
        res$status <- 500L
        res$send("500: Internal Server Error")
      }

      invisible(self)
    },
    #' @details Cache templates in memory instead of reading
    #' them from disk.
    cache_templates = function() {
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
    listen = function(port) {
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
    set_404 = function(handler) {
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
    #' error_handler <- function(req, res, error) {
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
    #' whoami <- function(req, res) {
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
    static = function(path, uri = "www") {
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
      if (private$.is_running) {
        cli::cli_alert_warning("Server is already running")
        return()
      }
      if (is.null(port)) {
        port <- private$.port
      }

      if (is.null(host)) {
        host <- private$.host
      }

      port <- get_port(host, port)

      private$.register_openapi_routes()

      super$prepare()
      private$.routes <- super$get_routes()

      if (private$n_routes() == 0L) {
        stop("No routes specified")
      }

      private$.receivers <- super$get_receivers()
      private$.middleware <- super$get_middleware()
      private$.params <- super$get_params()

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
            if (private$.limit <= 0) {
              return(NULL)
            }

            if (length(req$CONTENT_LENGTH) > 0) {
              size <- as.numeric(req$CONTENT_LENGTH)
            } else if (length(req$HTTP_TRANSFER_ENCODING) > 0) {
              size <- Inf
            }

            if (size > private$.limit) {
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

      browser_host <- switch(
        EXPR = host,
        "0.0.0.0" = "127.0.0.1",
        host
      )

      browser_url <- sprintf("http://%s:%s", browser_host, port)

      .globals$successLog$log("Listening on", browser_url)

      # runs
      private$.is_running <- TRUE

      # open
      browse_ambiorix(open, browser_url)

      on.exit({
        self$stop()
      })

      # continually process requests:
      httpuv::service(timeoutMs = Inf)

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
    serialiser = function(handler) {
      assert_that(is_function(handler))
      options(AMBIORIX_SERIALISER = handler)
      invisible(self)
    },
    #' @details Enable OpenAPI (Swagger) documentation.
    #'
    #' When enabled, two routes are registered when the app starts: an
    #' interactive Swagger UI (`ui_path`, default `/docs`) and the OpenAPI JSON
    #' document (`spec_path`, default `/openapi.json`). Only routes registered
    #' with a `docs` argument (see [openapi_docs()]) appear in the document.
    #'
    #' If `ui_path` or `spec_path` collides with an existing route, the
    #' corresponding docs route is not registered and a warning is emitted.
    #' The OpenAPI document is always serialised with the default serialiser,
    #' regardless of any custom serialiser set via `serialiser()`.
    #'
    #' @param title Title of the API.
    #' @param version Version of the API.
    #' @param description Optional description of the API.
    #' @param ui_path Path at which the Swagger UI is served.
    #' @param spec_path Path at which the OpenAPI JSON document is served.
    #' @param ... Additional fields added to the OpenAPI `info` object.
    #'
    #' @examples
    #' app <- Ambiorix$new()
    #'
    #' app$openapi(title = "My API", version = "1.0.0")
    #'
    #' app$get(
    #'   "/",
    #'   function(req, res) {
    #'     res$send("Using {ambiorix}!")
    #'   },
    #'   docs = openapi_docs(
    #'     summary = "Landing page",
    #'     responses = openapi_responses(
    #'       openapi_response(200, "The landing page")
    #'     )
    #'   )
    #' )
    #'
    #' if (interactive())
    #'   app$start()
    openapi = function(
      title = "API",
      version = "1.0.0",
      description = NULL,
      ui_path = "/docs",
      spec_path = "/openapi.json",
      ...
    ) {
      assert_that(is_string(title))
      assert_that(is_string(version))
      assert_that(is_string(ui_path))
      assert_that(is_string(spec_path))

      info <- list(title = title, version = version, ...)
      if (!is.null(description)) {
        info$description <- description
      }

      private$.openapi_info <- info
      private$.openapi_ui_path <- ui_path
      private$.openapi_spec_path <- spec_path
      private$.openapi_enabled <- TRUE

      invisible(self)
    },
    #' @details Stop
    #' Stop the webserver.
    stop = function() {
      if (!private$.is_running) {
        .globals$errorLog$log("Server not running")
        return(invisible())
      }

      # run on stop
      if (!is.null(self$on_stop)) {
        self$on_stop()
      }

      private$.server$stop()
      .globals$errorLog$log("Server stopped")

      private$.is_running <- FALSE

      invisible(self)
    },
    #' @details Print
    print = function() {
      cli::cli_rule("Ambiorix", right = "web server")
      cli::cli_li("routes: {.val {private$n_routes()}}")
    }
  ),
  active = list(
    port = function(value) {
      if (missing(value)) {
        return(private$.port)
      }

      private$.port <- as.integer(value)
    },
    host = function(value) {
      if (missing(value)) {
        return(private$.host)
      }

      private$.host <- value
    },
    limit = function(value) {
      if (missing(value)) {
        return(private$.limit)
      }

      private$.limit <- as.integer(value)
    }
  ),
  private = list(
    .host = "0.0.0.0",
    .port = 3000,
    .server = NULL,
    .static = list(),
    .is_running = FALSE,
    .limit = 5 * 1024 * 1024,
    .openapi_enabled = FALSE,
    .openapi_registered = FALSE,
    .openapi_info = list(),
    .openapi_ui_path = "/docs",
    .openapi_spec_path = "/openapi.json",
    n_routes = function() {
      length(private$.routes) + length(private$.static)
    },
    .make_path = function(path) {
      paste0(private$.basepath, path)
    },
    .register_openapi_routes = function() {
      if (!private$.openapi_enabled) {
        return(invisible(self))
      }

      # guard against repeated `start()` calls
      if (private$.openapi_registered) {
        return(invisible(self))
      }

      spec_path <- private$.openapi_spec_path
      ui_path <- private$.openapi_ui_path

      existing_paths <- vapply(
        private$.routes,
        function(route) paste0(route$route$basepath, route$path),
        character(1)
      )

      for (p in c(spec_path, ui_path)) {
        if (p %in% existing_paths) {
          cli::cli_alert_warning(
            "OpenAPI docs path {.val {p}} is already registered; skipping."
          )
        }
      }

      spec <- build_openapi(private$.routes, private$.openapi_info)
      ui_html <- swagger_ui_html(spec_path)

      spec_route <- list(
        route = Route$new(spec_path),
        path = spec_path,
        fun = function(req, res) {
          res$json(spec)
        },
        method = "GET",
        error = NULL,
        docs = NULL
      )

      ui_route <- list(
        route = Route$new(ui_path),
        path = ui_path,
        fun = function(req, res) {
          res$send(ui_html)
        },
        method = "GET",
        error = NULL,
        docs = NULL
      )

      spec_route$route$decompose()
      spec_route$route$as_pattern()
      spec_route$route$basepath <- "/"

      ui_route$route$decompose()
      ui_route$route$as_pattern()
      ui_route$route$basepath <- "/"

      private$.routes <- append(
        private$.routes,
        list(spec_route, ui_route)
      )

      invisible(self)
    }
  )
)
