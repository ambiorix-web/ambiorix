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
#' @section OpenAPI:
#' `app$openapi()` enables OpenAPI (Swagger) documentation: routes registered
#' with a `docs` argument (see [openapi_docs()]) are collected into an OpenAPI
#' document, served alongside an interactive UI, and optionally used to
#' validate incoming requests.
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
    #'
    #' When OpenAPI documentation is enabled with `openapi()`, this is also
    #' where the docs routes are registered and the OpenAPI document is built,
    #' so any problem with the documentation is reported here rather than when
    #' the document is first requested.
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

      private$.build_openapi()

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
    #' The Swagger UI assets (CSS & JavaScript) are bundled with ambiorix and
    #' served locally at `assets_path` (default `/__swagger__`), so the docs
    #' work without an internet connection.
    #'
    #' If `ui_path` or `spec_path` collides with an existing route, or
    #' `assets_path` collides with an existing static directory, the
    #' corresponding docs route (or asset directory) is not registered and a
    #' warning is emitted.
    #' The OpenAPI document is always serialised with the default serialiser,
    #' regardless of any custom serialiser set via `serialiser()`. It is built
    #' once, when the app starts: problems with the documentation are reported
    #' then, rather than when the document is requested.
    #'
    #' With `validate = TRUE` incoming requests are checked against the
    #' documented schemas before the handler runs, and a `400` is returned if
    #' they do not match. Query and path parameters documented with a
    #' non-string schema are converted to their documented type. Individual
    #' routes may opt out with `openapi_docs(validate = FALSE)`.
    #'
    #' @param title String /// Optional. \cr
    #'   Title of the API. \cr
    #'   Defaults to `"API"`.
    #'
    #' @param version String /// Optional. \cr
    #'   Version of the API, as you version it; unrelated to the version of the
    #'   OpenAPI specification, which is always 3.1.0. \cr
    #'   Defaults to `"1.0.0"`.
    #'
    #' @param description String /// Optional. \cr
    #'   Description of the API, shown under the title in the UI. \cr
    #'   Defaults to `NULL`.
    #'
    #' @param info Named list /// Optional. \cr
    #'   Additional fields added to the OpenAPI
    #'   [info object](https://spec.openapis.org/oas/v3.1.0#info-object), e.g.
    #'   `contact` or `license`. \cr
    #'   Defaults to `list()`. `title`, `version`, and `description` are set
    #'   from the arguments above and override anything given here.
    #'
    #' @param servers Character vector or List /// Optional. \cr
    #'   URLs at which the API is served. A `list` of
    #'   [server objects](https://spec.openapis.org/oas/v3.1.0#server-object)
    #'   instead of a character vector allows each to carry a `description` or
    #'   `variables`. \cr
    #'   Defaults to `NULL`, which the specification reads as a server at `/`,
    #'   i.e. the host the document was served from.
    #'
    #' @param tags Character vector or List /// Optional. \cr
    #'   Tags used to group routes. Names, if any, are the tag names and the
    #'   values their descriptions; an unnamed element is a tag with no
    #'   description. \cr
    #'   Defaults to `NULL`. Declaring tags here is optional: a tag used by a
    #'   route appears in the UI either way, this is how it gets a description
    #'   and a fixed order.
    #'
    #' @param security_schemes Named list /// Optional. \cr
    #'   The [security schemes](https://spec.openapis.org/oas/v3.1.0#security-scheme-object)
    #'   the API supports, named so that routes can refer to them, e.g.
    #'   `list(bearerAuth = list(type = "http", scheme = "bearer"))`. \cr
    #'   Defaults to `NULL`. Declaring a scheme only documents it: ambiorix does
    #'   not authenticate anything.
    #'
    #' @param security Character vector or List /// Optional. \cr
    #'   Names of the security schemes that apply to every route; see the
    #'   `security` argument of [openapi_docs()], which overrides this per
    #'   route. \cr
    #'   Defaults to `NULL`, no authentication.
    #'
    #' @param validate Logical /// Optional. \cr
    #'   Whether to validate incoming requests against the documented schemas.
    #'   Either `FALSE` (default) or `TRUE`. \cr
    #'   Individual routes override this with the `validate` argument of
    #'   [openapi_docs()].
    #'
    #' @param ui_path String /// Optional. \cr
    #'   Path at which the Swagger UI is served. \cr
    #'   Defaults to `"/docs"`.
    #'
    #' @param spec_path String /// Optional. \cr
    #'   Path at which the OpenAPI JSON document is served. \cr
    #'   Defaults to `"/openapi.json"`.
    #'
    #' @param assets_path String /// Optional. \cr
    #'   Path at which the Swagger UI assets (CSS & JavaScript) are served. \cr
    #'   Defaults to `"/__swagger__"`. Worth changing only if it collides with
    #'   a static directory of your own.
    #'
    #' @return The `Ambiorix` object invisibly, so calls can be chained.
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
    #'     responses = list(
    #'       openapi_response(200, "The landing page")
    #'     )
    #'   )
    #' )
    #'
    #' if (interactive()) {
    #'   app$start()
    #' }
    #'
    #' # a fuller document: servers, described tags, a security scheme applied
    #' # to every route, and validation of incoming requests
    #' app <- Ambiorix$new()
    #'
    #' app$openapi(
    #'   title = "My API",
    #'   version = "2.0.0",
    #'   description = "An API built with ambiorix",
    #'   servers = c("https://api.example.com", "http://localhost:3000"),
    #'   tags = c(users = "Everything about users"),
    #'   security_schemes = list(
    #'     bearerAuth = list(type = "http", scheme = "bearer")
    #'   ),
    #'   security = "bearerAuth",
    #'   validate = TRUE
    #' )
    #'
    #' app$get(
    #'   "/users/:id",
    #'   function(req, res) {
    #'     # `id` is an integer here, not a string: validation converted it
    #'     res$json(list(id = req$params$id))
    #'   },
    #'   docs = openapi_docs(
    #'     summary = "Get a user by ID",
    #'     tags = "users",
    #'     parameters = openapi_param(
    #'       "id",
    #'       location = "path",
    #'       schema = openapi_schema_integer()
    #'     ),
    #'     responses = openapi_response(200, "The user")
    #'   )
    #' )
    #'
    #' if (interactive()) {
    #'   app$start()
    #' }
    openapi = function(
      title = "API",
      version = "1.0.0",
      description = NULL,
      info = list(),
      servers = NULL,
      tags = NULL,
      security_schemes = NULL,
      security = NULL,
      validate = FALSE,
      ui_path = "/docs",
      spec_path = "/openapi.json",
      assets_path = "/__swagger__"
    ) {
      assert_that(is_string(title))
      assert_that(is_string(version))
      assert_that(is.null(description) || is_string(description))
      assert_that(is.list(info))
      assert_that(is.null(servers) || is.character(servers) || is.list(servers))
      assert_that(is.null(tags) || is.character(tags) || is.list(tags))
      assert_that(is.null(security_schemes) || is.list(security_schemes))
      assert_that(
        is.null(security) || is.character(security) || is.list(security)
      )
      assert_that(is_flag(validate))
      assert_that(is_string(ui_path))
      assert_that(is_string(spec_path))
      assert_that(is_string(assets_path))

      info$title <- title
      info$version <- version

      if (!is.null(description)) {
        info$description <- description
      }

      private$.openapi_info <- info
      private$.openapi_servers <- servers
      private$.openapi_tags <- tags
      private$.openapi_security_schemes <- security_schemes
      private$.openapi_security <- security
      private$.openapi_validate <- validate
      private$.openapi_ui_path <- ui_path
      private$.openapi_spec_path <- spec_path
      private$.openapi_assets_path <- assets_path
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
    .openapi_servers = NULL,
    .openapi_tags = NULL,
    .openapi_security_schemes = NULL,
    .openapi_security = NULL,
    .openapi_validate = FALSE,
    .openapi_json = NULL,
    .openapi_schemas = list(),
    .openapi_ui_path = "/docs",
    .openapi_spec_path = "/openapi.json",
    .openapi_assets_path = "/__swagger__",
    n_routes = function() {
      length(private$.routes) + length(private$.static)
    },
    .make_path = function(path) {
      paste0(private$.basepath, path)
    },
    # Build and Serialise the OpenAPI Document
    #
    # The document is a pure function of the (flattened) routes, so it is
    # built once, at startup, where its diagnostics are visible, rather than
    # on every request to `spec_path`. Called from `start()` after the routes
    # have been flattened and reordered, so that routers' routes are included
    # with their basepaths already applied.
    #
    # Serialised with the default serialiser rather than a user-defined one,
    # which may not produce a valid OpenAPI document.
    #
    # Stores the JSON on `.openapi_json` and the named schemas, which request
    # validation needs to resolve references, on `.openapi_schemas`.
    #
    # Returns `self` invisibly. A no-op unless `app$openapi()` was called.
    .build_openapi = function() {
      if (!private$.openapi_enabled) {
        return(invisible(self))
      }

      spec <- build_openapi(
        routes = private$.routes,
        doc = list(
          info = private$.openapi_info,
          servers = private$.openapi_servers,
          tags = private$.openapi_tags,
          security_schemes = private$.openapi_security_schemes,
          security = private$.openapi_security
        )
      )

      # use the default serialiser: a user-defined serialiser
      # may not produce a valid OpenAPI document
      private$.openapi_json <- default_serialiser(spec)
      private$.openapi_schemas <- openapi_named_schemas(private$.routes)

      invisible(self)
    },
    # Register the Routes That Serve the Documentation
    #
    # Adds three things: a route serving the JSON document at `spec_path`, one
    # serving the Swagger UI at `ui_path`, and a static directory at
    # `assets_path` for the UI's bundled CSS and JavaScript.
    #
    # Called from `start()` *before* the routes are flattened, so that these
    # three are themselves picked up. They are registered through the ordinary
    # `get()` and `static()` methods, so nothing about them is special
    # afterwards.
    #
    # Each of the three is skipped, with a warning naming the argument that
    # moves it, if the path is already taken. The user's own route wins: it
    # was there first, and silently shadowing it would be worse than serving
    # no documentation.
    #
    # Returns `self` invisibly. A no-op unless `app$openapi()` was called, and
    # only ever runs once however many times `start()` is called.
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
      assets_path <- private$.openapi_assets_path
      info <- private$.openapi_info

      existing_paths <- vapply(
        X = super$get_routes(),
        FUN = function(route) {
          paste0(route$route$basepath, route$path)
        },
        FUN.VALUE = character(1)
      )

      if (spec_path %in% existing_paths) {
        cli::cli_alert_warning(
          paste(
            "Route {.val {spec_path}} is already registered:",
            "the OpenAPI document will not be served.",
            "Use the {.code spec_path} argument of {.code app$openapi()}",
            "to serve it at another path."
          )
        )
      } else {
        self$get(spec_path, function(req, res) {
          res$header_content_json()
          res$send(private$.openapi_json %||% "{}")
        })
      }

      if (ui_path %in% existing_paths) {
        cli::cli_alert_warning(
          paste(
            "Route {.val {ui_path}} is already registered:",
            "the OpenAPI Swagger UI will not be served.",
            "Use the {.code ui_path} argument of {.code app$openapi()}",
            "to serve it at another path."
          )
        )
      } else {
        ui_html <- swagger_ui_html(
          spec_path,
          title = info$title,
          assets_path = assets_path
        )
        self$get(ui_path, function(req, res) {
          res$send(ui_html)
        })
      }

      assets_uri <- sub("^/+", "", assets_path)
      if (assets_uri %in% names(private$.static)) {
        cli::cli_alert_warning(
          paste(
            "Static directory {.val {assets_path}} is already registered:",
            "the Swagger UI assets will not be served.",
            "Use the {.code assets_path} argument of {.code app$openapi()}",
            "to serve them at another path."
          )
        )
      } else {
        self$static(
          path = system.file("swagger-ui", package = "ambiorix"),
          uri = assets_uri
        )
      }

      private$.openapi_registered <- TRUE
      invisible(self)
    }
  )
)
