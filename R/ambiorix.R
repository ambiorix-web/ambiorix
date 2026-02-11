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
    #' describing a response, e.g.: [response()].
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
    #' @param port Integer defining the port, defaults to `ambiorix.port` option: uses a random port if `NULL`.
    #' @param host A string defining the host.
    #' @param open Whether to open the app the browser.
    #' @param tls TLS configuration for HTTPS. Can be a result from [generate_cert()],
    #' or a list with `cert` and `key` file paths.
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
      open = interactive(),
      tls = NULL
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

      super$prepare()
      private$.routes <- super$get_routes()
      private$.stream_handlers <- super$get_stream_handlers()

      if (private$n_routes() == 0L) {
        stop("No routes specified")
      }

      private$.receivers <- super$get_receivers()
      private$.middleware <- super$get_middleware()
      private$.params <- super$get_params()

      # build url
      scheme <- if (!is.null(tls)) "https" else "http"
      url <- sprintf("%s://%s:%s", scheme, host, port)

      # build tls config
      tls_config <- NULL
      if (!is.null(tls)) {
        if (is.list(tls) && !is.null(tls$server)) {
          tls_config <- nanonext::tls_config(server = tls$server)
        } else if (is.list(tls) && !is.null(tls$cert)) {
          tls_config <- nanonext::tls_config(server = c(tls$cert, tls$key))
        } else {
          tls_config <- nanonext::tls_config(server = tls)
        }
      }

      # create server
      private$.server <- nanonext::http_server(
        url = url,
        handlers = private$.build_handlers(),
        tls = tls_config
      )

      private$.server$start()

      # register server for stop_all()
      .globals$servers <- append(.globals$servers, list(private$.server))

      # get actual url (port may have been auto-assigned)
      actual_url <- private$.server$url
      browser_host <- switch(host, "0.0.0.0" = "127.0.0.1", host)
      browser_url <- sub(host, browser_host, actual_url)

      .globals$successLog$log("Listening on", browser_url)

      private$.is_running <- TRUE

      browse_ambiorix(open, browser_url)

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
    #' @details Stop
    #' Stop the webserver.
    stop = function() {
      if (!private$.is_running) {
        .globals$errorLog$log("Server not running")
        return(invisible())
      }

      if (!is.null(self$on_stop)) {
        self$on_stop()
      }

      private$.server$close()
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
    .stream_handlers = list(),
    .stream_connections = list(),
    .is_running = FALSE,
    .limit = 5 * 1024 * 1024,
    n_routes = function() {
      length(private$.routes) +
        length(private$.static) +
        length(private$.stream_handlers)
    },
    .make_path = function(path) {
      paste0(private$.basepath, path)
    },
    .build_handlers = function() {
      handlers <- list()

      # static file handlers (NNG serves directly)
      for (uri in names(private$.static)) {
        uri_path <- if (startsWith(uri, "/")) uri else paste0("/", uri)
        handlers <- append(
          handlers,
          list(
            nanonext::handler_directory(uri_path, private$.static[[uri]])
          )
        )
      }

      # stream handlers
      for (path in names(private$.stream_handlers)) {
        user_handler <- private$.stream_handlers[[path]]

        # create closure to capture path and handler
        make_stream_handler <- function(p, h) {
          force(p)
          force(h)
          nanonext::handler_stream(
            path = p,
            on_request = function(conn, req) {
              conn$set_header("Content-Type", "text/event-stream")
              conn$set_header("Cache-Control", "no-cache")
              conn$set_header("Connection", "keep-alive")

              stream_conn <- StreamConnection$new(conn)
              request <- Request$new(req)
              .globals$stream_connections[[as.character(
                conn$id
              )]] <- stream_conn

              h(request, stream_conn)
            },
            on_close = function(conn) {
              .globals$stream_connections[[as.character(conn$id)]] <- NULL
            }
          )
        }

        handlers <- append(
          handlers,
          list(
            make_stream_handler(path, user_handler)
          )
        )
      }

      # websocket handler (root path)
      if (length(private$.receivers) > 0) {
        handlers <- append(
          handlers,
          list(
            nanonext::handler_ws(
              path = "/",
              on_message = function(ws, data) {
                private$.ws_on_message(ws, data)
              },
              on_open = function(ws) {
                private$.ws_on_open(ws)
              },
              on_close = function(ws) {
                private$.ws_on_close(ws)
              },
              textframes = TRUE
            )
          )
        )
      }

      # catch-all handler for dynamic routes
      handlers <- append(
        handlers,
        list(
          nanonext::handler(
            path = "",
            callback = function(req) {
              # check body size limit
              if (private$.limit > 0) {
                size <- length(req$body)
                if (size > private$.limit) {
                  .globals$errorLog$log("Request size exceeded, see app$limit")
                  return(list(
                    status = 413L,
                    headers = c("Content-Type" = "text/plain"),
                    body = "Maximum upload size exceeded"
                  ))
                }
              }

              private$.call(req)
            },
            method = "*",
            prefix = TRUE
          )
        )
      )

      handlers
    }
  )
)
