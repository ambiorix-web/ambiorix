#' Routing HTTP Methods
#'
#' Register route handlers for HTTP verbs on a [`Routing`] instance.
#'
#' The routing helpers provide a fluent API for attaching handlers to HTTP
#' methods. Each helper shares the same signature and behaviour.
#'
#' ## Supported helpers
#'
#' - `get()`: Respond to HTTP `GET` requests.
#' - `post()`: Respond to HTTP `POST` requests.
#' - `put()`: Respond to HTTP `PUT` requests.
#' - `patch()`: Respond to HTTP `PATCH` requests.
#' - `delete()`: Respond to HTTP `DELETE` requests.
#' - `options()`: Respond to HTTP `OPTIONS` requests.
#' - `all()`: Respond to every method above.
#'
#' ## Path matching
#'
#' Paths are treated as regular expressions; use `:` to define a parameter
#' (e.g. `"/hello/:name"`).
#'
#' - A parameter matches one path segment: `"/users/:res"` matches
#'   `/users/1`, not `/users/2/3`, `/users/1/` or `/users/`.
#' - Exact paths are tried before parameters, whichever router they are on,
#'   so `/users/me` is matched before `/users/:id`.
#' - Regular expression syntax is available for finer control, e.g.
#'   `app$get("/users/.+", ...)` to match across `/` without a parameter, or
#'   `app$get("/file\\.json", ...)` to match a literal dot (an unescaped `.`
#'   matches any character).
#' - To customise how paths are converted to patterns app-wide, see
#'   [as_path_to_pattern()].
#'
#' @param path String /// Required. \cr
#'             Route to listen to, treated as a regular expression; use `:`
#'             to define a parameter (e.g. `"/hello/:name"`). See the
#'             *Path matching* section.
#'
#' @param handler Function /// Required. \cr
#'                A function that accepts the request and response objects
#'                and returns an httpuv response (e.g. [response()]).
#'                Handlers can return the result of helper functions such as
#'                `Response$text()`, `Response$json()`, or the output of any
#'                renderer.
#'
#' @param error Function /// Optional. \cr
#'              A handler invoked if the route raises an error; receives the \cr
#'              request, response, and the error condition. \cr
#'              It answers for everything the route runs, not just `handler`: \cr
#'              a middleware, a parameter middleware, and request validation \cr
#'              with its `on_invalid`. \cr
#'              Defaults to `NULL`, the app's error handler.
#'
#' @param docs OpenAPI docs /// Optional. \cr
#'             Documentation for the route, created with [openapi_docs()].
#'             When the app enables docs via `app$openapi()`, documented
#'             routes appear in the generated OpenAPI document, and, if
#'             validation is enabled, incoming requests are checked against
#'             the documented schemas before the handler runs. \cr
#'             Path parameters are documented automatically from the route's
#'             `:param` tokens with a string schema; declare them via
#'             [openapi_param()] with `location = "path"` to override that
#'             default. \cr
#'             Defaults to `NULL`, which leaves the route out of the
#'             document entirely.
#'
#' @return The routing object invisibly so calls can be chained.
#'
#' @examples
#' app <- Ambiorix$new(port = 3000L)
#'
#' app$get("/", function(req, res) {
#'   res$text("Hello, world!")
#' })
#'
#' app$post("/echo", function(req, res) {
#'   body <- req$parse_json()
#'   res$json(list(received = body))
#' })
#'
#' app$all("/health", function(req, res) {
#'   res$json(list(status = "ok"))
#' })
#'
#' app$get(
#'   "/users/:id",
#'   function(req, res) {
#'     res$json(list(id = req$params$id))
#'   },
#'   docs = openapi_docs(
#'     summary = "Get a user by ID",
#'     tags = "users",
#'     responses = list(
#'       openapi_response(200, "The user")
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   app$start()
#' }
#'
#' @seealso [`Routing`], [openapi_docs()]
#'
#' @name routing-http-methods
NULL

#' Core Routing Class
#'
#' Core routing class.
#' Do not use directly, see [Ambiorix], and [Router].
#'
#' @field error Error handler, see `set_error()`.
#' @field get Register a route handler for HTTP GET requests. See
#'   [routing-http-methods].
#' @field put Register a route handler for HTTP PUT requests. See
#'   [routing-http-methods].
#' @field patch Register a route handler for HTTP PATCH requests. See
#'   [routing-http-methods].
#' @field delete Register a route handler for HTTP DELETE requests. See
#'   [routing-http-methods].
#' @field post Register a route handler for HTTP POST requests. See
#'   [routing-http-methods].
#' @field options Register a route handler for HTTP OPTIONS requests. See
#'   [routing-http-methods].
#' @field all Register a route handler that responds to every HTTP verb used by
#'   Ambiorix. See [routing-http-methods].
#' @field basepath Basepath, read-only.
#' @field websocket Websocket handler.
#' @section HTTP methods:
#' See [routing-http-methods] for the full argument reference. The routing
#' instance exposes helpers for common HTTP verbs; they are registered when the
#' object is initialised and share the same signature.
#'
#' - `get()`, `put()`, `patch()`, `delete()`, `post()`, `options()` register a
#'   handler for the single corresponding HTTP verb; see
#'   [routing-http-methods].
#' - `all()` registers a handler that responds to `GET`, `POST`, `PUT`,
#'   `DELETE`, and `PATCH`; see [routing-http-methods].
#'
#' @return A Routing object.
#' @seealso [routing-http-methods]
#' @keywords export
Routing <- R6::R6Class(
  "Routing",
  public = list(
    error = NULL,
    get = NULL,
    put = NULL,
    patch = NULL,
    delete = NULL,
    post = NULL,
    options = NULL,
    all = NULL,
    #' @details Initialise
    #'
    #' @param path String /// Optional. \cr
    #'   Prefix path. \cr
    #'   Defaults to `""`, no prefix.
    #'
    initialize = function(path = "") {
      private$.basepath <- path
      private$.is_router <- path != ""
      private$.register_http_methods()
    },
    #' @details Sets the error handler.
    #'
    #' It answers an error raised by any route of this router, or of a
    #' router mounted on it, that has no `error` handler of its own. A
    #' router without one falls back to the router it is mounted on, and
    #' in the end to the app's.
    #'
    #' @param handler Function /// Required. \cr
    #'   A function that accepts a request, a response, and an error object.
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
    #' @details Add a parameter middleware
    #'
    #' It runs for every route of this router, and of the routers mounted
    #' on it, whose path has a `:name` parameter. It runs after the
    #' middleware and the request validation, see `app$openapi()`, so
    #' `value` is the validated value when the route is documented, and
    #' right before the handler. A `forward()` runs it again for the next
    #' matching route. Return a response to answer the request there.
    #'
    #' @param name Character vector /// Required. \cr
    #'   Name(s) of the parameter.
    #'
    #' @param handler Function /// Required. \cr
    #'   A function that accepts the request, response, parameter value, and
    #'   the parameter name.
    #'
    #' @examples
    #' app <- Ambiorix$new()
    #'
    #' app$get("/", function(req,res){
    #'  res$send("Hello!")
    #' })
    #'
    #' app$param("person", function(req, res, value, name){
    #'  if(value == "notWanted"){
    #'   res$status <- 403L
    #'   res$send("This is the end.")
    #'  }
    #'
    #'  # continue processing the request...
    #' })
    #'
    #' app$get("/hi/:person", function(req,res){
    #'  res$sendf("Hi! %s", req$params$person)
    #' })
    #' app$get("/info/:person", function(req,res){
    #'  res$sendf("Here is all your info, %s", req$params$person)
    #' })
    #' if(interactive())
    #'  app$start()
    param = function(name, handler) {
      assert_that(not_missing(handler))
      assert_that(is_param_handler(handler))
      p <- lapply(name, function(s) {
        list(
          handler = handler,
          params = s
        )
      })
      private$.params <- append(private$.params, p)
      invisible(self)
    },
    #' @details Receive Websocket Message
    #'
    #' @param name String /// Required. \cr
    #'   Name of the message.
    #'
    #' @param handler Function /// Required. \cr
    #'   A function to run when the message is received. Its first argument
    #'   is the message, parsed the way `req$parse_json()` parses a request
    #'   body, see [parse_json()]; the second, if it takes one, is the
    #'   [Websocket] to answer on.
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
    receive = function(name, handler) {
      private$.receivers <- append(
        private$.receivers,
        list(WebsocketHandler$new(name, handler))
      )

      invisible(self)
    },
    #' @details Print
    print = function() {
      cli::cli_rule("Ambiorix", right = "web server")
      cli::cli_li("routes: {.val {private$n_routes()}}")
    },
    #' @details Engine to use for rendering templates.
    #'
    #' @param engine Function /// Required. \cr
    #'   The engine to render templates with.
    #'
    engine = function(engine) {
      if (!is_renderer_obj(engine)) {
        engine <- as_renderer(engine)
      }

      self$use(engine)
      invisible(self)
    },
    #' @details Use a router or middleware
    #'
    #' @param use Router, Function, or List /// Required. \cr
    #'   Either a router as returned by [Router], a function to use as
    #'   middleware, or a `list` of functions. \cr
    #'   If a function is passed, it must accept two arguments (the request,
    #'   and the response): this function will be executed every time the
    #'   server receives a request.
    #' _Middleware may but does not have to return a response, unlike other methods such as `get`_
    #' Note that multiple routers and middlewares can be used.
    use = function(use) {
      assert_that(not_missing(use))

      # recurse through items
      if (is.list(use)) {
        for (i in seq_along(use)) {
          self$use(use[[i]])
        }
      }

      # mount router
      if (inherits(use, "Router")) {
        private$.routers <- append(private$.routers, use)
      }

      if (is_renderer_obj(use) && private$.is_router) {
        .globals$errorLog$log(
          "Cannot pass cookie preprocessor to `Router`, only to `Ambiorix`"
        )
        return(invisible(self))
      }

      if (is_renderer_obj(use)) {
        .Deprecated(
          "engine",
          package = "ambiorix",
          msg = "Use `engine` instead of `use` for custom renderers."
        )
        .globals$renderer <- use
        return(invisible(self))
      }

      if (is_cookie_parser(use) && private$.is_router) {
        .globals$errorLog$log(
          "Cannot pass cookie parser to `Router`, only to `Ambiorix`"
        )
        return(invisible(self))
      }

      if (is_path_to_pattern(use) && private$.is_router) {
        .globals$errorLog$log(
          "Cannot pass path to pattern converter to `Router`, only to `Ambiorix`"
        )
        return(invisible(self))
      }

      if (is_cookie_parser(use)) {
        .globals$cookieParser <- use
        return(invisible(self))
      }

      if (is_path_to_pattern(use)) {
        .globals$pathToPattern <- use
        return(invisible(self))
      }

      if (is_cookie_preprocessor(use) && private$.is_router) {
        .globals$errorLog$log(
          "Cannot pass cookie preprocessor to `Router`, only to `Ambiorix`"
        )
        return(invisible(self))
      }

      if (is_cookie_preprocessor(use)) {
        .globals$cookiePreprocessors <- append(
          .globals$cookiePreprocessors,
          use
        )
        return(invisible(self))
      }

      # pass middleware
      if (is.function(use)) {
        assert_that(is_handler(use))
        private$.middleware <- append(private$.middleware, use)
        return(invisible(self))
      }

      invisible(self)
    },
    #' @details Get the routes, compiled from their full path: the
    #' basepaths of the routers they are mounted under included.
    #'
    #' @param parent String /// Optional. \cr
    #'               Parent path. \cr
    #'               Defaults to `""`.
    #'
    get_routes = function(parent = "") {
      routes <- lapply(
        private$.routes,
        function(route) {
          # a copy per mount: a router mounted twice compiles to two paths
          route$route <- route$route$clone()
          route$route$compile(parent)
          route$route$basepath <- paste0(parent, private$.basepath)
          route
        }
      )

      parent <- paste0(parent, private$.basepath)

      for (router in private$.routers) {
        routes <- append(routes, router$get_routes(parent))
      }

      # a route without its own error handler takes the nearest router's,
      # then the app's: each level fills what the levels below left empty
      routes <- lapply(routes, function(route) {
        route$error <- route$error %||% self$error
        route
      })

      # fewer parameters is more specific, across every router: `/users/me`
      # is tried before `/users/:id`, and that before `/:kind/:id`
      n_params <- vapply(
        routes,
        function(route) length(route$route$params),
        integer(1)
      )
      nchars <- vapply(
        routes,
        function(route) nchar(route$route$pattern),
        integer(1)
      )
      routes[order(n_params, -nchars)]
    },
    #' @details Get the parameter middlewares
    #'
    #' @param params List /// Optional. \cr
    #'   Existing list of parameter middlewares. \cr
    #'   Defaults to `list()`.
    #'
    #' @param parent String /// Optional. \cr
    #'   Parent path. \cr
    #'   Defaults to `""`.
    #'
    get_params = function(params = list(), parent = "") {
      params <- append(
        params,
        lapply(
          private$.params,
          function(fn) {
            attr(fn, "basepath") <- paste0(parent, private$.basepath)
            return(fn)
          }
        )
      )

      if (!length(private$.routers)) {
        return(params)
      }

      parent <- paste0(parent, private$.basepath)

      for (router in private$.routers) {
        params <- router$get_params(params, parent)
      }

      return(params)
    },
    #' @details Get the websocket receivers
    #'
    #' @param receivers List /// Optional. \cr
    #'   Existing list of receivers. \cr
    #'   Defaults to `list()`.
    #'
    get_receivers = function(receivers = list()) {
      receivers <- append(receivers, private$.receivers)

      if (!length(private$.routers)) {
        return(receivers)
      }

      for (router in private$.routers) {
        receivers <- router$get_receivers(receivers)
      }

      return(receivers)
    },
    #' @details Get the middleware
    #'
    #' @param middlewares List /// Optional. \cr
    #'   Existing list of middlewares. \cr
    #'   Defaults to `list()`.
    #'
    #' @param parent String /// Optional. \cr
    #'   Parent path. \cr
    #'   Defaults to `""`.
    #'
    get_middleware = function(middlewares = list(), parent = "") {
      middlewares <- append(
        middlewares,
        lapply(
          private$.middleware,
          function(fn) {
            attr(fn, "basepath") <- paste0(parent, private$.basepath)
            return(fn)
          }
        )
      )

      if (!length(private$.routers)) {
        return(middlewares)
      }

      parent <- paste0(parent, private$.basepath)

      for (router in private$.routers) {
        middlewares <- router$get_middleware(middlewares, parent)
      }

      return(middlewares)
    }
  ),
  active = list(
    basepath = function(path) {
      if (!missing(path)) {
        private$.basepath <- path
        return(path)
      }

      invisible(private$.basepath)
    },
    websocket = function(ws) {
      if (missing(ws) && !is.null(private$.wss_custom)) {
        return(private$.wss_custom)
      }

      if (missing(ws) && is.null(private$.wss_custom)) {
        return(private$.wss)
      }

      private$.wss_custom <- ws
      invisible(self)
    }
  ),
  private = list(
    .basepath = "/",
    .is_router = FALSE,
    .routes = list(),
    .static = list(),
    .receivers = list(),
    .middleware = list(),
    .params = list(),
    .is_running = FALSE,
    .wss_custom = NULL,
    .routers = list(),
    # Validate a Request Against Its Route's Documentation
    #
    # Runs before the handler. Returns a response listing what is wrong when
    # the request does not match the route's documentation, and `NULL`
    # otherwise, which lets the caller treat "no response" as "carry on".
    #
    # A no-op for a route registered without `docs`, or with validation off.
    # Validation is on for every documented route once `app$openapi()` is
    # called; `openapi_docs(validate =)` overrides that per route, in either
    # direction.
    #
    # The `.openapi_*` fields live on `Ambiorix`, not here: a `Router` is
    # validated by the app it is mounted on, so they are read through `%||%`
    # fallbacks and are absent on a standalone `Router`.
    .validate_request = function(request, res, route) {
      if (is.null(route$docs) || !is_openapi_docs(route$docs)) {
        return(NULL)
      }

      # `openapi_docs(validate =)` overrides the app wide setting
      enabled <- route$docs$validate %||% private$.openapi_validate %||% FALSE

      if (!isTRUE(enabled)) {
        return(NULL)
      }

      details <- openapi_validate_request(
        request = request,
        docs = route$docs,
        schemas = private$.openapi_schemas %||% list(),
        path = paste0(route$route$basepath, route$path)
      )

      if (!length(details)) {
        return(NULL)
      }

      on_invalid <- private$.openapi_on_invalid %||% openapi_invalid_response

      on_invalid(request, res, details)
    },
    .register_http_methods = function() {
      http_methods <- list(
        get = "GET",
        put = "PUT",
        patch = "PATCH",
        delete = "DELETE",
        post = "POST",
        options = "OPTIONS",
        all = c("GET", "POST", "PUT", "DELETE", "PATCH")
      )

      Map(
        f = function(name, value) {
          self[[name]] <- function(path, handler, error = NULL, docs = NULL) {
            assert_that(valid_path(path))
            assert_that(not_missing(handler))
            assert_that(is_handler(handler))
            assert_that(is.null(docs) || is_openapi_docs(docs))

            r <- list(
              route = Route$new(private$.make_path(path)),
              path = path,
              fun = handler,
              method = value,
              error = error,
              docs = docs
            )
            private$.routes <- append(private$.routes, list(r))

            invisible(self)
          }
        },
        name = names(http_methods),
        value = unname(http_methods)
      )

      invisible(self)
    },
    .call = function(req) {
      request <- Request$new(req)
      res <- Response$new()

      # loop over routes
      for (i in seq_along(private$.routes)) {
        # if path matches pattern and method
        if (
          grepl(private$.routes[[i]]$route$pattern, req$PATH_INFO) &&
            req$REQUEST_METHOD %in% private$.routes[[i]]$method
        ) {
          .globals$infoLog$log(req$REQUEST_METHOD, "on", req$PATH_INFO)

          basepath <- private$.routes[[i]]$route$basepath

          # a `return()` in here still leaves `.call()`
          response <- tryCatch(
            {
              request$params <- set_params(
                request$PATH_INFO,
                private$.routes[[i]]$route
              )

              # middleware:
              for (j in seq_along(private$.middleware)) {
                mid_basepath <- attr(private$.middleware[[j]], "basepath")
                under_router <- identical(basepath, mid_basepath) ||
                  startsWith(basepath, paste0(mid_basepath, "/"))

                if (!under_router) {
                  next
                }

                mid_res <- private$.middleware[[j]](request, res)

                if (is_response(mid_res)) {
                  return(mid_res)
                }
              }

              # validate the request against its documentation: after the
              # middleware, so that e.g. authentication runs first
              invalid <- private$.validate_request(
                request,
                res,
                private$.routes[[i]]
              )

              if (is_response(invalid)) {
                return(invalid)
              }

              # parameter middleware
              for (j in seq_along(private$.params)) {
                pn <- private$.params[[j]]$params
                pv <- request$params[[pn]]
                mid_basepath <- attr(private$.params[[j]], "basepath")
                under_router <- identical(basepath, mid_basepath) ||
                  startsWith(basepath, paste0(mid_basepath, "/"))

                if (!under_router || is.null(pv)) {
                  next
                }

                param_res <- private$.params[[j]]$handler(
                  request,
                  res,
                  pv,
                  pn
                )

                if (is_response(param_res)) {
                  return(param_res)
                }
              }

              # get response
              private$.routes[[i]]$fun(request, res)
            },
            error = function(error) {
              error
            }
          )

          if (inherits(response, "error")) {
            return(private$.routes[[i]]$error(request, res, response))
          }

          if (inherits(x = response, what = c("promise", "Future", "mirai"))) {
            return(
              promises::then(
                response,
                onFulfilled = function(response) {
                  response %response%
                    response("Must return a response", status = 206L)
                },
                onRejected = function(error) {
                  message(conditionMessage(error))
                  .globals$errorLog$log(
                    req$REQUEST_METHOD,
                    "on",
                    req$PATH_INFO,
                    "-",
                    "Server error"
                  )

                  private$.routes[[i]]$error(request, res, error)
                }
              )
            )
          }

          if (is_forward(response)) {
            next
          }

          # if not a response return something that is
          return(
            response %response%
              response("Must return a response", status = 206L)
          )
        }
      }

      .globals$errorLog$log(
        request$REQUEST_METHOD,
        "on",
        request$PATH_INFO,
        "- Not found"
      )

      self$not_found(request, res)
    },
    .wss = function(ws) {
      .globals$wsc <- append(.globals$wsc, Websocket$new(ws))

      # receive
      ws$onMessage(function(binary, message) {
        # don't run if no receiver
        if (length(private$.receivers) == 0) {
          return(NULL)
        }

        # a text frame is a string, a binary frame is raw. the parser
        # takes raw, so a message reads the way a request body does
        if (is.character(message)) {
          message <- charToRaw(message)
        }

        message <- get_json_parser()(message)

        for (i in seq_along(private$.receivers)) {
          if (private$.receivers[[i]]$is_handler(message)) {
            .globals$infoLog$log(
              "Received message from websocket:",
              message$name
            )
            return(private$.receivers[[i]]$receive(message, ws))
          }
        }
      })
    },
    n_routes = function() {
      length(private$.routes)
    },
    .make_path = function(path) {
      paste0(private$.basepath, path)
    }
  )
)
