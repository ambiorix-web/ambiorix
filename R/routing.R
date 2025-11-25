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
#' @param path String. Route to listen to; use `:` to define a parameter (e.g.
#' `"/hello/:name"`).
#' @param handler Function that accepts the request and response objects and
#' returns an httpuv response (e.g. [response()]). Handlers can return the result
#' of helper functions such as `Response$text()`, `Response$json()`, or the
#' output of any renderer.
#' @param error Optional handler invoked if the route raises an error; receives
#' the request, response, and the error condition.
#'
#' @return The routing object invisibly so calls can be chained.
#'
#' @examples
#' app <- Ambiorix$new()
#'
#' app$get("/", function(req, res) {
#'   res$text("Hello, world!")
#' })
#'
#' app$post("/echo", function(req, res) {
#'   res$json(list(received = req$body))
#' })
#'
#' app$all("/health", function(req, res) {
#'   res$json(list(status = "ok"))
#' })
#'
#' @seealso [`Routing`]
#'
#' @name routing-http-methods
NULL

#' Core Routing Class
#'
#' Core routing class.
#' Do not use directly, see [Ambiorix], and [Router].
#'
#' @field error Error handler.
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
    #' @param path Prefix path.
    initialize = function(path = "") {
      private$.basepath <- path
      private$.is_router <- path != ""
      private$.register_http_methods()
    },
    #' @details PARAM Method
    #'
    #' @param name Name of the parameter
    #' @param handler Function that accepts the request, response, parameter value and the parameter name.
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
    #' @param engine Engine function.
    engine = function(engine) {
      if (!is_renderer_obj(engine)) {
        engine <- as_renderer(engine)
      }

      self$use(engine)
      invisible(self)
    },
    #' @details Use a router or middleware
    #' @param use Either a router as returned by [Router], a function to use as middleware,
    #' or a `list` of functions.
    #' If a function is passed, it must accept two arguments (the request, and the response):
    #' this function will be executed every time the server receives a request.
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
    #' @details Get the routes
    #' @param routes Existing list of routes.
    #' @param parent Parent path.
    get_routes = function(routes = list(), parent = "") {
      routes <- append(
        routes,
        lapply(
          private$.routes,
          function(route) {
            route$route$as_pattern(parent)
            route$route$decompose(parent)
            route$route$basepath <- private$.basepath
            route
          }
        )
      )

      if (!length(private$.routers)) {
        return(routes)
      }

      parent <- paste0(parent, private$.basepath)

      for (router in private$.routers) {
        routes <- router$get_routes(routes, parent)
      }

      return(routes)
    },
    #' @details Get the parameter middlewares
    #' @param params Existing list of parameter middlewares.
    #' @param parent Parent path.
    get_params = function(params = list(), parent = "") {
      params <- append(
        params,
        lapply(
          private$.params,
          function(fn) {
            attr(fn, "basepath") <- private$.basepath
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
    #' @param receivers Existing list of receivers
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
    #' @param middlewares Existing list of middleswares
    #' @param parent Parent path
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
    },
    #' @details Prepare routes and decomposes paths
    prepare = function() {
      for (route in private$.routes) {
        route$route$as_pattern()
        route$route$decompose()
      }

      private$reorder_routes()
      if (!length(private$.routers)) {
        return()
      }

      for (route in private$.routers) {
        route$prepare()
      }
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
    .http_methods = list(
      get = "GET",
      put = "PUT",
      patch = "PATCH",
      delete = "DELETE",
      post = "POST",
      options = "OPTIONS",
      all = c("GET", "POST", "PUT", "DELETE", "PATCH")
    ),
    .register_http_methods = function() {
      for (name in names(private$.http_methods)) {
        self[[name]] <- private$.route_adder_factory(name)
      }

      invisible(self)
    },
    .route_adder_factory = function(name) {
      http_methods <- private$.http_methods[[name]]
      force(http_methods)

      function(path, handler, error = NULL) {
        assert_that(valid_path(path))
        assert_that(not_missing(handler))
        assert_that(is_handler(handler))

        r <- list(
          route = Route$new(private$.make_path(path)),
          path = path,
          fun = handler,
          method = http_methods,
          error = error %error% self$error
        )
        private$.routes <- append(private$.routes, list(r))

        invisible(self)
      }
    },
    # we reorder the routes before launching the app
    # we make sure the longest patterns are checked first
    # this makes sure /:id/x matches BEFORE /:id does
    # however we also want to try to match exact paths
    # BEFORE dynamic ones
    # e.g. /hello should be matched before /:id
    reorder_routes = function() {
      if (!length(private$.routes)) {
        return()
      }

      indices <- seq_along(private$.routes)
      paths <- lapply(private$.routes, function(route) {
        data.frame(
          nchar = nchar(route$route$path),
          dynamic = route$route$dynamic
        )
      })
      df <- do.call(rbind, paths)
      df$order <- seq_len(nrow(df))
      df <- df[order(df$dynamic, -df$nchar), ]

      private$.routes <- private$.routes[df$order]
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

          # parse request
          request$params <- tryCatch(
            set_params(request$PATH_INFO, private$.routes[[i]]$route),
            error = function(error) {
              error
            }
          )

          if (inherits(request$params, "error")) {
            return(private$.routes[[i]]$error(request, res, request$params))
          }

          # parameter middleware

          if (length(private$.params) > 0L && length(request$params) > 0L) {
            for (j in seq_along(private$.params)) {
              param_res <- NULL
              pn <- private$.params[[j]]$params
              pv <- request$params[[pn]]

              # if param middleware is on correct router and has a handler for
              # a request parameter.

              if (
                identical(attr(private$.params[[j]], "basepath"), basepath) &&
                  !is.null(pv)
              ) {
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
            }
          }

          # Middleware

          if (length(private$.middleware) > 0L) {
            for (j in seq_along(private$.middleware)) {
              mid_basepath <- attr(private$.middleware[[j]], "basepath")

              mid_res <- NULL
              if (grepl(mid_basepath, req$PATH_INFO)) {
                mid_res <- private$.middleware[[j]](request, res)
              }

              if (is_response(mid_res)) {
                return(mid_res)
              }
            }
          }

          # get response
          response <- tryCatch(
            private$.routes[[i]]$fun(request, res),
            error = function(error) {
              error
            }
          )

          if (
            inherits(response, "error") && !is.null(private$.routes[[i]]$error)
          ) {
            return(private$.routes[[i]]$error(request, res, response))
          }

          if (inherits(response, "error") && !is.null(self$error)) {
            return(self$error(request, res, response))
          }

          if (
            inherits(x = response, what = c("promise", "Future")) ||
              inherits(x = response, what = "mirai")
          ) {
            return(
              promises::then(
                response,
                onFulfilled = function(response) {
                  return(
                    response %response%
                      response("Must return a response", status = 206L)
                  )
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

      # return 404
      request$params <- set_params(
        request$PATH_INFO,
        Route$new(request$PATH_INFO)
      )
      return(self$not_found(request, res))
    },
    .wss = function(ws) {
      .globals$wsc <- append(.globals$wsc, Websocket$new(ws))

      # receive
      ws$onMessage(function(binary, message) {
        # don't run if no receiver
        if (length(private$.receivers) == 0) {
          return(NULL)
        }

        message <- yyjsonr::read_json_str(message)

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
