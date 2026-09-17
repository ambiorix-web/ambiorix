#' Mirror What start() Does
#'
#' @param app Ambiorix app instance /// Required.
#'
#' @return `NULL` (invisibly).
#'
#' @keywords internal
#' @noRd
prepare_app <- function(app) {
  private <- app$.__enclos_env__$private

  app$prepare()
  private$.routes <- app$get_routes()
  private$.middleware <- app$get_middleware()
  private$.params <- app$get_params()
  invisible(NULL)
}

#' Process Mock Request
#'
#' @param app Ambiorix app instance /// Required.
#'
#' @param path String /// Optional.
#'             Route path. Defaults to "/".
#'
#' @param query String /// Optional.
#'              Query string. Defaults to "".
#'
#' @return [Response()]
#'
#' @keywords internal
#' @noRd
call_app <- function(app, path = "/", query = "") {
  req <- mockRequest(path = path, query = query)$body
  app$.__enclos_env__$private$.call(req)
}

test_that("default global error handler returns 500 on route error", {
  app <- Ambiorix$new()

  app$get("/", function(req, res) {
    stop("boom")
  })

  prepare_app(app)
  resp <- call_app(app)

  expect_equal(resp$status, 500L)

  stop_all()
})

test_that("global error handler set before routes is used on error", {
  app <- Ambiorix$new()

  app$error <- function(req, res, error) {
    res$status <- 503L
    res$send("custom before")
  }

  app$get("/", function(req, res) {
    stop("boom")
  })

  prepare_app(app)
  resp <- call_app(app)

  expect_equal(resp$status, 503L)
  expect_equal(resp$body, "custom before")

  stop_all()
})

test_that("global error handler set after routes is used on error", {
  app <- Ambiorix$new()

  app$get("/", function(req, res) {
    stop("boom")
  })

  app$error <- function(req, res, error) {
    res$status <- 503L
    res$send("custom after")
  }

  prepare_app(app)
  resp <- call_app(app)

  expect_equal(resp$status, 503L)
  expect_equal(resp$body, "custom after")

  stop_all()
})

test_that("route-specific error handler takes priority over global handler", {
  app <- Ambiorix$new()

  app$error <- function(req, res, error) {
    res$status <- 500L
    res$send("global handler")
  }

  app$get(
    "/",
    function(req, res) {
      stop("boom")
    },
    error = function(req, res, error) {
      res$status <- 422L
      res$send("route handler")
    }
  )

  prepare_app(app)
  resp <- call_app(app)

  expect_equal(resp$status, 422L)
  expect_equal(resp$body, "route handler")

  stop_all()
})

test_that("route-specific handler does not bleed into other routes", {
  app <- Ambiorix$new()

  app$error <- function(req, res, error) {
    res$status <- 500L
    res$send("global handler")
  }

  app$get(
    "/specific",
    function(req, res) {
      stop("boom")
    },
    error = function(req, res, error) {
      res$status <- 422L
      res$send("route handler")
    }
  )

  app$get("/other", function(req, res) {
    stop("boom")
  })

  prepare_app(app)

  resp_specific <- call_app(app, "/specific")
  resp_other <- call_app(app, "/other")

  expect_equal(resp_specific$status, 422L)
  expect_equal(resp_specific$body, "route handler")

  expect_equal(resp_other$status, 500L)
  expect_equal(resp_other$body, "global handler")

  stop_all()
})

test_that("error handler receives the actual error condition", {
  app <- Ambiorix$new()

  captured_error <- NULL
  app$error <- function(req, res, error) {
    captured_error <<- error
    res$status <- 500L
    res$send("error caught")
  }

  app$get("/", function(req, res) {
    stop("specific message")
  })

  prepare_app(app)
  call_app(app)

  expect_true(inherits(captured_error, "error"))
  expect_equal(conditionMessage(captured_error), "specific message")

  stop_all()
})

test_that("an error in a middleware is answered by the error handler", {
  app <- Ambiorix$new()

  captured_error <- NULL
  app$error <- function(req, res, error) {
    captured_error <<- error
    res$status <- 503L
    res$send("global handler")
  }

  app$use(function(req, res) {
    stop("middleware bug")
  })

  app$get("/", function(req, res) {
    res$send("never reached")
  })

  prepare_app(app)
  resp <- call_app(app)

  expect_equal(resp$status, 503L)
  expect_equal(resp$body, "global handler")
  expect_equal(conditionMessage(captured_error), "middleware bug")

  stop_all()
})

test_that("an error in a parameter middleware is answered by the error handler", {
  app <- Ambiorix$new()

  app$error <- function(req, res, error) {
    res$status <- 503L
    res$send(conditionMessage(error))
  }

  app$param("id", function(req, res, value, name) {
    stop("param middleware bug")
  })

  app$get("/items/:id", function(req, res) {
    res$send("never reached")
  })

  prepare_app(app)
  resp <- call_app(app, "/items/1")

  expect_equal(resp$status, 503L)
  expect_equal(resp$body, "param middleware bug")

  stop_all()
})

test_that("an error in `on_invalid` is answered by the error handler", {
  app <- Ambiorix$new()

  app$error <- function(req, res, error) {
    res$status <- 503L
    res$send(conditionMessage(error))
  }

  app$openapi(
    on_invalid = function(req, res, details) {
      stop("on_invalid bug")
    }
  )

  app$get(
    "/items",
    function(req, res) {
      res$send("never reached")
    },
    docs = openapi_docs(
      parameters = openapi_param(
        name = "n",
        schema = openapi_schema_integer()
      )
    )
  )

  prepare_app(app)

  # a valid request never meets `on_invalid`
  resp <- call_app(app, "/items", query = "?n=1")
  expect_equal(resp$body, "never reached")

  resp <- call_app(app, "/items", query = "?n=abc")
  expect_equal(resp$status, 503L)
  expect_equal(resp$body, "on_invalid bug")

  stop_all()
})

test_that("a route's own error handler answers for its middleware too", {
  app <- Ambiorix$new()

  app$error <- function(req, res, error) {
    res$status <- 500L
    res$send("global handler")
  }

  app$use(function(req, res) {
    stop("middleware bug")
  })

  app$get(
    "/",
    function(req, res) {
      res$send("never reached")
    },
    error = function(req, res, error) {
      res$status <- 422L
      res$send("route handler")
    }
  )

  prepare_app(app)
  resp <- call_app(app)

  expect_equal(resp$status, 422L)
  expect_equal(resp$body, "route handler")

  stop_all()
})

test_that("a stage that answers early still does, and forward() still forwards", {
  app <- Ambiorix$new()

  app$use(function(req, res) {
    if (identical(req$query$auth, "no")) {
      res$status <- 401L
      return(res$send("middleware answered"))
    }
  })

  app$get("/", function(req, res) {
    forward()
  })

  app$get("/", function(req, res) {
    res$send("second handler")
  })

  prepare_app(app)

  resp <- call_app(app, query = "?auth=no")
  expect_equal(resp$status, 401L)
  expect_equal(resp$body, "middleware answered")

  resp <- call_app(app)
  expect_equal(resp$body, "second handler")

  stop_all()
})
