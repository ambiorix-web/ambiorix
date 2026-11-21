test_that("Route", {
  r <- Route$new(path = "/:id")$compile()

  expect_equal(r$pattern, "^/[^/]+$")
  expect_equal(r$params, "id")
})

test_that("Route params match one segment", {
  r <- Route$new(path = "/users/:res")$compile()

  expect_true(grepl(r$pattern, "/users/1"))
  expect_false(grepl(r$pattern, "/users/2/3"))
  expect_false(grepl(r$pattern, "/users/1/"))
  expect_false(grepl(r$pattern, "/users/"))
})

test_that("Route ignores a trailing slash", {
  r <- Route$new(path = "/users/:res/")$compile()
  expect_equal(r$pattern, "^/users/[^/]+$")

  # a router's `/` route
  r <- Route$new(path = "/")$compile("/orgs/:org")
  expect_equal(r$pattern, "^/orgs/[^/]+$")
  expect_equal(r$params, "org")
})

test_that("Route is compiled from the full path, parent included", {
  r <- Route$new(path = "/teams/:team/info")$compile("/orgs/:org")

  expect_equal(r$pattern, "^/orgs/[^/]+/teams/[^/]+/info$")
  expect_equal(r$params, c("org", "team"))
  expect_true(grepl(r$pattern, "/orgs/acme/teams/core/info"))

  # a static path under a dynamic parent has the parent's parameters
  r <- Route$new(path = "/info")$compile("/orgs/:org")
  expect_equal(r$params, "org")
})

test_that("Route paths are regular expressions", {
  # an unescaped `.` matches any character
  r <- Route$new(path = "/file.json")$compile()
  expect_true(grepl(r$pattern, "/file.json"))
  expect_true(grepl(r$pattern, "/fileXjson"))

  # escape it to match a literal dot
  r <- Route$new(path = "/file\\.json")$compile()
  expect_true(grepl(r$pattern, "/file.json"))
  expect_false(grepl(r$pattern, "/fileXjson"))

  # raw regex for greedy matching without a parameter
  r <- Route$new(path = "/users/.+")$compile()
  expect_true(grepl(r$pattern, "/users/1"))
  expect_true(grepl(r$pattern, "/users/2/3"))
  expect_false(grepl(r$pattern, "/users/"))
})

test_that("Route params are not duplicated on repeated compile calls", {
  r <- Route$new(path = "/users/:id")
  r$compile()
  r$compile()

  expect_equal(r$params, "id")
})

test_that("route middleware runs for its route only, in order", {
  app <- Ambiorix$new()
  steps <- character()
  step <- function(name) {
    force(name)
    function(req, res) {
      steps <<- c(steps, name)
    }
  }

  app$use(step("app"))
  app$param("id", function(req, res, value, name) {
    steps <<- c(steps, "param")
  })

  app$get(
    "/users/:id",
    function(req, res) {
      steps <<- c(steps, "handler")
      res$send("user")
    },
    middleware = list(step("first"), step("second"))
  )
  app$get("/open", function(req, res) res$send("open"))

  private <- app$.__enclos_env__$private
  private$.compile()

  expect_equal(private$.call(mockRequest(path = "/users/1")$body)$body, "user")
  expect_equal(steps, c("app", "first", "second", "param", "handler"))

  steps <- character()
  expect_equal(private$.call(mockRequest(path = "/open")$body)$body, "open")
  expect_equal(steps, "app")
})

test_that("route middleware can answer the request", {
  app <- Ambiorix$new()
  reached <- FALSE

  app$get(
    "/admin",
    function(req, res) {
      reached <<- TRUE
      res$send("welcome")
    },
    middleware = function(req, res) {
      res$status <- 401L
      res$send("unauthorised")
    }
  )

  private <- app$.__enclos_env__$private
  private$.compile()
  resp <- private$.call(mockRequest(path = "/admin")$body)

  expect_equal(resp$status, 401L)
  expect_equal(resp$body, "unauthorised")
  expect_false(reached)
})

test_that("route middleware must be a function of `req` and `res`, or a list of them", {
  app <- Ambiorix$new()
  handler <- function(req, res) res$send("ok")

  expect_error(app$get("/", handler, middleware = "nope"), "`middleware` must")
  expect_error(
    app$get("/", handler, middleware = function(req) NULL),
    "`middleware` must"
  )
  expect_error(
    app$get("/", handler, middleware = list(handler, 1L)),
    "`middleware` must"
  )
})
