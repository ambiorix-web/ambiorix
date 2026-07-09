test_that("application", {
  library(ambiorix)

  app <- Ambiorix$new()

  app$get("/", function(req, res) {
    res$send("home")
  })

  app$post("/", function(req, res) {
    res$send("home")
  })

  app$put("/", function(req, res) {
    res$send("home")
  })

  app$patch("/", function(req, res) {
    res$send("home")
  })

  app$delete("/", function(req, res) {
    res$send("home")
  })

  app$all("/", function(req, res) {
    res$send("home")
  })

  app$options("/", function(req, res) {
    res$send("home")
  })

  # dynamic
  app$get("/.path", function(req, res) {
    res$send("home")
  })

  app$receive("message", function(...) {
    print("received")
  })

  expect_type(app$get_routes(), "list")
  expect_length(app$get_routes(), 8L)
  expect_snapshot(app)

  stop_all()
})

test_that("app$openapi enables docs and returns self", {
  app <- Ambiorix$new()

  out <- app$openapi(title = "My API", version = "2.0.0")
  expect_identical(out, app)

  expect_error(app$openapi(title = 1L))
  expect_error(app$openapi(version = 1L))

  stop_all()
})

test_that("openapi docs routes are registered on start", {
  app <- Ambiorix$new()
  app$openapi(title = "My API", version = "1.0.0")

  app$get(
    "/users/:id",
    function(req, res) res$json(list(id = req$params$id)),
    docs = openapi_docs(summary = "Get a user")
  )

  # emulate what `start()` does before launching the server
  app$prepare()
  private <- environment(app$openapi)$private
  private$.routes <- app$get_routes()
  private$.register_openapi_routes()

  paths <- vapply(
    private$.routes,
    function(route) route$path,
    character(1)
  )

  expect_true("/openapi.json" %in% paths)
  expect_true("/docs" %in% paths)

  stop_all()
})
