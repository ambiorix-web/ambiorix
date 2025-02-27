test_that("rlication", {
  library(ambiorix)

  expect_error(Router$new())

  r <- Router$new("/users")

  r$get("/", function(req, res) {
    res$send("home")
  })

  r$post("/", function(req, res) {
    res$send("home")
  })
  
  r$put("/", function(req, res) {
    res$send("home")
  })

  r$patch("/", function(req, res) {
    res$send("home")
  })

  r$delete("/", function(req, res) {
    res$send("home")
  })

  r$all("/", function(req, res) {
    res$send("home")
  })

  r$options("/", function(req, res) {
    res$send("home")
  })

  # dynamic
  r$get("/.path", function(req, res) {
    res$send("home")
  })

  r$receive("message", function(...) {
    print("received")
  })

  expect_type(r$get_routes(), "list")
  expect_length(r$get_routes(), 8L)
  expect_snapshot(r)

  app <- Ambiorix$new()
  app$use(r)
  expect_type(app$get_routes(), "list")
  expect_length(app$get_routes(), 8L)

  stop_all()
})
