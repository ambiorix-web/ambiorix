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
