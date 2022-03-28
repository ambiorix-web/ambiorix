test_that("application", {
  library(ambiorix)

  app <- Ambiorix$new()

  app$get("/", \(req, res) {
    res$send("home")
  })

  app$post("/", \(req, res) {
    res$send("home")
  })
  
  app$put("/", \(req, res) {
    res$send("home")
  })

  app$patch("/", \(req, res) {
    res$send("home")
  })

  app$delete("/", \(req, res) {
    res$send("home")
  })

  app$all("/", \(req, res) {
    res$send("home")
  })

  app$options("/", \(req, res) {
    res$send("home")
  })

  # dynamic
  app$get("/.path", \(req, res) {
    res$send("home")
  })

  app$receive("message", \(...) {
    print("received")
  })

  expect_type(app$get_routes(), "list")
  expect_length(app$get_routes(), 8L)
  expect_snapshot(app)

  stop_all()
})
