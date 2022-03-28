test_that("rlication", {
  library(ambiorix)

  expect_error(Router$new())

  r <- Router$new("/users")

  r$get("/", \(req, res) {
    res$send("home")
  })

  r$post("/", \(req, res) {
    res$send("home")
  })
  
  r$put("/", \(req, res) {
    res$send("home")
  })

  r$patch("/", \(req, res) {
    res$send("home")
  })

  r$delete("/", \(req, res) {
    res$send("home")
  })

  r$all("/", \(req, res) {
    res$send("home")
  })

  r$options("/", \(req, res) {
    res$send("home")
  })

  # dynamic
  r$get("/.path", \(req, res) {
    res$send("home")
  })

  r$receive("message", \(...) {
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
