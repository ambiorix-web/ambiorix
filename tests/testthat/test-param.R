test_that("parameter middleware", {
  app <- Ambiorix$new()

  app$param("uid", function(req, res, value, name) {
    req$params$uid <- "loki"
  })
  # two parameters with the same callback
  app$param(c("from", "to"), function(req, res, value, name) {
    req$params[[name]] <- as.numeric(value)
  })

  app$get("/user/:uid", function(req, res) {
    res$send(req$params$uid)
  })
  app$get("/post/:from/:to", function(req, res) {
    from <- typeof(req$params$from)
    to <- typeof(req$params$to)
    res$send(paste0(from, "-", to))
  })

  expect_type(app$get_params(), "list")
  expect_length(app$get_params(), 3L)

  stop_all()
})
