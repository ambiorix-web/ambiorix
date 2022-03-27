test_that("options", {
  options(ambiorix.port.force = 3000L)
  app <- Ambiorix$new()
  expect_equal(app$port, 3000L)
})
