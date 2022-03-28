test_that("Request", {
  req <- mockRequest()
  req$query <- list(x = 1)
  req$params <- list(x = 1)
  expect_snapshot(req)
})
