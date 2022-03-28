test_that("Request", {
  req <- mockRequest()
  req$query <- list(x = 1)
  req$params <- list(x = 1)
  expect_snapshot(req)

  req <- mockRequest(
    query = "?x=1&y=2",
    path = "/hello"
  )

  expect_length(req$query, 2L)
})
