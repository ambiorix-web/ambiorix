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

  expect_error(req$set())
  expect_error(req$set("hello"))
  expect_error(req$set("hello", "world"))

  expect_error(req$get())
  expect_error(req$get("hello"))

  expect_error(req$get_header())
})
