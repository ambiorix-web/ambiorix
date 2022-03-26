test_that("Request cookie", {
  req <- mockRequest(
    cookie = "yummy_cookie=choco; tasty_cookie=strawberry"
  )
  expect_s3_class(req, "Request")
  expect_equal(
    req$cookie,
    list(
      yummy_cookie = "choco", 
      tasty_cookie = "strawberry"
    )
  )
})
