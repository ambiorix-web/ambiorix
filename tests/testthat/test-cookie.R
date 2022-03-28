test_that("Request cookie", {
  # cookie set
  req <- mockRequest(
    cookie = "yummy_cookie=choco; tasty_cookie=strawberry"
  )
  expect_s3_class(req, "Request")
  expect_length(req$cookie, 2L)
  expect_equal(
    req$cookie,
    list(
      yummy_cookie = "choco", 
      tasty_cookie = "strawberry"
    )
  )

  # empty coookie
  req <- mockRequest(cookie = "")
  expect_length(req$cookie, 0L)
  expect_type(req$cookie, "list")
  expect_equal(req$cookie, list())

  # incorrect cookie
  req <- mockRequest(
    cookie = "yummy_cookie=choco; tasty_cookie="
  )
  expect_length(req$cookie, 1L)
  expect_type(req$cookie, "list")
  expect_equal(
    req$cookie, 
    list(
      yummy_cookie = "choco"
    )
  )

  # parser
  fn <- \(req) {
    return(req$HTTP_COOKIE)
  }
  expect_false(is_cookie_parser(fn))
  parser <- as_cookie_parser(fn)
  expect_true(is_cookie_parser(parser))
  expect_snapshot(parser)
  app <- Ambiorix$new()
  app$use(parser)
  req <- mockRequest(
    cookie = "yummy_cookie=choco;"
  )
  expect_type(req$cookie, "character")
  expect_equal(req$cookie, "yummy_cookie=choco;")

  # object
  cook <- cookie("hello", "world")
  expect_snapshot(cook)
  expect_s3_class(cook, "cookie")
})
