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
  fn <- function(req) {
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

  # expires date
  res <- Response$new()
  res$cookie("hello", "world", expires = as.Date("2022-01-01"))
  resp <- res$send("hello")
  expect_equal(
    res$headers[["Set-Cookie"]],
    "hello=world; Expires=Sat, 01 Jan 2022 00:00:00 GMT; Path=/; Secure; HttpOnly"
  )

  # expires character
  res <- Response$new()
  res$cookie("hello", "world", expires = "2022-01-01")
  resp <- res$send("hello")
  expect_equal(
    res$headers[["Set-Cookie"]],
    "hello=world; Expires=2022-01-01; Path=/; Secure; HttpOnly"
  )

  # preprocessor
  .fn <- function(name, value, ...){
    sprintf("prefix.%s", value)
  }
  prep <- as_cookie_preprocessor(.fn)

  app <- Ambiorix$new()
  app$use(prep)
  res <- Response$new()
  res$cookie("hello", "world")
  resp <- res$send("hello")
  expect_equal(
    res$headers[["Set-Cookie"]],
    "hello=prefix.world; Path=/; Secure; HttpOnly"
  )

  # clear
  res$cookie("world", "hello")
  res$clear_cookie("world")
  resp <- res$send("hello")
  expect_equal(
    res$headers[["Set-Cookie"]],
    "hello=prefix.world; Path=/; Secure; HttpOnly"
  )
})
