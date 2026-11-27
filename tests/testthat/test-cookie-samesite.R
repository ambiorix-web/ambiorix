test_that("Response cookie `same_site` defaults to the option (#166)", {
  opts <- options(
    ambiorix.cookie.samesite = "Strict",
    ambiorix.cookie.savesite = NULL
  )
  on.exit(options(opts), add = TRUE)

  res <- Response$new()
  res$cookie("hello", "world")
  res$send("hello")
  expect_match(res$headers[["Set-Cookie"]], "; SameSite=Strict$")

  # the misspelled `savesite` option documented before the fix keeps working
  options(ambiorix.cookie.samesite = NULL, ambiorix.cookie.savesite = "Lax")
  res <- Response$new()
  res$cookie("hello", "world")
  res$send("hello")
  expect_match(res$headers[["Set-Cookie"]], "; SameSite=Lax$")
})
