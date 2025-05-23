test_that("Route", {
  r <- Route$new(path = "/:id")$decompose()$as_pattern()

  expect_true(r$dynamic)
  expect_equal(r$pattern, "^/[[:alnum:][:space:][:punct:]]*$")
})
