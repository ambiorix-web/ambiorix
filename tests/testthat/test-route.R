test_that("Route", {
  r <- Route$new(path = "/:id")$decompose()$as_pattern()

  expect_true(r$dynamic)
  expect_equal(r$pattern, "^/[^/]+$")

  # dynamic segments match a single, non-empty path segment
  expect_true(grepl(r$pattern, "/123"))
  expect_true(grepl(r$pattern, "/hello-world"))
  expect_false(grepl(r$pattern, "/"))
  expect_false(grepl(r$pattern, "/123/posts"))
})

test_that("Route params are not duplicated on repeated as_pattern calls", {
  r <- Route$new(path = "/users/:id")
  r$decompose()
  r$as_pattern()
  r$as_pattern()
  r$decompose()
  r$as_pattern()

  expect_equal(r$params, "id")
})

test_that("Route escapes regex metacharacters in static components", {
  r <- Route$new(path = "/openapi.json")$decompose()$as_pattern()

  expect_true(grepl(r$pattern, "/openapi.json"))
  expect_false(grepl(r$pattern, "/openapiXjson"))
})
