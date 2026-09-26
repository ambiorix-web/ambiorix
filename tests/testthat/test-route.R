test_that("Route", {
  r <- Route$new(path = "/:id")$compile()

  expect_equal(r$pattern, "^/[^/]+$")
  expect_equal(r$params, "id")
})

test_that("Route params match one segment", {
  r <- Route$new(path = "/users/:res")$compile()

  expect_true(grepl(r$pattern, "/users/1"))
  expect_false(grepl(r$pattern, "/users/2/3"))
  expect_false(grepl(r$pattern, "/users/1/"))
  expect_false(grepl(r$pattern, "/users/"))
})

test_that("Route ignores a trailing slash", {
  r <- Route$new(path = "/users/:res/")$compile()
  expect_equal(r$pattern, "^/users/[^/]+$")

  # a router's `/` route
  r <- Route$new(path = "/")$compile("/orgs/:org")
  expect_equal(r$pattern, "^/orgs/[^/]+$")
  expect_equal(r$params, "org")
})

test_that("Route is compiled from the full path, parent included", {
  r <- Route$new(path = "/teams/:team/info")$compile("/orgs/:org")

  expect_equal(r$pattern, "^/orgs/[^/]+/teams/[^/]+/info$")
  expect_equal(r$params, c("org", "team"))
  expect_true(grepl(r$pattern, "/orgs/acme/teams/core/info"))

  # a static path under a dynamic parent has the parent's parameters
  r <- Route$new(path = "/info")$compile("/orgs/:org")
  expect_equal(r$params, "org")
})

test_that("Route paths are regular expressions", {
  # an unescaped `.` matches any character
  r <- Route$new(path = "/file.json")$compile()
  expect_true(grepl(r$pattern, "/file.json"))
  expect_true(grepl(r$pattern, "/fileXjson"))

  # escape it to match a literal dot
  r <- Route$new(path = "/file\\.json")$compile()
  expect_true(grepl(r$pattern, "/file.json"))
  expect_false(grepl(r$pattern, "/fileXjson"))

  # raw regex for greedy matching without a parameter
  r <- Route$new(path = "/users/.+")$compile()
  expect_true(grepl(r$pattern, "/users/1"))
  expect_true(grepl(r$pattern, "/users/2/3"))
  expect_false(grepl(r$pattern, "/users/"))
})

test_that("Route params are not duplicated on repeated compile calls", {
  r <- Route$new(path = "/users/:id")
  r$compile()
  r$compile()

  expect_equal(r$params, "id")
})
