test_that("Route", {
  r <- Route$new(path = "/:id")$decompose()$as_pattern()

  expect_true(r$dynamic)
  expect_equal(r$pattern, "^/[[:alnum:][:space:][:punct:]]*$")
})

test_that("Route params match greedily", {
  r <- Route$new(path = "/users/:res")$decompose()$as_pattern()

  expect_true(grepl(r$pattern, "/users/1"))
  # greedy: parameters match across `/`
  expect_true(grepl(r$pattern, "/users/2/3"))
  # zero or more: empty values match too
  expect_true(grepl(r$pattern, "/users/"))
})

test_that("Route paths are regular expressions", {
  # an unescaped `.` matches any character
  r <- Route$new(path = "/file.json")$decompose()$as_pattern()
  expect_true(grepl(r$pattern, "/file.json"))
  expect_true(grepl(r$pattern, "/fileXjson"))

  # escape it to match a literal dot
  r <- Route$new(path = "/file\\.json")$decompose()$as_pattern()
  expect_true(grepl(r$pattern, "/file.json"))
  expect_false(grepl(r$pattern, "/fileXjson"))

  # raw regex for greedy matching without a parameter
  r <- Route$new(path = "/users/.+")$decompose()$as_pattern()
  expect_true(grepl(r$pattern, "/users/1"))
  expect_true(grepl(r$pattern, "/users/2/3"))
  expect_false(grepl(r$pattern, "/users/"))
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
