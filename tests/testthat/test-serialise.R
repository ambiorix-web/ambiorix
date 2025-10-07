test_that("serialise", {
  obj <- list(a = 1, b = "hello, world!")

  expect_equal(
    default_serialiser(obj),
    yyjsonr::write_json_str(obj, auto_unbox = TRUE)
  )

  expect_equal(
    default_serialiser(obj, auto_unbox = FALSE),
    yyjsonr::write_json_str(obj)
  )

  expect_equal(
    default_serialiser(obj, opts = list(auto_unbox = FALSE)),
    yyjsonr::write_json_str(obj)
  )
})

test_that("serialise falls back to default serialiser when option unset", {
  obj <- list(a = 1, b = "hello, world!")
  global_serialiser <- getOption("AMBIORIX_SERIALISER")
  on.exit(options(AMBIORIX_SERIALISER = global_serialiser), add = TRUE)

  options(AMBIORIX_SERIALISER = NULL)

  expect_identical(
    serialise(obj),
    default_serialiser(obj)
  )
})

test_that("serialise forwards dots to custom serialiser", {
  obj <- list(a = 1)
  global_serialiser <- getOption("AMBIORIX_SERIALISER")
  on.exit(options(AMBIORIX_SERIALISER = global_serialiser), add = TRUE)

  captured <- NULL
  my_serialiser <- function(data, ...) {
    captured <<- list(data = data, dots = list(...))
    "custom"
  }

  options(AMBIORIX_SERIALISER = my_serialiser)

  result <- serialise(obj, digits = 2, auto_unbox = FALSE)
  expect_equal(result, "custom")
  expect_equal(captured$data, obj)
  expect_equal(captured$dots, list(digits = 2, auto_unbox = FALSE))
})

test_that("default serialiser gives precedence to dots over opts", {
  obj <- list(a = 1)

  expect_equal(
    default_serialiser(
      obj,
      opts = list(auto_unbox = FALSE),
      auto_unbox = TRUE
    ),
    yyjsonr::write_json_str(
      obj,
      opts = list(auto_unbox = TRUE)
    )
  )
})

test_that("custom serialiser works", {
  global_serialiser <- getOption("AMBIORIX_SERIALISER")
  on.exit(options(AMBIORIX_SERIALISER = global_serialiser))

  my_serialiser <- function(data, ...) {
    list(
      data = data,
      serialised = jsonlite::toJSON(x = data, ...)
    )
  }

  options(AMBIORIX_SERIALISER = my_serialiser)

  obj <- list(a = 1, b = "hello, world!")

  expect_equal(
    my_serialiser(obj),
    list(
      data = obj,
      serialised = jsonlite::toJSON(obj)
    )
  )
})
