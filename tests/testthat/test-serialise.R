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
