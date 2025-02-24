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
