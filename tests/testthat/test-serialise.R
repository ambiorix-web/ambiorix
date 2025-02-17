test_that("serialise", {
  json <- default_serialiser(list(x = 1))
  expect_equal(
    json,
    yyjsonr::write_json_str(
      list(x = 1)
    )
  )
})
