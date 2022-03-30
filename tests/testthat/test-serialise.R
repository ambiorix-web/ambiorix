test_that("serialise", {
  json <- default_serialiser(list(x = 1))
  expect_equal(
    json,
    jsonlite::toJSON(
      list(x = 1),
      auto_unbox = TRUE    
    )
  )
})
