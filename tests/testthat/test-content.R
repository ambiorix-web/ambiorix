test_that("content", {
  expect_equal(
    content_csv(),
    "text/csv"
  )

  expect_equal(
    content_tsv(),
    "tab-separated-values"
  )

  expect_equal(
    content_html(),
    "text/html"
  )

  expect_equal(
    content_plain(),
    "text/plain"
  )

  expect_equal(
    content_json(),
    "application/json"
  )

  expect_equal(
    content_protobuf(),
    "application/x-protobuf"
  )
})
