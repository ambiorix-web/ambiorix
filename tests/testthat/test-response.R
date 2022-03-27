test_that("Response", {
  # errors
  expect_error(response())

  # basic response
  res <- response(charToRaw("1"))
  expect_snapshot(res)
  expect_equal(res$status, 200L)
  expect_equal(res$body, charToRaw("1"))
  expect_equal(res$headers, list())

  # 404
  res <- response_404("404")
  expect_equal(res$status, 404L)
  expect_equal(res$body, "404")
  expect_equal(
    res$headers,
    list(
      `Content-Type` = content_html()
    )
  )

  # 500
  res <- response_500("error")
  expect_equal(res$status, 500L)
  expect_equal(res$body, "error")
  expect_equal(
    res$headers,
    list(
      `Content-Type` = content_html()
    )
  )
})
