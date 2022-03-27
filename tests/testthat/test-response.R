test_that("response", {
  # errors
  expect_error(response())

  # basic response
  res <- response(charToRaw("1"))
  expect_snapshot(res)
  expect_equal(res$status, 200L)
  expect_equal(res$body, charToRaw("1"))
  expect_equal(res$headers, list())

  # 404
  res <- response_404(I("404"))
  expect_equal(res$status, 404L)
  expect_equal(res$body, I("404"))
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
  expect_true(is_response(res))
})

test_that("Response", {
  res <- Response$new()
  expect_s3_class(res, "Response")

  # htmltools
  resp <- res$send(
    htmltools::p("hello")
  )
  expect_equal(resp$body, "<p>hello</p>")
  
  # factor
  resp <- res$send(as.factor("hello"))
  expect_equal(resp$body, "hello")

  # status
  res$set_status(404L)
  expect_equal(res$status, 404L)

  res$status <- 200L
  expect_equal(res$status, 200L)

  # sendf
  resp <- res$sendf("hello %s", "world")
  expect_equal(resp$body, "hello world")

  # text
  resp <- res$text("hello")
  expect_equal(resp$body, "hello")
  expect_equal(
    resp$headers[["Content-Type"]],
    content_plain()
  )

  # file
  resp <- res$send_file("file.html")
  expect_equal(nchar(resp$body), 48)

  # redirect
  resp <- res$redirect("/")
  expect_equal(resp$headers$Location, "/")

  # render
  resp <- res$render("render.html", list(title = "hello"))
  expect_equal(
    resp$body,
    "<html><script>console.log('tests');</script>  <body>     <h1>hello</h1>  </body></html>"
  )
  resp <- res$render("render.md", list(title = "hello"))
  expect_equal(
    resp$body,
    "<script>console.log('tests');</script>\n<h1>hello</h1>"
  )
})
