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
  expect_snapshot(res)

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

  # json
  resp <- res$json(list(1, 2))
  expect_equal(
    resp$body, 
    serialise(list(1,2))
  )
  expect_equal(
    resp$headers[["Content-Type"]],
    content_json()
  )

  # csv
  resp <- res$csv(cars[1, ])
  expect_equal(
    resp$body,
    "speed,dist\n4,2\n"
  )
  expect_equal(
    resp$headers[["Content-Type"]],
    content_csv()
  )

  # tsv
  resp <- res$tsv(cars[1, ])
  expect_equal(
    resp$body,
    "speed\tdist\n4\t2\n"
  )
  expect_equal(
    resp$headers[["Content-Type"]],
    content_tsv()
  )

  # md
  resp <- res$md("render.md", list(title = "hello"))
  expect_equal(
    resp$body,
    "<script>console.log('tests');</script>\n<h1>hello</h1>"
  )

  # png
  resp <- res$png("logo.png")
  response <- resp$body |> 
    head() |> 
    as.character() |> 
    paste0(collapse = "")
  expect_equal(
    response,
    "89504e470d0a"
  )

  # image
  resp <- res$image(img)
  response <- resp$body |> 
    head() |> 
    as.character() |> 
    paste0(collapse = "")
  expect_equal(
    response,
    "89504e470d0a"
  )

  # ggplot2
  plot <- ggplot2::ggplot(cars)
  resp <- res$ggplot2(plot)
  response <- resp$body |> 
    head() |> 
    as.character() |> 
    paste0(collapse = "")
  expect_equal(
    response,
    "89504e470d0a"
  )

  # header
  expect_error(res$header())
  expect_error(res$header("xxx"))
  res$header("xxx", "hello")
  expect_equal(
    res$get_header("xxx"),
    "hello"
  )

  # content type json
  res$header_content_json()
  expect_equal(
    res$get_header("Content-Type"),
    content_json()
  )

  # content type html
  res$header_content_html()
  expect_equal(
    res$get_header("Content-Type"),
    content_html()
  )

  # content type plain
  res$header_content_plain()
  expect_equal(
    res$get_header("Content-Type"),
    content_plain()
  )

  # content type csv
  res$header_content_csv()
  expect_equal(
    res$get_header("Content-Type"),
    content_csv()
  )

  # content type tvs
  res$header_content_tsv()
  expect_equal(
    res$get_header("Content-Type"),
    content_tsv()
  )

  # headers
  headers <- res$get_headers()
  expect_type(headers, "list")
  expect_snapshot(res)

  expect_error(res$headers("error"))
  expect_type(res$headers, "list")
})
