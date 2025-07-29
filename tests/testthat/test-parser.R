test_that("parse_json works correctly", {
  # empty body
  req <- mockRequest()
  req$rook.input <- list(read = function() raw())
  result <- parse_json(req)
  expect_equal(result, list())

  # JSON object
  json_data <- '{"name": "John", "age": 30, "active": true}'
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw(json_data))
  result <- parse_json(req)
  expected <- list(name = "John", age = 30L, active = TRUE)
  expect_equal(result, expected)

  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw('{"invalid": json}'))
  expect_error(parse_json(req))

  # JSON array
  json_array <- '[{"id": 1}, {"id": 2}]'
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw(json_array))
  result <- parse_json(req)
  expected <- data.frame(id = 1:2)
  expect_equal(result, expected)

  # empty JSON object
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw("{}"))
  result <- parse_json(req)
  expected <- list()
  names(expected) <- character()
  expect_equal(result, expected)

  # empty JSON array
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw("[]"))
  result <- parse_json(req)
  expected <- list()
  expect_equal(result, expected)

  # custom parser override
  custom_parser <- function(body, ...) {
    list(custom = "parsed", raw_length = length(body))
  }
  old_option <- getOption("AMBIORIX_JSON_PARSER")
  options(AMBIORIX_JSON_PARSER = custom_parser)

  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw('{"test": "data"}'))
  result <- parse_json(req)
  expect_equal(result$custom, "parsed")
  expect_equal(result$raw_length, nchar('{"test": "data"}'))

  # restore original option
  options(AMBIORIX_JSON_PARSER = old_option)
})

test_that("parse_form_urlencoded works correctly", {
  # empty body
  req <- mockRequest()
  req$rook.input <- list(read = function() raw())
  result <- parse_form_urlencoded(req)
  expect_equal(result, list())

  # simple form data
  form_data <- "name=John&age=30&active=true"
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw(form_data))
  result <- parse_form_urlencoded(req)
  expect_equal(result$name, "John")
  expect_equal(result$age, "30")
  expect_equal(result$active, "true")

  # malformed data
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw("invalid=form=data=here"))
  # webutils is quite tolerant, will parse first value:
  result <- parse_form_urlencoded(req)
  expected <- list(invalid = "form")
  expect_equal(result, expected)

  # URL-encoded special characters
  form_data <- "message=Hello%20World%21&email=user%40example.com"
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw(form_data))
  result <- parse_form_urlencoded(req)
  expect_equal(result$message, "Hello World!")
  expect_equal(result$email, "user@example.com")

  # empty values
  form_data <- "empty=&filled=value&another="
  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw(form_data))
  result <- parse_form_urlencoded(req)
  expect_equal(result$empty, NA_character_)
  expect_equal(result$filled, "value")
  expect_equal(result$another, NA_character_)

  # custom parser override
  custom_parser <- function(body, ...) {
    list(custom = "form_parsed", body_length = length(body))
  }
  old_option <- getOption("AMBIORIX_FORM_URLENCODED_PARSER")
  options(AMBIORIX_FORM_URLENCODED_PARSER = custom_parser)

  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw("test=data"))
  result <- parse_form_urlencoded(req)
  expect_equal(result$custom, "form_parsed")
  expect_equal(result$body_length, nchar("test=data"))

  # restore original option
  options(AMBIORIX_FORM_URLENCODED_PARSER = old_option)
})

test_that("parse_multipart works correctly", {
  # empty body
  req <- mockRequest()
  req$rook.input <- list(read = function() raw())
  req$CONTENT_TYPE <- "multipart/form-data; boundary=----WebKitFormBoundary"
  result <- parse_multipart(req)
  expect_equal(result, list())

  # simple multipart data with text fields
  boundary <- "----WebKitFormBoundary7MA4YWxkTrZu0gW"
  multipart_body <- paste0(
    "--",
    boundary,
    "\r\n",
    "Content-Disposition: form-data; name=\"username\"\r\n\r\n",
    "john_doe\r\n",
    "--",
    boundary,
    "\r\n",
    "Content-Disposition: form-data; name=\"email\"\r\n\r\n",
    "john@example.com\r\n",
    "--",
    boundary,
    "--\r\n"
  )

  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw(multipart_body))
  req$CONTENT_TYPE <- paste0("multipart/form-data; boundary=", boundary)
  result <- parse_multipart(req)

  expect_equal(result$username, "john_doe")
  expect_equal(result$email, "john@example.com")

  # multipart with file upload
  file_content <- "file content here"
  multipart_with_file <- paste0(
    "--",
    boundary,
    "\r\n",
    "Content-Disposition: form-data; name=\"document\"; filename=\"test.txt\"\r\n",
    "Content-Type: text/plain\r\n\r\n",
    file_content,
    "\r\n",
    "--",
    boundary,
    "\r\n",
    "Content-Disposition: form-data; name=\"description\"\r\n\r\n",
    "A test file\r\n",
    "--",
    boundary,
    "--\r\n"
  )

  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw(multipart_with_file))
  req$CONTENT_TYPE <- paste0("multipart/form-data; boundary=", boundary)
  result <- parse_multipart(req)

  # file should retain its structure
  expect_true("filename" %in% names(result$document))
  expect_equal(result$document$filename, "test.txt")
  expect_equal(result$document$content_type, "text/plain")
  expect_equal(rawToChar(result$document$value), file_content)

  # text field should be converted to character
  expect_equal(result$description, "A test file")

  # custom parser override
  custom_parser <- function(body, content_type, ...) {
    list(custom = "multipart_parsed", content_type = content_type)
  }
  old_option <- getOption("AMBIORIX_MULTIPART_FORM_DATA_PARSER")
  options(AMBIORIX_MULTIPART_FORM_DATA_PARSER = custom_parser)

  req <- mockRequest()
  req$rook.input <- list(read = function() charToRaw("dummy"))
  req$CONTENT_TYPE <- "multipart/form-data; boundary=test"
  result <- parse_multipart(req)
  expect_equal(result$custom, "multipart_parsed")
  expect_equal(result$content_type, "multipart/form-data; boundary=test")

  # restore original option
  options(AMBIORIX_MULTIPART_FORM_DATA_PARSER = old_option)
})

