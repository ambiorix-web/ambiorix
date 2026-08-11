mock_request <- function(query = list(), params = list(), body = NULL) {
  request <- mockRequest()
  request$query <- query
  request$params <- params
  request$rook.input <- list(
    read = function() {
      if (is.null(body)) {
        return(raw())
      }
      charToRaw(body)
    },
    rewind = function() invisible(NULL)
  )
  request
}

messages <- function(problems) {
  vapply(X = problems, FUN = function(p) p$message, FUN.VALUE = character(1))
}

paths <- function(problems) {
  vapply(X = problems, FUN = function(p) p$path, FUN.VALUE = character(1))
}

test_that("types are checked against the parsed JSON", {
  expect_length(
    openapi_validate(
      value = "a",
      schema = openapi_schema_string()
    ),
    0L
  )
  expect_length(
    openapi_validate(
      value = 1L,
      schema = openapi_schema_integer()
    ),
    0L
  )
  expect_length(
    openapi_validate(
      value = 1.5,
      schema = openapi_schema_number()
    ),
    0L
  )
  expect_length(
    openapi_validate(
      value = TRUE,
      schema = openapi_schema_boolean()
    ),
    0L
  )

  expect_match(
    messages(
      openapi_validate(
        value = 1L,
        schema = openapi_schema_string()
      )
    ),
    "must be a string"
  )
  expect_match(
    messages(
      openapi_validate(
        value = "a",
        schema = openapi_schema_integer()
      )
    ),
    "must be an integer"
  )
  expect_length(
    openapi_validate(
      value = 1,
      schema = openapi_schema_integer()
    ),
    0L
  )
  expect_length(
    openapi_validate(
      value = 1.5,
      schema = openapi_schema_integer()
    ),
    1L
  )
})

test_that("an array is not a scalar", {
  req <- mock_request(body = '{"tags":["a","b"],"title":"a"}')
  body <- req$parse_json()

  expect_length(
    openapi_validate(
      value = body$tags,
      schema = openapi_schema_array(
        items = openapi_schema_string()
      )
    ),
    0L
  )

  expect_length(
    openapi_validate(
      value = body$title,
      schema = openapi_schema_string()
    ),
    0L
  )

  expect_length(
    openapi_validate(
      value = body$title,
      schema = openapi_schema_array(
        items = openapi_schema_string()
      )
    ),
    1L
  )

  expect_length(
    openapi_validate(
      value = body$tags,
      schema = openapi_schema_string()
    ),
    1L
  )

  expect_length(
    openapi_validate(
      value = I("a"),
      schema = openapi_schema_array(
        items = openapi_schema_string()
      )
    ),
    0L
  )

  expect_length(
    openapi_validate(
      value = I("a"),
      schema = openapi_schema_string()
    ),
    1L
  )
})

test_that("empty arrays and objects are told apart", {
  req <- mock_request(body = '{"arr":[],"obj":{}}')
  body <- req$parse_json()

  expect_length(
    openapi_validate(
      value = body$arr,
      schema = openapi_schema_array(
        items = openapi_schema_string()
      )
    ),
    0L
  )

  expect_length(
    openapi_validate(
      value = body$obj,
      schema = openapi_schema_object()
    ),
    0L
  )

  expect_length(
    openapi_validate(
      value = body$arr,
      schema = openapi_schema_object()
    ),
    1L
  )
})

test_that("required properties are checked", {
  schema <- openapi_schema_object(
    properties = list(
      title = openapi_schema_string(),
      done = openapi_schema_boolean()
    ),
    required = "title"
  )

  req <- mock_request(body = '{"title":"a"}')
  body <- req$parse_json()

  expect_length(
    openapi_validate(
      value = body,
      schema = schema
    ),
    0L
  )

  req <- mock_request(body = "{}")
  body <- req$parse_json()
  problems <- openapi_validate(
    value = body,
    schema = schema
  )

  expect_length(problems, 1L)
  expect_equal(paths(problems), "title")
  expect_equal(messages(problems), "is required")
})

test_that("nested objects and arrays report the path of the problem", {
  schema <- openapi_schema_object(
    properties = list(
      task = openapi_schema_object(
        properties = list(
          tags = openapi_schema_array(
            items = openapi_schema_string()
          )
        )
      )
    )
  )

  req <- mock_request(body = '{"task":{"tags":["a",1,"c"]}}')
  body <- req$parse_json()
  problems <- openapi_validate(
    value = body,
    schema = schema
  )

  expect_length(problems, 1L)
  expect_equal(paths(problems), "task.tags[2]")
})

test_that("string, number, and array keywords are checked", {
  expect_match(
    messages(
      openapi_validate(
        value = "",
        schema = openapi_schema_string(minLength = 1L)
      )
    ),
    "at least 1"
  )

  expect_match(
    messages(
      openapi_validate(
        value = "abc",
        schema = openapi_schema_string(maxLength = 2L)
      )
    ),
    "at most 2"
  )

  expect_match(
    messages(
      openapi_validate(
        value = "abc",
        schema = openapi_schema_string(pattern = "^z")
      )
    ),
    "must match"
  )

  expect_match(
    messages(
      openapi_validate(
        value = 0L,
        schema = openapi_schema_integer(minimum = 1L)
      )
    ),
    "greater than or equal to 1"
  )

  expect_match(
    messages(
      openapi_validate(
        value = 5L,
        schema = openapi_schema_integer(maximum = 1L)
      )
    ),
    "less than or equal to 1"
  )

  expect_match(
    messages(
      openapi_validate(
        value = "c",
        schema = openapi_schema_string(enum = c("a", "b"))
      )
    ),
    "must be one of"
  )

  req <- mock_request(body = '{"t":["a","b","a"]}')
  tags <- req$parse_json()$t

  expect_match(
    messages(
      openapi_validate(
        value = tags,
        schema = openapi_schema_array(
          items = openapi_schema_string(),
          maxItems = 2L
        )
      )
    ),
    "at most 2"
  )

  expect_match(
    messages(
      openapi_validate(
        value = tags,
        schema = openapi_schema_array(
          items = openapi_schema_string(),
          uniqueItems = TRUE
        )
      )
    ),
    "must not contain duplicates"
  )
})

test_that("additionalProperties = FALSE rejects unknown properties", {
  schema <- openapi_schema_object(
    properties = list(
      title = openapi_schema_string()
    ),
    additionalProperties = FALSE
  )

  req <- mock_request(body = '{"title":"a","nope":1}')
  body <- req$parse_json()
  problems <- openapi_validate(
    value = body,
    schema = schema
  )

  expect_length(problems, 1L)
  expect_equal(paths(problems), "nope")
})

test_that("references are resolved against the document's schemas", {
  task <- openapi_schema_ref(
    name = "Task",
    schema = openapi_schema_object(
      properties = list(
        title = openapi_schema_string()
      ),
      required = "title"
    )
  )

  schemas <- list(Task = task)

  req <- mock_request(body = '{"title":"a"}')
  body <- req$parse_json()

  expect_length(
    openapi_validate(
      value = body,
      schema = task,
      schemas = schemas
    ),
    0L
  )

  req <- mock_request(body = "{}")
  body <- req$parse_json()
  problems <- openapi_validate(
    value = body,
    schema = openapi_schema_ref(name = "Task"),
    schemas = schemas
  )

  expect_equal(messages(problems), "is required")

  expect_length(
    openapi_validate(
      value = body,
      schema = openapi_schema_ref(name = "Nope"),
      schemas = schemas
    ),
    0L
  )
})

test_that("parameters are converted to their documented type", {
  expect_identical(
    openapi_convert(
      value = "3",
      schema = openapi_schema_integer()
    ),
    3L
  )
  expect_identical(
    openapi_convert(
      value = "3.5",
      schema = openapi_schema_number()
    ),
    3.5
  )
  expect_identical(
    openapi_convert(
      value = "true",
      schema = openapi_schema_boolean()
    ),
    TRUE
  )
  expect_identical(
    openapi_convert(
      value = "false",
      schema = openapi_schema_boolean()
    ),
    FALSE
  )
  expect_identical(
    openapi_convert(
      value = "abc",
      schema = openapi_schema_string()
    ),
    "abc"
  )

  expect_s3_class(
    openapi_convert(
      value = "abc",
      schema = openapi_schema_integer()
    ),
    "ambiorix_openapi_conversion_error"
  )
  expect_s3_class(
    openapi_convert(
      value = "maybe",
      schema = openapi_schema_boolean()
    ),
    "ambiorix_openapi_conversion_error"
  )
})

test_that("query and path parameters are validated and converted", {
  docs <- openapi_docs(
    parameters = list(
      openapi_param(
        name = "id",
        location = "path",
        schema = openapi_schema_integer()
      ),
      openapi_param(
        name = "limit",
        location = "query",
        schema = openapi_schema_integer(maximum = 10L)
      )
    )
  )

  req <- mock_request(
    query = list(limit = "5"),
    params = list(id = "42")
  )

  expect_length(
    openapi_validate_request(
      request = req,
      docs = docs
    ),
    0L
  )

  expect_identical(req$params$id, 42L)
  expect_identical(req$query$limit, 5L)

  req <- mock_request(
    query = list(),
    params = list(id = "abc")
  )
  problems <- openapi_validate_request(
    request = req,
    docs = docs
  )

  expect_equal(paths(problems), "id")
  expect_match(messages(problems), "must be an integer")

  req <- mock_request(
    query = list(limit = "50"),
    params = list(id = "1")
  )
  problems <- openapi_validate_request(
    request = req,
    docs = docs
  )

  expect_match(messages(problems), "less than or equal to 10")
})

test_that("a missing required parameter is reported", {
  docs <- openapi_docs(
    parameters = list(
      openapi_param(
        name = "limit",
        location = "query",
        required = TRUE
      )
    )
  )

  req <- mock_request()
  problems <- openapi_validate_request(
    request = req,
    docs = docs
  )

  expect_equal(paths(problems), "limit")
  expect_equal(messages(problems), "is required")

  docs <- openapi_docs(
    parameters = list(
      openapi_param(
        name = "limit",
        location = "query"
      )
    )
  )
  req <- mock_request()

  expect_length(
    openapi_validate_request(
      request = req,
      docs = docs
    ),
    0L
  )
})

test_that("the request body is validated", {
  docs <- openapi_docs(
    request_body = openapi_request_body(
      schema = openapi_schema_object(
        properties = list(
          title = openapi_schema_string(minLength = 1L)
        ),
        required = "title"
      )
    )
  )

  req <- mock_request(body = '{"title":"hello"}')
  rook_body <- req$body

  expect_length(
    openapi_validate_request(
      request = req,
      docs = docs
    ),
    0L
  )
  expect_identical(req$payload, list(title = "hello"))
  expect_identical(req$body, rook_body)

  req <- mock_request(body = "{}")
  problems <- openapi_validate_request(
    request = req,
    docs = docs
  )

  expect_match(messages(problems), "a request body is required")
  expect_true(is.list(req$payload))
  expect_length(req$payload, 0L)

  req <- mock_request(body = '{"description":"some description"}')
  problems <- openapi_validate_request(
    request = req,
    docs = docs
  )

  expect_equal(paths(problems), "title")
  expect_identical(req$payload, list(description = "some description"))
})

test_that("header and cookie parameters are not checked", {
  docs <- openapi_docs(
    parameters = list(
      openapi_param(
        name = "X-Trace",
        location = "header",
        required = TRUE
      ),
      openapi_param(
        name = "session",
        location = "cookie",
        required = TRUE
      )
    )
  )

  req <- mock_request()

  expect_length(
    openapi_validate_request(
      request = req,
      docs = docs
    ),
    0L
  )
})

test_that("validation is opt-in and can be overridden per route", {
  app <- Ambiorix$new()
  private <- environment(app$openapi)$private

  docs <- openapi_docs(
    request_body = openapi_request_body(
      schema = openapi_schema_object(
        properties = list(
          title = openapi_schema_string()
        ),
        required = "title"
      )
    )
  )

  route <- list(docs = docs)
  req <- mock_request(body = "{}")
  res <- Response$new()

  expect_null(private$.validate_request(req, res, route))

  private$.openapi_validate <- TRUE
  invalid <- private$.validate_request(req, res, route)

  expect_true(is_response(invalid))
  expect_equal(invalid$status, 400L)

  route$docs$validate <- FALSE
  expect_null(private$.validate_request(req, res, route))

  private$.openapi_validate <- FALSE
  route$docs$validate <- TRUE
  expect_true(is_response(private$.validate_request(req, res, route)))

  expect_null(private$.validate_request(req, res, list(docs = NULL)))

  stop_all()
})
