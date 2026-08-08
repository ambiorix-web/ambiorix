parse_body <- function(json) {
  yyjsonr::read_json_str(json, opts = openapi_parse_opts())
}

messages <- function(problems) {
  vapply(X = problems, FUN = function(p) p$message, FUN.VALUE = character(1))
}

paths <- function(problems) {
  vapply(X = problems, FUN = function(p) p$path, FUN.VALUE = character(1))
}

test_that("types are checked against the parsed JSON", {
  expect_length(openapi_validate("a", openapi_schema_string()), 0L)
  expect_length(openapi_validate(1L, openapi_schema_integer()), 0L)
  expect_length(openapi_validate(1.5, openapi_schema_number()), 0L)
  expect_length(openapi_validate(TRUE, openapi_schema_boolean()), 0L)

  expect_match(
    messages(openapi_validate(1L, openapi_schema_string())),
    "must be a string"
  )
  expect_match(
    messages(openapi_validate("a", openapi_schema_integer())),
    "must be an integer"
  )
  # JSON has no integer type: 1.0 is an integer, 1.5 is not
  expect_length(openapi_validate(1, openapi_schema_integer()), 0L)
  expect_length(openapi_validate(1.5, openapi_schema_integer()), 1L)
})

test_that("a one element array is not a scalar", {
  body <- parse_body('{"tags":["a"],"title":"a"}')

  expect_length(
    openapi_validate(body$tags, openapi_schema_array(openapi_schema_string())),
    0L
  )
  expect_length(
    openapi_validate(body$title, openapi_schema_string()),
    0L
  )

  # and neither may stand in for the other
  expect_length(
    openapi_validate(body$title, openapi_schema_array(openapi_schema_string())),
    1L
  )
  expect_length(openapi_validate(body$tags, openapi_schema_string()), 1L)
})

test_that("empty arrays and objects are told apart", {
  body <- parse_body('{"arr":[],"obj":{}}')

  expect_length(
    openapi_validate(body$arr, openapi_schema_array(openapi_schema_string())),
    0L
  )
  expect_length(openapi_validate(body$obj, openapi_schema_object()), 0L)
  expect_length(openapi_validate(body$arr, openapi_schema_object()), 1L)
})

test_that("required properties are checked", {
  schema <- openapi_schema_object(
    properties = list(
      title = openapi_schema_string(),
      done = openapi_schema_boolean()
    ),
    required = "title"
  )

  expect_length(openapi_validate(parse_body('{"title":"a"}'), schema), 0L)

  problems <- openapi_validate(parse_body("{}"), schema)
  expect_length(problems, 1L)
  expect_equal(paths(problems), "title")
  expect_equal(messages(problems), "is required")
})

test_that("nested objects and arrays report the path of the problem", {
  schema <- openapi_schema_object(
    properties = list(
      task = openapi_schema_object(
        properties = list(
          tags = openapi_schema_array(openapi_schema_string())
        )
      )
    )
  )

  problems <- openapi_validate(
    parse_body('{"task":{"tags":["a",1,"c"]}}'),
    schema
  )

  expect_length(problems, 1L)
  expect_equal(paths(problems), "task.tags[2]")
})

test_that("string, number, and array keywords are checked", {
  expect_match(
    messages(openapi_validate("", openapi_schema_string(minLength = 1L))),
    "at least 1"
  )
  expect_match(
    messages(openapi_validate("abc", openapi_schema_string(maxLength = 2L))),
    "at most 2"
  )
  expect_match(
    messages(openapi_validate("abc", openapi_schema_string(pattern = "^z"))),
    "must match"
  )
  expect_match(
    messages(openapi_validate(0L, openapi_schema_integer(minimum = 1L))),
    "greater than or equal to 1"
  )
  expect_match(
    messages(openapi_validate(5L, openapi_schema_integer(maximum = 1L))),
    "less than or equal to 1"
  )
  expect_match(
    messages(openapi_validate("c", openapi_schema_string(enum = c("a", "b")))),
    "must be one of"
  )

  tags <- parse_body('{"t":["a","b","a"]}')$t
  expect_match(
    messages(
      openapi_validate(
        tags,
        openapi_schema_array(openapi_schema_string(), maxItems = 2L)
      )
    ),
    "at most 2"
  )
  expect_match(
    messages(
      openapi_validate(
        tags,
        openapi_schema_array(openapi_schema_string(), uniqueItems = TRUE)
      )
    ),
    "must not contain duplicates"
  )
})

test_that("additionalProperties = FALSE rejects unknown properties", {
  schema <- openapi_schema_object(
    properties = list(title = openapi_schema_string()),
    additionalProperties = FALSE
  )

  problems <- openapi_validate(parse_body('{"title":"a","nope":1}'), schema)
  expect_length(problems, 1L)
  expect_equal(paths(problems), "nope")
})

test_that("references are resolved against the document's schemas", {
  task <- openapi_schema_ref(
    "Task",
    openapi_schema_object(
      properties = list(title = openapi_schema_string()),
      required = "title"
    )
  )

  schemas <- list(Task = task)

  expect_length(
    openapi_validate(parse_body('{"title":"a"}'), task, schemas),
    0L
  )

  # a bare reference resolves to the named schema
  problems <- openapi_validate(
    parse_body("{}"),
    openapi_schema_ref("Task"),
    schemas
  )
  expect_equal(messages(problems), "is required")

  # an unresolvable reference is not checked
  expect_length(
    openapi_validate(parse_body("{}"), openapi_schema_ref("Nope"), schemas),
    0L
  )
})

test_that("parameters are converted to their documented type", {
  expect_identical(openapi_convert("3", openapi_schema_integer()), 3L)
  expect_identical(openapi_convert("3.5", openapi_schema_number()), 3.5)
  expect_identical(openapi_convert("true", openapi_schema_boolean()), TRUE)
  expect_identical(openapi_convert("false", openapi_schema_boolean()), FALSE)
  expect_identical(openapi_convert("abc", openapi_schema_string()), "abc")

  expect_s3_class(
    openapi_convert("abc", openapi_schema_integer()),
    "ambiorix_openapi_conversion_error"
  )
  expect_s3_class(
    openapi_convert("maybe", openapi_schema_boolean()),
    "ambiorix_openapi_conversion_error"
  )
})

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

test_that("query and path parameters are validated and converted", {
  docs <- openapi_docs(
    parameters = list(
      openapi_param("id", location = "path", schema = openapi_schema_integer()),
      openapi_param(
        "limit",
        location = "query",
        schema = openapi_schema_integer(maximum = 10L)
      )
    )
  )

  request <- mock_request(query = list(limit = "5"), params = list(id = "42"))
  expect_length(openapi_validate_request(request, docs), 0L)

  # the converted values are written back onto the request
  expect_identical(request$params$id, 42L)
  expect_identical(request$query$limit, 5L)

  request <- mock_request(query = list(), params = list(id = "abc"))
  problems <- openapi_validate_request(request, docs)
  expect_equal(paths(problems), "id")
  expect_match(messages(problems), "must be an integer")

  # keywords are checked after the conversion
  request <- mock_request(query = list(limit = "50"), params = list(id = "1"))
  problems <- openapi_validate_request(request, docs)
  expect_match(messages(problems), "less than or equal to 10")
})

test_that("a missing required parameter is reported", {
  docs <- openapi_docs(
    parameters = list(
      openapi_param("limit", location = "query", required = TRUE)
    )
  )

  problems <- openapi_validate_request(mock_request(), docs)
  expect_equal(paths(problems), "limit")
  expect_equal(messages(problems), "is required")

  # optional parameters may be absent
  docs <- openapi_docs(
    parameters = list(openapi_param("limit", location = "query"))
  )
  expect_length(openapi_validate_request(mock_request(), docs), 0L)
})

test_that("the request body is validated", {
  docs <- openapi_docs(
    request_body = openapi_request_body(
      openapi_schema_object(
        properties = list(title = openapi_schema_string(minLength = 1L)),
        required = "title"
      )
    )
  )

  request <- mock_request(body = '{"title":"hello"}')
  expect_length(openapi_validate_request(request, docs), 0L)

  request <- mock_request(body = "{}")
  problems <- openapi_validate_request(request, docs)
  expect_equal(paths(problems), "title")

  # a required body that is absent altogether
  problems <- openapi_validate_request(mock_request(), docs)
  expect_match(messages(problems), "a request body is required")
})

test_that("header and cookie parameters are not checked", {
  docs <- openapi_docs(
    parameters = list(
      openapi_param("X-Trace", location = "header", required = TRUE),
      openapi_param("session", location = "cookie", required = TRUE)
    )
  )

  expect_length(openapi_validate_request(mock_request(), docs), 0L)
})

test_that("validation is opt-in and can be overridden per route", {
  app <- Ambiorix$new()
  private <- environment(app$openapi)$private

  docs <- openapi_docs(
    request_body = openapi_request_body(
      openapi_schema_object(
        properties = list(title = openapi_schema_string()),
        required = "title"
      )
    )
  )

  route <- list(docs = docs)
  request <- mock_request(body = "{}")
  res <- Response$new()

  # off by default
  expect_null(private$.validate_request(request, res, route))

  private$.openapi_validate <- TRUE
  invalid <- private$.validate_request(request, res, route)
  expect_true(is_response(invalid))
  expect_equal(invalid$status, 400L)

  # the route may opt out
  route$docs$validate <- FALSE
  expect_null(private$.validate_request(request, res, route))

  # ... and opt in when the app has not
  private$.openapi_validate <- FALSE
  route$docs$validate <- TRUE
  expect_true(is_response(private$.validate_request(request, res, route)))

  # undocumented routes are never validated
  expect_null(private$.validate_request(request, res, list(docs = NULL)))

  stop_all()
})
