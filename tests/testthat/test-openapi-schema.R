test_that("openapi schema builders", {
  expect_s3_class(openapi_schema_string(), "ambiorix_openapi_schema")
  expect_equal(openapi_schema_string()$type, "string")
  expect_equal(openapi_schema_integer()$type, "integer")
  expect_equal(openapi_schema_number()$type, "number")
  expect_equal(openapi_schema_boolean()$type, "boolean")

  arr <- openapi_schema_array(openapi_schema_string())
  expect_equal(arr$type, "array")
  expect_equal(arr$items$type, "string")

  obj <- openapi_schema_object(
    properties = list(
      id = openapi_schema_integer(),
      name = openapi_schema_string()
    )
  )
  expect_equal(obj$type, "object")
  expect_equal(obj$properties$id$type, "integer")
  expect_equal(obj$properties$name$type, "string")

  # nested schemas keep their class: nothing is unclassed at construction
  expect_s3_class(arr$items, "ambiorix_openapi_schema")
  expect_s3_class(obj$properties$id, "ambiorix_openapi_schema")
})

test_that("`...` takes arbitrary JSON Schema keywords", {
  schema <- openapi_schema_string(
    minLength = 1L,
    format = "email",
    example = "a@b.com"
  )

  expect_equal(schema$minLength, 1L)
  expect_equal(schema$format, "email")
  expect_equal(schema$example, "a@b.com")

  # extension keywords are always allowed
  expect_silent(openapi_schema_string(`x-internal` = TRUE))

  # the primitive constructor takes any type
  expect_equal(openapi_schema(type = "null")$type, "null")
})

test_that("unknown keywords warn but are kept", {
  expect_message(
    schema <- openapi_schema_string(minLenght = 3L),
    "minLenght"
  )
  expect_equal(schema$minLenght, 3L)
})

test_that("openapi_schema_object declares required properties", {
  obj <- openapi_schema_object(
    properties = list(
      title = openapi_schema_string(),
      done = openapi_schema_boolean()
    ),
    required = "title"
  )

  expect_equal(obj$required, "title")

  # `TRUE` marks every declared property, in declaration order
  all_required <- openapi_schema_object(
    properties = list(
      title = openapi_schema_string(),
      done = openapi_schema_boolean()
    ),
    required = TRUE
  )
  expect_equal(all_required$required, c("title", "done"))

  # nothing required: the keyword is omitted altogether
  expect_null(
    openapi_schema_object(
      properties = list(title = openapi_schema_string())
    )$required
  )
  expect_null(
    openapi_schema_object(
      properties = list(title = openapi_schema_string()),
      required = FALSE
    )$required
  )
  expect_null(
    openapi_schema_object(
      properties = list(title = openapi_schema_string()),
      required = character()
    )$required
  )
})

test_that("`required` must name declared properties", {
  expect_error(
    openapi_schema_object(
      properties = list(title = openapi_schema_string()),
      required = "titel"
    ),
    "titel"
  )

  # composition keywords may introduce properties declared elsewhere,
  # so the names cannot be checked
  expect_error(
    openapi_schema_object(
      required = "title",
      allOf = list(openapi_schema_ref("Task"))
    ),
    NA
  )
})

test_that("a single required property serialises to a JSON array", {
  schema <- openapi_schema_object(
    properties = list(title = openapi_schema_string()),
    required = "title"
  )

  json <- default_serialiser(as_openapi(schema, new_openapi_ctx()))
  expect_true(grepl('"required":["title"]', json, fixed = TRUE))

  # the same holds for every array valued keyword
  json <- default_serialiser(
    as_openapi(openapi_schema_string(enum = "a"), new_openapi_ctx())
  )
  expect_true(grepl('"enum":["a"]', json, fixed = TRUE))
})

test_that("openapi_schema_array/object validate their inputs", {
  expect_error(openapi_schema_array("nope"))
  expect_error(openapi_schema_object(properties = list(id = "nope")))
  expect_error(openapi_schema_object(
    properties = list(openapi_schema_string())
  ))
  expect_error(openapi_schema_string("unnamed keyword"))
})

test_that("openapi_schema_ref names a schema", {
  schema <- openapi_schema_ref(
    "Task",
    openapi_schema_object(properties = list(id = openapi_schema_integer()))
  )

  expect_s3_class(schema, "ambiorix_openapi_schema")
  expect_equal(attr(schema, "openapi_name"), "Task")

  # the name is an attribute: it can never leak into the document
  expect_null(schema$name)

  # a bare reference holds no keywords of its own
  bare <- openapi_schema_ref("Task")
  expect_length(unclass(bare), 0L)
  expect_equal(attr(bare, "openapi_name"), "Task")

  expect_error(openapi_schema_ref("not a name!"))
  expect_error(openapi_schema_ref("Task", "nope"))
})

test_that("openapi_response accepts valid statuses and rejects others", {
  expect_equal(openapi_response(200, "OK")$status, "200")
  expect_equal(openapi_response(201L, "Created")$status, "201")
  expect_equal(openapi_response("404", "Not found")$status, "404")
  expect_equal(openapi_response("default", "Fallback")$status, "default")
  expect_equal(openapi_response("2XX", "Success")$status, "2XX")

  expect_error(openapi_response("abc", "Nope"))
  expect_error(openapi_response(99, "Nope"))
  expect_error(openapi_response(600, "Nope"))
  expect_error(openapi_response(NA, "Nope"))
})

test_that("openapi_response takes response headers", {
  response <- openapi_response(
    201,
    "Created",
    headers = list(Location = openapi_schema_string())
  )

  expect_equal(response$headers$Location$type, "string")
  expect_error(openapi_response(201, "Created", headers = list(Location = 1L)))
})

test_that("openapi_docs takes plain lists of parameters and responses", {
  docs <- openapi_docs(
    summary = "Get a user",
    tags = "users",
    parameters = list(openapi_param("verbose", location = "query")),
    responses = list(
      openapi_response(200, "The user"),
      openapi_response(404, "Not found")
    )
  )

  expect_s3_class(docs, "ambiorix_openapi_docs")
  expect_length(docs$parameters, 1L)
  expect_length(docs$responses, 2L)
})

test_that("openapi_docs accepts a single parameter or response", {
  docs <- openapi_docs(
    parameters = openapi_param("verbose", location = "query"),
    responses = openapi_response(200, "OK")
  )

  expect_length(docs$parameters, 1L)
  expect_length(docs$responses, 1L)
  expect_s3_class(docs$parameters[[1]], "ambiorix_openapi_parameter")
  expect_s3_class(docs$responses[[1]], "ambiorix_openapi_response")
})

test_that("openapi_docs reports which element is wrong", {
  expect_error(
    openapi_docs(responses = list(openapi_response(200, "OK"), "nope")),
    "element 2"
  )
  expect_error(
    openapi_docs(parameters = list("nope")),
    "element 1"
  )
})

test_that("openapi_param builds a parameter", {
  p <- openapi_param(
    "verbose",
    location = "query",
    required = TRUE,
    description = "Return extra fields",
    schema = openapi_schema_boolean()
  )

  expect_s3_class(p, "ambiorix_openapi_parameter")
  expect_equal(p$name, "verbose")
  expect_equal(p$location, "query")
  expect_true(p$required)
})

test_that("openapi_param accepts path location and forces required", {
  p <- openapi_param(
    "id",
    location = "path",
    schema = openapi_schema_integer()
  )

  expect_s3_class(p, "ambiorix_openapi_parameter")
  expect_equal(p$location, "path")
  expect_equal(p$schema$type, "integer")
  # path parameters are always required, even if not requested
  expect_true(p$required)
  expect_true(openapi_param("id", location = "path", required = FALSE)$required)
})

test_that("openapi builders validate scalar arguments", {
  expect_error(openapi_param("verbose", required = "yes"))
  expect_error(openapi_param("verbose", description = 1L))
  expect_error(openapi_request_body(openapi_schema_string(), required = "yes"))
  expect_error(openapi_docs(summary = 1L))
  expect_error(openapi_docs(tags = 1L))
  expect_error(openapi_docs(operation_id = 1L))
  expect_error(openapi_docs(deprecated = "yes"))
  expect_error(openapi_docs(validate = "yes"))
})
