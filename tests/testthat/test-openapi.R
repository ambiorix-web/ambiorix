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
    id = openapi_schema_integer(),
    name = openapi_schema_string()
  )
  expect_equal(obj$type, "object")
  expect_equal(obj$properties$id$type, "integer")
  expect_equal(obj$properties$name$type, "string")
})

test_that("openapi_schema_array/object validate their inputs", {
  expect_error(openapi_schema_array("nope"))
  expect_error(openapi_schema_object(id = "nope"))
  expect_error(openapi_schema_object(openapi_schema_string()))
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

test_that("openapi_param rejects path location", {
  expect_error(
    openapi_param("id", location = "path"),
    "Path parameters"
  )
})

test_that("openapi_docs validates its components", {
  d <- openapi_docs(
    summary = "Get a user",
    tags = "users",
    parameters = openapi_parameters(
      openapi_param("verbose", location = "query")
    ),
    responses = openapi_responses(
      openapi_response(200, "The user")
    )
  )

  expect_s3_class(d, "ambiorix_openapi_docs")
  expect_equal(d$summary, "Get a user")

  expect_error(openapi_docs(parameters = list()))
  expect_error(openapi_docs(responses = list()))
  expect_error(openapi_docs(request_body = list()))
})

test_that("openapi_path converts :param to {param}", {
  expect_equal(openapi_path("/users/:id"), "/users/{id}")
  expect_equal(
    openapi_path("/users/:id/posts/:post"),
    "/users/{id}/posts/{post}"
  )
  expect_equal(openapi_path("/users"), "/users")
})

test_that("build_openapi produces a valid document", {
  app <- Ambiorix$new()

  app$get(
    "/users/:id",
    function(req, res) res$json(list(id = req$params$id)),
    docs = openapi_docs(
      summary = "Get a user by ID",
      tags = "users",
      parameters = openapi_parameters(
        openapi_param(
          "verbose",
          location = "query",
          schema = openapi_schema_boolean()
        )
      ),
      responses = openapi_responses(
        openapi_response(200, "The user")
      )
    )
  )

  # undocumented route should be excluded
  app$get("/health", function(req, res) res$json(list(ok = TRUE)))

  app$prepare()
  routes <- app$get_routes()
  doc <- build_openapi(routes, list(title = "Test", version = "9.9.9"))

  expect_equal(doc$openapi, "3.1.0")
  expect_equal(doc$info$title, "Test")
  expect_equal(doc$info$version, "9.9.9")

  # documented path present, converted; undocumented excluded
  expect_true("/users/{id}" %in% names(doc$paths))
  expect_false("/health" %in% names(doc$paths))

  op <- doc$paths[["/users/{id}"]]$get
  expect_equal(op$summary, "Get a user by ID")

  # path param auto-derived and required
  path_param <- Filter(function(p) p$name == "id", op$parameters)[[1]]
  expect_equal(path_param$`in`, "path")
  expect_true(path_param$required)

  # query param carried over
  query_param <- Filter(function(p) p$name == "verbose", op$parameters)[[1]]
  expect_equal(query_param$`in`, "query")

  stop_all()
})

test_that("build_openapi expands all() across verbs", {
  app <- Ambiorix$new()

  app$all(
    "/thing",
    function(req, res) res$json(list(ok = TRUE)),
    docs = openapi_docs(summary = "Any verb")
  )

  app$prepare()
  routes <- app$get_routes()
  doc <- build_openapi(routes)

  item <- doc$paths[["/thing"]]
  expect_true(all(c("get", "post", "put", "delete", "patch") %in% names(item)))

  stop_all()
})

test_that("swagger_ui_html embeds the spec url", {
  html <- swagger_ui_html("/openapi.json")
  expect_true(grepl("swagger-ui", html))
  expect_true(grepl("/openapi.json", html, fixed = TRUE))
})
