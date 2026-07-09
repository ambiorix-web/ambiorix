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

test_that("swagger_ui_html embeds the spec url and title", {
  html <- swagger_ui_html("/openapi.json")
  expect_true(grepl("swagger-ui", html))
  expect_true(grepl("/openapi.json", html, fixed = TRUE))
  expect_true(grepl("<title>API Documentation</title>", html, fixed = TRUE))

  html <- swagger_ui_html("/openapi.json", title = "My <API>")
  expect_true(grepl("<title>My &lt;API&gt;</title>", html, fixed = TRUE))
})

test_that("empty paths and properties serialise to JSON objects", {
  # no documented routes: `paths` must serialise to {}
  doc <- build_openapi(list())
  json <- default_serialiser(doc)
  expect_true(grepl('"paths":{}', json, fixed = TRUE))

  # schema object without properties: `properties` must serialise to {}
  schema <- openapi_schema_object()
  json <- default_serialiser(unclass(schema))
  expect_true(grepl('"properties":{}', json, fixed = TRUE))
})

test_that("build_openapi handles nested routers", {
  inner <- Router$new("/v1")
  inner$get(
    "/users/:id",
    function(req, res) res$json(list()),
    docs = openapi_docs(summary = "Get a user")
  )

  outer <- Router$new("/api")
  outer$use(inner)

  app <- Ambiorix$new()
  app$use(outer)

  app$prepare()
  routes <- app$get_routes()
  doc <- build_openapi(routes)

  expect_true("/api/v1/users/{id}" %in% names(doc$paths))

  stop_all()
})

test_that("path params are not duplicated across restarts", {
  app <- Ambiorix$new()

  app$get(
    "/users/:id",
    function(req, res) res$json(list()),
    docs = openapi_docs(summary = "Get a user")
  )

  # simulate two consecutive `start()` calls
  app$prepare()
  app$get_routes()
  app$prepare()
  routes <- app$get_routes()

  doc <- build_openapi(routes)
  params <- doc$paths[["/users/{id}"]]$get$parameters
  expect_length(params, 1L)

  stop_all()
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

test_that("openapi builders validate scalar arguments", {
  expect_error(openapi_param("verbose", required = "yes"))
  expect_error(openapi_param("verbose", description = 1L))
  expect_error(openapi_request_body(openapi_schema_string(), required = "yes"))
  expect_error(openapi_docs(summary = 1L))
  expect_error(openapi_docs(tags = 1L))
})
