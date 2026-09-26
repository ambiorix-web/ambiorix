test_that("openapi_path converts :param to {param}", {
  expect_equal(openapi_path("/users/:id"), "/users/{id}")
  expect_equal(
    openapi_path("/users/:id/posts/:post"),
    "/users/{id}/posts/{post}"
  )
  expect_equal(openapi_path("/users"), "/users")
})

test_that("openapi_path_params reads the path itself", {
  expect_equal(openapi_path_params("/users/:id"), "id")
  expect_equal(
    openapi_path_params("/users/:id/posts/:post"),
    c("id", "post")
  )
  expect_length(openapi_path_params("/users"), 0L)
})

test_that("path params survive a custom path to pattern converter", {
  # `Route$params` is not populated when a converter is installed: the
  # params must come from the path itself
  as_path_to_pattern(function(path) {
    sprintf("^%s$", gsub(":[^/]+", "[^/]+", path))
  })
  on.exit(.globals$pathToPattern <- NULL)

  app <- Ambiorix$new()
  app$get(
    "/users/:id",
    function(req, res) res$json(list()),
    docs = openapi_docs(summary = "Get a user")
  )

  doc <- build_openapi(app$get_routes())

  params <- doc$paths[["/users/{id}"]]$get$parameters
  expect_length(params, 1L)
  expect_equal(params[[1]]$name, "id")

  stop_all()
})

test_that("build_openapi produces a valid document", {
  app <- Ambiorix$new()

  app$get(
    "/users/:id",
    function(req, res) res$json(list(id = req$params$id)),
    docs = openapi_docs(
      summary = "Get a user by ID",
      tags = "users",
      parameters = list(
        openapi_param(
          "verbose",
          location = "query",
          schema = openapi_schema_boolean()
        )
      ),
      responses = list(openapi_response(200, "The user"))
    )
  )

  # undocumented route should be excluded
  app$get("/health", function(req, res) res$json(list(ok = TRUE)))

  routes <- app$get_routes()
  doc <- build_openapi(
    routes,
    list(info = list(title = "Test", version = "9.9.9"))
  )

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

test_that("named schemas are hoisted into components and referenced", {
  task <- openapi_schema_ref(
    "Task",
    openapi_schema_object(
      properties = list(id = openapi_schema_integer()),
      required = "id"
    )
  )

  app <- Ambiorix$new()

  # the same schema is used twice: it must be registered once
  app$get(
    "/tasks",
    function(req, res) res$json(list()),
    docs = openapi_docs(
      responses = list(
        openapi_response(200, "Tasks", openapi_schema_array(task))
      )
    )
  )

  app$get(
    "/tasks/:id",
    function(req, res) res$json(list()),
    docs = openapi_docs(responses = list(openapi_response(200, "A task", task)))
  )

  doc <- build_openapi(app$get_routes())

  expect_equal(names(doc$components$schemas), "Task")
  expect_equal(doc$components$schemas$Task$required, list("id"))

  ref <- doc$paths[["/tasks/{id}"]]$get$responses[["200"]]$content[[
    "application/json"
  ]]$schema
  expect_equal(ref, list(`$ref` = "#/components/schemas/Task"))

  # nested inside an array, the reference is hoisted just the same
  items <- doc$paths[["/tasks"]]$get$responses[["200"]]$content[[
    "application/json"
  ]]$schema$items
  expect_equal(items, list(`$ref` = "#/components/schemas/Task"))

  stop_all()
})

test_that("named schemas nested in composition keywords are hoisted", {
  schema <- openapi_schema(
    oneOf = list(
      openapi_schema_ref("A", openapi_schema_object()),
      openapi_schema_ref("B", openapi_schema_object())
    )
  )

  routes <- list(
    list(
      route = list(full_path = "/x"),
      method = "GET",
      docs = openapi_docs(responses = list(openapi_response(200, "x", schema)))
    )
  )

  doc <- build_openapi(routes)
  expect_setequal(names(doc$components$schemas), c("A", "B"))
})

test_that("two different schemas sharing a name is an error", {
  mk <- function(path, schema) {
    list(
      route = list(full_path = path),
      method = "GET",
      docs = openapi_docs(
        responses = list(openapi_response(200, "ok", schema))
      )
    )
  }

  a <- openapi_schema_ref(
    "Task",
    openapi_schema_object(properties = list(id = openapi_schema_integer()))
  )
  b <- openapi_schema_ref(
    "Task",
    openapi_schema_object(properties = list(id = openapi_schema_string()))
  )

  expect_error(build_openapi(list(mk("/a", a), mk("/b", b))), "Task")

  # the same definition twice is fine
  expect_error(build_openapi(list(mk("/a", a), mk("/b", a))), NA)
})

test_that("references to undefined schemas are an error", {
  routes <- list(
    list(
      route = list(full_path = "/x"),
      method = "GET",
      docs = openapi_docs(
        responses = list(
          openapi_response(200, "ok", openapi_schema_ref("Nope"))
        )
      )
    )
  )

  expect_error(build_openapi(routes), "Nope")
})

test_that("an undefined schema inside `not` is caught at build", {
  routes <- list(
    list(
      route = list(full_path = "/x"),
      method = "POST",
      docs = openapi_docs(
        request_body = openapi_request_body(
          openapi_schema_object(
            properties = list(n = openapi_schema_integer()),
            not = openapi_schema_ref("Nope")
          )
        )
      )
    )
  )

  expect_error(build_openapi(routes), "Nope")
})

test_that("a recursive schema can be expressed with a bare reference", {
  task <- openapi_schema_ref(
    "Task",
    openapi_schema_object(
      properties = list(
        id = openapi_schema_integer(),
        subtasks = openapi_schema_array(openapi_schema_ref("Task"))
      )
    )
  )

  routes <- list(
    list(
      route = list(full_path = "/x"),
      method = "GET",
      docs = openapi_docs(responses = list(openapi_response(200, "ok", task)))
    )
  )

  doc <- build_openapi(routes)
  expect_equal(
    doc$components$schemas$Task$properties$subtasks$items,
    list(`$ref` = "#/components/schemas/Task")
  )
})

test_that("document level fields render", {
  routes <- list(
    list(
      route = list(full_path = "/x"),
      method = "GET",
      docs = openapi_docs(
        security = "bearerAuth",
        deprecated = TRUE,
        responses = list(openapi_response(200, "ok"))
      )
    )
  )

  doc <- build_openapi(
    routes,
    list(
      info = list(title = "T", version = "1"),
      servers = "https://api.example.com",
      tags = c(things = "All the things", "misc"),
      security_schemes = list(
        bearerAuth = list(type = "http", scheme = "bearer")
      )
    )
  )

  expect_equal(doc$servers, list(list(url = "https://api.example.com")))
  expect_equal(
    doc$tags,
    list(
      list(name = "things", description = "All the things"),
      list(name = "misc")
    )
  )
  expect_equal(
    doc$components$securitySchemes$bearerAuth,
    list(type = "http", scheme = "bearer")
  )

  op <- doc$paths[["/x"]]$get
  expect_equal(op$security, list(list(bearerAuth = list())))
  expect_true(op$deprecated)
})

test_that("security is read into requirements", {
  serialise <- function(security) {
    default_serialiser(openapi_security_requirements(security))
  }

  # a character vector names alternatives
  expect_equal(serialise("bearerAuth"), '[{"bearerAuth":[]}]')
  expect_equal(
    serialise(c("bearerAuth", "apiKey")),
    '[{"bearerAuth":[]},{"apiKey":[]}]'
  )

  # a named list is one requirement; a single scope is still an array
  expect_equal(
    serialise(list(oauth = "read:users")),
    '[{"oauth":["read:users"]}]'
  )
  expect_equal(
    serialise(list(oauth = c("read", "write"), apiKey = NULL)),
    '[{"oauth":["read","write"],"apiKey":[]}]'
  )

  # an unnamed list holds alternatives: schemes needed together, or scopes
  expect_equal(
    serialise(list(c("apiKey", "appId"), list(oauth = "read"))),
    '[{"apiKey":[],"appId":[]},{"oauth":["read"]}]'
  )

  # an empty alternative makes authentication optional
  expect_equal(
    serialise(list(character(), "bearerAuth")),
    '[{},{"bearerAuth":[]}]'
  )

  # no authentication
  expect_equal(serialise(list()), "[]")
  expect_equal(serialise(character()), "[]")
})

test_that("a security shape that is not a requirement is an error", {
  expect_error(openapi_docs(security = 1L), "character vector or a list")
  expect_error(openapi_docs(security = list(oauth = 1L)), "requirement 1")
  expect_error(
    openapi_docs(security = list("bearerAuth", list(oauth = NA_character_))),
    "requirement 2"
  )
  expect_error(
    openapi_docs(security = list("a", oauth = "read")),
    "requirement 1"
  )
  expect_error(openapi_docs(security = ""), "requirement 1")
  expect_error(openapi_docs(security = NA_character_), "requirement 1")
  expect_error(
    Ambiorix$new()$openapi(security = list(oauth = 1L)),
    "requirement 1"
  )
})

test_that("a security requirement must name declared schemes", {
  routes <- list(
    list(
      route = list(full_path = "/x"),
      method = "GET",
      docs = openapi_docs(security = list(oauth = "read"))
    )
  )

  expect_error(
    build_openapi(
      routes,
      list(
        security = openapi_security_requirements(c("bearerAuth", "apiKey")),
        security_schemes = list(
          bearerAuth = list(type = "http", scheme = "bearer")
        )
      )
    ),
    "undeclared scheme\\(s\\): `apiKey`, `oauth`"
  )

  # no authentication names nothing
  routes[[1]]$docs <- openapi_docs(security = list())
  expect_equal(build_openapi(routes)$paths[["/x"]]$get$security, list())
})

test_that("user-declared path params override the auto-generated defaults", {
  app <- Ambiorix$new()

  app$get(
    "/tasks/:id/items/:item",
    function(req, res) res$json(list()),
    docs = openapi_docs(
      summary = "Get an item of a task",
      parameters = list(
        openapi_param(
          "id",
          location = "path",
          description = "Task identifier",
          schema = openapi_schema_integer()
        )
      )
    )
  )

  routes <- app$get_routes()
  doc <- build_openapi(routes)

  params <- doc$paths[["/tasks/{id}/items/{item}"]]$get$parameters
  expect_length(params, 2L)

  # declared path param overrides the default string schema, no duplicate
  id_params <- Filter(function(p) p$name == "id", params)
  expect_length(id_params, 1L)
  expect_equal(id_params[[1]]$`in`, "path")
  expect_true(id_params[[1]]$required)
  expect_equal(id_params[[1]]$schema$type, "integer")
  expect_equal(id_params[[1]]$description, "Task identifier")

  # undeclared path param keeps the automatic string default
  item_param <- Filter(function(p) p$name == "item", params)[[1]]
  expect_equal(item_param$`in`, "path")
  expect_true(item_param$required)
  expect_equal(item_param$schema$type, "string")

  stop_all()
})

test_that("path params matching no route token are an error", {
  app <- Ambiorix$new()

  app$get(
    "/tasks/:id",
    function(req, res) res$json(list()),
    docs = openapi_docs(
      summary = "Get a task",
      parameters = list(openapi_param("task_id", location = "path"))
    )
  )

  expect_error(
    build_openapi(app$get_routes()),
    "Path parameter\\(s\\) `task_id` of `/tasks/:id` match no `:param` token"
  )

  # at startup, before a request could be answered with a `400`
  app$openapi()
  expect_error(app$start(open = FALSE), "task_id")

  stop_all()
})

test_that("build_openapi expands all() across verbs", {
  app <- Ambiorix$new()

  app$all(
    "/thing",
    function(req, res) res$json(list(ok = TRUE)),
    docs = openapi_docs(summary = "Any verb")
  )

  routes <- app$get_routes()
  doc <- build_openapi(routes)

  item <- doc$paths[["/thing"]]
  expect_true(all(c("get", "post", "put", "delete", "patch") %in% names(item)))

  stop_all()
})

test_that("operation ids are suffixed per verb and must be unique", {
  route <- list(
    route = list(full_path = "/thing"),
    method = c("GET", "POST"),
    docs = openapi_docs(
      operation_id = "thing",
      responses = list(openapi_response(200, "ok"))
    )
  )

  doc <- build_openapi(list(route))
  expect_equal(doc$paths[["/thing"]]$get$operationId, "thing_get")
  expect_equal(doc$paths[["/thing"]]$post$operationId, "thing_post")

  # a single verb keeps the id as given
  route$method <- "GET"
  doc <- build_openapi(list(route))
  expect_equal(doc$paths[["/thing"]]$get$operationId, "thing")

  other <- route
  other$route$full_path <- "/other"
  expect_error(build_openapi(list(route, other)), "thing")
})

test_that("swagger_ui_html embeds the spec url and title", {
  html <- swagger_ui_html("openapi.json")
  expect_true(grepl("swagger-ui", html))
  expect_true(grepl('url: "openapi.json"', html, fixed = TRUE))
  expect_true(grepl("<title>API Documentation</title>", html, fixed = TRUE))

  html <- swagger_ui_html("openapi.json", title = "My <API>")
  expect_true(grepl("<title>My &lt;API&gt;</title>", html, fixed = TRUE))
})

test_that("swagger_ui_html references local assets, not a CDN", {
  html <- swagger_ui_html("openapi.json")
  expect_true(grepl('"__swagger__/swagger-ui.css"', html, fixed = TRUE))
  expect_true(grepl('"__swagger__/swagger-ui-bundle.js"', html, fixed = TRUE))
  expect_false(grepl("cdn.jsdelivr.net", html, fixed = TRUE))

  # custom assets url, trailing slashes normalised
  html <- swagger_ui_html("openapi.json", assets_url = "../assets/")
  expect_true(grepl('"../assets/swagger-ui.css"', html, fixed = TRUE))
  expect_true(grepl('"../assets/swagger-ui-bundle.js"', html, fixed = TRUE))
})

test_that("documented paths are the paths the app answers", {
  app <- Ambiorix$new()
  handler <- function(req, res) res$send("ok")
  docs <- openapi_docs(summary = "x")

  users <- Router$new("/users")
  users$get("/", handler, docs = docs)
  app$use(users)

  app$get("/items/:id/", handler, docs = docs)

  api <- Router$new("/api/")
  api$get("/status", handler, docs = docs)
  app$use(api)

  root <- Router$new("/")
  root$get("/health", handler, docs = docs)
  app$use(root)

  app$get("/", handler, docs = docs)

  doc <- build_openapi(app$get_routes())
  expect_setequal(
    names(doc$paths),
    c("/users", "/items/{id}", "/api/status", "/health", "/")
  )

  stop_all()
})

test_that("a route at a docs path is caught however it is spelled", {
  app <- Ambiorix$new()
  app$get("/docs/", function(req, res) res$send("my own docs"))
  app$openapi()

  private <- environment(app$openapi)$private
  expect_message(
    private$.register_openapi_routes(),
    "Swagger UI will not be served"
  )

  stop_all()
})

test_that("the docs page and the server are relative to the app", {
  built <- function(configure) {
    app <- Ambiorix$new()
    configure(app)
    app$get("/x", function(req, res) res$send("x"))
    private <- environment(app$openapi)$private
    private$.compile()
    private$.build_openapi()
    html <- private$.openapi_ui_html
    list(
      spec = regmatches(html, regexpr('url: "[^"]*"', html)),
      css = regmatches(html, regexpr('"[^"]*swagger-ui.css"', html)),
      servers = yyjsonr::read_json_str(private$.openapi_json)$servers
    )
  }

  # defaults: `/docs`, `/openapi.json`, `/__swagger__`
  out <- built(function(app) app$openapi())
  expect_equal(out$spec, 'url: "openapi.json"')
  expect_equal(out$css, '"__swagger__/swagger-ui.css"')
  expect_equal(out$servers$url, ".")

  # a deeper page climbs back to the root
  out <- built(function(app) app$openapi(ui_path = "/api/docs"))
  expect_equal(out$spec, 'url: "../openapi.json"')
  expect_equal(out$css, '"../__swagger__/swagger-ui.css"')

  # a deeper document puts the server further up
  out <- built(function(app) app$openapi(spec_path = "/api/v1/openapi.json"))
  expect_equal(out$spec, 'url: "api/v1/openapi.json"')
  expect_equal(out$servers$url, "../..")

  # the basepath moves the routes, not the static assets
  out <- built(function(app) {
    app$basepath <- "/v1"
    app$openapi()
  })
  expect_equal(out$spec, 'url: "../v1/openapi.json"')
  expect_equal(out$css, '"../__swagger__/swagger-ui.css"')
  expect_equal(out$servers$url, "..")

  # servers of the app's own are kept
  out <- built(function(app) app$openapi(servers = "https://api.example.com"))
  expect_equal(out$servers$url, "https://api.example.com")

  stop_all()
})

test_that("swagger ui assets are bundled with the package", {
  dir <- system.file("swagger-ui", package = "ambiorix")
  expect_true(dir.exists(dir))
  expect_true(file.exists(file.path(dir, "swagger-ui.css")))
  expect_true(file.exists(file.path(dir, "swagger-ui-bundle.js")))
  expect_true(file.exists(file.path(dir, "LICENSE")))
  expect_true(file.exists(file.path(dir, "NOTICE")))
})

test_that("an empty paths object serialises to a JSON object", {
  doc <- build_openapi(list())
  json <- default_serialiser(doc)
  expect_true(grepl('"paths":{}', json, fixed = TRUE))

  # no components: the key is omitted rather than emitted empty
  expect_false(grepl("components", json, fixed = TRUE))
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
  app$get_routes()
  routes <- app$get_routes()

  doc <- build_openapi(routes)
  params <- doc$paths[["/users/{id}"]]$get$parameters
  expect_length(params, 1L)

  stop_all()
})

test_that("the document is built once, at startup", {
  app <- Ambiorix$new()

  app$openapi(title = "Cached", version = "2.0.0")
  app$get(
    "/users",
    function(req, res) res$json(list()),
    docs = openapi_docs(summary = "Users")
  )

  private <- environment(app$openapi)$private
  expect_null(private$.openapi_json)

  private$.compile()
  private$.build_openapi()

  expect_type(private$.openapi_json, "character")
  spec <- yyjsonr::read_json_str(private$.openapi_json)
  expect_equal(spec$info$title, "Cached")
  expect_true("/users" %in% names(spec$paths))

  stop_all()
})

test_that("openapi_named_schemas collects the schemas by name", {
  task <- openapi_schema_ref(
    "Task",
    openapi_schema_object(properties = list(id = openapi_schema_integer()))
  )

  routes <- list(
    list(
      route = list(full_path = "/x"),
      method = "GET",
      docs = openapi_docs(responses = list(openapi_response(200, "ok", task)))
    )
  )

  schemas <- openapi_named_schemas(routes)
  expect_equal(names(schemas), "Task")
  expect_s3_class(schemas$Task, "ambiorix_openapi_schema")
})
