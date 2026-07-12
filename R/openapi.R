#' OpenAPI Schemas
#'
#' Build [OpenAPI Schema Objects](https://spec.openapis.org/oas/v3.1.0#schema-object)
#' used to describe request bodies, responses, and parameters.
#'
#' @param items An OpenAPI schema (from any `openapi_schema_*()` helper)
#' describing the type of every element in the array.
#' @param ... Named OpenAPI schemas describing the properties of an object.
#'
#' @return An object of class `ambiorix_openapi_schema`; a `list` that mirrors
#' an OpenAPI schema object.
#'
#' @examples
#' openapi_schema_string()
#'
#' openapi_schema_object(
#'   id = openapi_schema_integer(),
#'   name = openapi_schema_string(),
#'   tags = openapi_schema_array(openapi_schema_string())
#' )
#'
#' @name openapi-schemas
NULL

#' @keywords internal
#' @noRd
new_openapi_schema <- function(x) {
  structure(x, class = c("ambiorix_openapi_schema", "list"))
}

#' @rdname openapi-schemas
#' @export
openapi_schema_string <- function() {
  new_openapi_schema(list(type = "string"))
}

#' @rdname openapi-schemas
#' @export
openapi_schema_integer <- function() {
  new_openapi_schema(list(type = "integer"))
}

#' @rdname openapi-schemas
#' @export
openapi_schema_number <- function() {
  new_openapi_schema(list(type = "number"))
}

#' @rdname openapi-schemas
#' @export
openapi_schema_boolean <- function() {
  new_openapi_schema(list(type = "boolean"))
}

#' @rdname openapi-schemas
#' @export
openapi_schema_array <- function(items) {
  assert_that(not_missing(items))
  assert_that(is_openapi_schema(items))
  new_openapi_schema(list(type = "array", items = unclass(items)))
}

#' @rdname openapi-schemas
#' @export
openapi_schema_object <- function(...) {
  properties <- list(...)

  if (length(properties)) {
    assert_that(has_names(properties))

    for (p in properties) {
      assert_that(is_openapi_schema(p))
    }
  }

  properties <- lapply(properties, unclass)

  # ensure an empty `properties` serialises to `{}`, not `[]`
  if (!length(properties)) {
    properties <- named_list()
  }

  new_openapi_schema(
    list(
      type = "object",
      properties = properties
    )
  )
}

#' @export
print.ambiorix_openapi_schema <- function(x, ...) {
  cli::cli_alert_info("An OpenAPI schema of type {.val {x$type}}")
  invisible(x)
}

#' OpenAPI Parameter
#'
#' Describe a single query, header, or cookie parameter. Path parameters are
#' derived automatically from the route's `:param` tokens and must not be
#' declared here.
#'
#' @param name Name of the parameter.
#' @param location Where the parameter is passed; one of `"query"`, `"header"`,
#' or `"cookie"`. `"path"` is not allowed: path parameters are inferred from the
#' route.
#' @param required Whether the parameter is required.
#' @param description Human readable description of the parameter.
#' @param schema An OpenAPI schema (see [openapi-schemas]) describing the
#' parameter's type.
#'
#' @return An object of class `ambiorix_openapi_parameter`.
#'
#' @examples
#' openapi_param(
#'   "verbose",
#'   location = "query",
#'   description = "Return extra fields",
#'   schema = openapi_schema_boolean()
#' )
#'
#' @export
openapi_param <- function(
  name,
  location = c("query", "header", "cookie"),
  required = FALSE,
  description = NULL,
  schema = openapi_schema_string()
) {
  assert_that(not_missing(name))
  assert_that(is_string(name))

  if (identical(location, "path")) {
    stop(
      "Path parameters are derived automatically from the route path; ",
      "do not declare them with `openapi_param()`.",
      call. = FALSE
    )
  }

  location <- match.arg(location)
  assert_that(is_flag(required))
  assert_that(is.null(description) || is_string(description))
  assert_that(is_openapi_schema(schema))

  structure(
    list(
      name = name,
      location = location,
      required = required,
      description = description,
      schema = schema
    ),
    class = c("ambiorix_openapi_parameter", "list")
  )
}

#' @export
print.ambiorix_openapi_parameter <- function(x, ...) {
  cli::cli_alert_info(
    "OpenAPI {x$location} parameter {.val {x$name}}"
  )
  invisible(x)
}

#' OpenAPI Parameters
#'
#' Collect query, header, and cookie parameters for a route. Path parameters are
#' derived automatically from the route's `:param` tokens.
#'
#' @param ... Objects created with [openapi_param()].
#'
#' @return An object of class `ambiorix_openapi_parameters`.
#'
#' @examples
#' openapi_parameters(
#'   openapi_param("verbose", location = "query", schema = openapi_schema_boolean()),
#'   openapi_param("X-Trace", location = "header")
#' )
#'
#' @export
openapi_parameters <- function(...) {
  params <- list(...)

  for (p in params) {
    assert_that(is_openapi_parameter(p))
  }

  structure(
    params,
    class = c("ambiorix_openapi_parameters", "list")
  )
}

#' @export
print.ambiorix_openapi_parameters <- function(x, ...) {
  cli::cli_alert_info("{length(x)} OpenAPI parameter{?s}")
  invisible(x)
}

#' OpenAPI Request Body
#'
#' Describe the body accepted by a route.
#'
#' @param schema An OpenAPI schema (see [openapi-schemas]) describing the body.
#' @param content_type The media type of the body.
#' @param required Whether the body is required.
#' @param description Human readable description of the body.
#'
#' @return An object of class `ambiorix_openapi_request_body`.
#'
#' @examples
#' openapi_request_body(
#'   schema = openapi_schema_object(
#'     name = openapi_schema_string()
#'   )
#' )
#'
#' @export
openapi_request_body <- function(
  schema,
  content_type = "application/json",
  required = TRUE,
  description = NULL
) {
  assert_that(not_missing(schema))
  assert_that(is_openapi_schema(schema))
  assert_that(is_string(content_type))
  assert_that(is_flag(required))
  assert_that(is.null(description) || is_string(description))

  structure(
    list(
      schema = schema,
      content_type = content_type,
      required = required,
      description = description
    ),
    class = c("ambiorix_openapi_request_body", "list")
  )
}

#' @export
print.ambiorix_openapi_request_body <- function(x, ...) {
  cli::cli_alert_info("An OpenAPI request body ({.val {x$content_type}})")
  invisible(x)
}

#' OpenAPI Response
#'
#' Describe a single response for a route.
#'
#' @param status HTTP status code, e.g. `200L`. Also accepts the string
#' `"default"` or a status range such as `"2XX"`, as allowed by the OpenAPI
#' specification.
#' @param description Human readable description of the response.
#' @param content_type The media type of the response body.
#' @param schema An optional OpenAPI schema (see [openapi-schemas]) describing
#' the response body.
#'
#' @return An object of class `ambiorix_openapi_response`.
#'
#' @examples
#' openapi_response(
#'   200,
#'   "The user",
#'   schema = openapi_schema_object(id = openapi_schema_integer())
#' )
#'
#' @export
openapi_response <- function(
  status,
  description,
  content_type = "application/json",
  schema = NULL
) {
  assert_that(not_missing(status))
  assert_that(is_openapi_status(status))
  assert_that(not_missing(description))
  assert_that(is_string(description))
  assert_that(is_string(content_type))

  if (!is.null(schema)) {
    assert_that(is_openapi_schema(schema))
  }

  if (is.numeric(status)) {
    status <- as.character(as.integer(status))
  }

  structure(
    list(
      status = status,
      description = description,
      content_type = content_type,
      schema = schema
    ),
    class = c("ambiorix_openapi_response", "list")
  )
}

#' @export
print.ambiorix_openapi_response <- function(x, ...) {
  cli::cli_alert_info("An OpenAPI response ({.val {x$status}})")
  invisible(x)
}

#' OpenAPI Responses
#'
#' Collect the responses of a route.
#'
#' @param ... Objects created with [openapi_response()].
#'
#' @return An object of class `ambiorix_openapi_responses`.
#'
#' @examples
#' openapi_responses(
#'   openapi_response(200, "Success"),
#'   openapi_response(404, "Not found")
#' )
#'
#' @export
openapi_responses <- function(...) {
  responses <- list(...)

  for (r in responses) {
    assert_that(is_openapi_response(r))
  }

  structure(
    responses,
    class = c("ambiorix_openapi_responses", "list")
  )
}

#' @export
print.ambiorix_openapi_responses <- function(x, ...) {
  cli::cli_alert_info("{length(x)} OpenAPI response{?s}")
  invisible(x)
}

#' OpenAPI Route Documentation
#'
#' Document a single route. Pass the result to the `docs` argument of a routing
#' method (see [routing-http-methods]).
#'
#' Path parameters are documented automatically from the route's `:param`
#' tokens, so only query, header, and cookie parameters need to be declared via
#' [openapi_parameters()].
#'
#' @param summary Short summary of what the route does.
#' @param description Longer description of the route.
#' @param tags Character vector of tags used to group routes.
#' @param parameters Query, header, and cookie parameters; see
#' [openapi_parameters()].
#' @param request_body The request body; see [openapi_request_body()].
#' @param responses The responses; see [openapi_responses()].
#'
#' @return An object of class `ambiorix_openapi_docs`.
#'
#' @examples
#' openapi_docs(
#'   summary = "Get a user by ID",
#'   tags = "users",
#'   responses = openapi_responses(
#'     openapi_response(200, "The user")
#'   )
#' )
#'
#' @seealso [routing-http-methods]
#'
#' @export
openapi_docs <- function(
  summary = NULL,
  description = NULL,
  tags = NULL,
  parameters = NULL,
  request_body = NULL,
  responses = NULL
) {
  assert_that(is.null(summary) || is_string(summary))
  assert_that(is.null(description) || is_string(description))
  assert_that(is.null(tags) || is.character(tags))

  if (!is.null(parameters)) {
    assert_that(is_openapi_parameters(parameters))
  }

  if (!is.null(request_body)) {
    assert_that(is_openapi_request_body(request_body))
  }

  if (!is.null(responses)) {
    assert_that(is_openapi_responses(responses))
  }

  structure(
    list(
      summary = summary,
      description = description,
      tags = tags,
      parameters = parameters,
      request_body = request_body,
      responses = responses
    ),
    class = c("ambiorix_openapi_docs", "list")
  )
}

#' @export
print.ambiorix_openapi_docs <- function(x, ...) {
  cli::cli_rule("Ambiorix", right = "OpenAPI docs")
  if (!is.null(x$summary)) {
    cli::cli_li("summary: {.val {x$summary}}")
  }
  if (!is.null(x$tags)) {
    cli::cli_li("tags: {.val {x$tags}}")
  }
  invisible(x)
}

#' Convert an ambiorix path to an OpenAPI path
#'
#' Turns `/users/:id` into `/users/{id}`.
#'
#' @param path A route path.
#'
#' @keywords internal
#' @noRd
openapi_path <- function(path) {
  gsub(":([^/]+)", "{\\1}", path)
}

#' Build an OpenAPI parameter object from an ambiorix parameter
#'
#' @keywords internal
#' @noRd
openapi_render_parameter <- function(param) {
  out <- list(
    name = param$name,
    `in` = param$location,
    required = param$required,
    schema = unclass(param$schema)
  )

  if (!is.null(param$description)) {
    out$description <- param$description
  }

  out
}

#' Build the OpenAPI operation object for a single route
#'
#' @keywords internal
#' @noRd
openapi_render_operation <- function(route) {
  docs <- route$docs
  operation <- list()

  if (!is.null(docs$summary)) {
    operation$summary <- docs$summary
  }

  if (!is.null(docs$description)) {
    operation$description <- docs$description
  }

  if (!is.null(docs$tags)) {
    operation$tags <- as.list(docs$tags)
  }

  # auto-derive path parameters from the route's :param tokens
  parameters <- list()
  path_params <- route$route$params
  for (pname in path_params) {
    parameters <- append(
      parameters,
      list(
        list(
          name = pname,
          `in` = "path",
          required = TRUE,
          schema = list(type = "string")
        )
      )
    )
  }

  # query / header / cookie parameters
  if (!is.null(docs$parameters)) {
    for (param in docs$parameters) {
      parameters <- append(
        parameters,
        list(openapi_render_parameter(param))
      )
    }
  }

  if (length(parameters)) {
    operation$parameters <- parameters
  }

  if (!is.null(docs$request_body)) {
    body <- docs$request_body
    content <- list()
    content[[body$content_type]] <- list(schema = unclass(body$schema))
    operation$requestBody <- list(
      required = body$required,
      content = content
    )
    if (!is.null(body$description)) {
      operation$requestBody$description <- body$description
    }
  }

  responses <- list()
  if (!is.null(docs$responses)) {
    for (resp in docs$responses) {
      entry <- list(description = resp$description)
      if (!is.null(resp$schema)) {
        content <- list()
        content[[resp$content_type]] <- list(schema = unclass(resp$schema))
        entry$content <- content
      }
      responses[[resp$status]] <- entry
    }
  }

  # OpenAPI requires at least one response
  if (!length(responses)) {
    responses[["default"]] <- list(description = "Default response")
  }
  operation$responses <- responses

  operation
}

#' Build an OpenAPI document from a list of routes
#'
#' @param routes A list of routes as stored in `Routing`'s private `.routes`.
#' @param info A list of OpenAPI info fields (`title`, `version`, ...).
#'
#' @return A list representing an OpenAPI 3.1.0 document.
#'
#' @keywords internal
#' @noRd
build_openapi <- function(routes, info = list()) {
  info$title <- info$title %error% "API"
  info$version <- info$version %error% "1.0.0"

  # named so an empty `paths` serialises to `{}`, not `[]`
  paths <- named_list()

  for (route in routes) {
    if (is.null(route$docs)) {
      next
    }

    if (!is_openapi_docs(route$docs)) {
      next
    }

    full_path <- paste0(route$route$basepath, route$path)
    oapi_path <- openapi_path(full_path)
    operation <- openapi_render_operation(route)

    if (is.null(paths[[oapi_path]])) {
      paths[[oapi_path]] <- list()
    }

    for (method in route$method) {
      paths[[oapi_path]][[tolower(method)]] <- operation
    }
  }

  list(
    openapi = "3.1.0",
    info = info,
    paths = paths
  )
}

#' Build the Swagger UI HTML page
#'
#' The Swagger UI assets (CSS & JavaScript) are bundled with ambiorix
#' (`inst/swagger-ui/`) and referenced relative to `assets_path`, so the
#' page works without an internet connection.
#'
#' @param spec_url URL of the OpenAPI JSON document.
#' @param title Title of the page.
#' @param assets_path Path at which the Swagger UI assets are served.
#'
#' @return A single character string of HTML.
#'
#' @keywords internal
#' @noRd
swagger_ui_html <- function(
  spec_url,
  title = "API Documentation",
  assets_path = "/__swagger__"
) {
  title <- title %error% "API Documentation"
  assets_path <- sub("/+$", "", assets_path)
  sprintf(
    '<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>%s</title>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <link
      rel="stylesheet"
      href="%s/swagger-ui.css"
    />
  </head>
  <body>
    <div id="swagger-ui"></div>
    <script src="%s/swagger-ui-bundle.js"></script>
    <script>
      window.onload = function () {
        window.ui = SwaggerUIBundle({
          url: "%s",
          dom_id: "#swagger-ui"
        });
      };
    </script>
  </body>
</html>',
    html_escape(title),
    assets_path,
    assets_path,
    spec_url
  )
}
