#' OpenAPI Parameter
#'
#' Describe a single query, path, header, or cookie parameter. Path parameters
#' are documented automatically from the route's `:param` tokens with a string
#' schema; declare one here with `location = "path"` to override that default,
#' e.g. to document it as an integer.
#'
#' @param name String /// Required. \cr
#'             Name of the parameter. For path parameters this must match one
#'             of the route's `:param` tokens.
#'
#' @param location String /// Optional. \cr
#'                 Where the parameter is passed. Either `"query"` (default),
#'                 `"path"`, `"header"`, or `"cookie"`.
#'
#' @param required Logical /// Optional. \cr
#'                 Whether the parameter is required. Either `FALSE` (default)
#'                 or `TRUE`. \cr
#'                 Path parameters are always required so this
#'                 argument is ignored for them and forced to `TRUE`.
#'
#' @param description String /// Optional. \cr
#'                    Human readable description of the parameter. \cr
#'                    Defaults to `NULL`.
#'
#' @param schema OpenAPI schema /// Optional. \cr
#'               An OpenAPI schema (see [openapi-schemas]) describing the
#'               parameter's type. \cr
#'               Defaults to [openapi_schema_string()].
#'
#' @param ... Key=Value pairs /// Optional. \cr
#'            Additional fields of the [parameter object](https://spec.openapis.org/oas/v3.1.0#parameter-object),
#'            e.g. `style`, `explode`, or `example`.
#'
#' @return An object of class `ambiorix_openapi_parameter`.
#'
#' @examples
#' openapi_param(
#'   name = "verbose",
#'   location = "query",
#'   description = "Return extra fields",
#'   schema = openapi_schema_boolean()
#' )
#'
#' # override the automatic string schema of a path parameter
#' openapi_param(
#'   name = "id",
#'   location = "path",
#'   schema = openapi_schema_integer()
#' )
#'
#' @export
openapi_param <- function(
  name,
  location = c("query", "path", "header", "cookie"),
  required = FALSE,
  description = NULL,
  schema = openapi_schema_string(),
  ...
) {
  assert_that(not_missing(name))
  assert_that(is_string(name))

  location <- match.arg(arg = location)

  assert_that(is_flag(required))
  assert_that(is.null(description) || is_string(description))
  assert_that(is_openapi_schema(schema))

  extra <- list(...)

  if (length(extra)) {
    assert_that(has_names(extra))
  }

  # the OpenAPI specification mandates that path parameters are required
  if (identical(location, "path")) {
    required <- TRUE
  }

  structure(
    list(
      name = name,
      location = location,
      required = required,
      description = description,
      schema = schema,
      extra = extra
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

#' OpenAPI Request Body
#'
#' Describe the body accepted by a route.
#'
#' @param schema An OpenAPI schema (see [openapi-schemas]) describing the body.
#' @param description Human readable description of the body.
#' @param required Whether the body is required.
#' @param content_type The media type of the body.
#'
#' @return An object of class `ambiorix_openapi_request_body`.
#'
#' @examples
#' openapi_request_body(
#'   schema = openapi_schema_object(
#'     properties = list(name = openapi_schema_string()),
#'     required = "name"
#'   )
#' )
#'
#' @export
openapi_request_body <- function(
  schema,
  description = NULL,
  required = TRUE,
  content_type = "application/json"
) {
  assert_that(not_missing(schema))
  assert_that(is_openapi_schema(schema))
  assert_that(is.null(description) || is_string(description))
  assert_that(is_flag(required))
  assert_that(is_string(content_type))

  structure(
    list(
      schema = schema,
      description = description,
      required = required,
      content_type = content_type
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
#' @param schema An optional OpenAPI schema (see [openapi-schemas]) describing
#' the response body.
#' @param headers An optional named `list` of OpenAPI schemas describing the
#' headers sent with the response.
#' @param content_type The media type of the response body.
#'
#' @return An object of class `ambiorix_openapi_response`.
#'
#' @examples
#' openapi_response(
#'   200,
#'   "The user",
#'   schema = openapi_schema_object(
#'     properties = list(id = openapi_schema_integer())
#'   )
#' )
#'
#' openapi_response(
#'   201,
#'   "The created user",
#'   headers = list(Location = openapi_schema_string())
#' )
#'
#' @export
openapi_response <- function(
  status,
  description,
  schema = NULL,
  headers = NULL,
  content_type = "application/json"
) {
  assert_that(not_missing(status))
  assert_that(is_openapi_status(status))
  assert_that(not_missing(description))
  assert_that(is_string(description))
  assert_that(is_string(content_type))

  if (!is.null(schema)) {
    assert_that(is_openapi_schema(schema))
  }

  if (!is.null(headers)) {
    assert_that(is.list(headers))
    assert_that(has_names(headers))
    assert_that(is_openapi_schema_list(headers))
  }

  if (is.numeric(status)) {
    status <- as.character(as.integer(status))
  }

  structure(
    list(
      status = status,
      description = description,
      schema = schema,
      headers = headers,
      content_type = content_type
    ),
    class = c("ambiorix_openapi_response", "list")
  )
}

#' @export
print.ambiorix_openapi_response <- function(x, ...) {
  cli::cli_alert_info("An OpenAPI response ({.val {x$status}})")
  invisible(x)
}

#' OpenAPI Route Documentation
#'
#' Document a single route. Pass the result to the `docs` argument of a routing
#' method (see [routing-http-methods]).
#'
#' Path parameters are documented automatically from the route's `:param`
#' tokens with a string schema, so only query, header, and cookie parameters
#' need to be declared. To override an automatic path parameter (e.g. to
#' document it as an integer), declare it with [openapi_param()] using
#' `location = "path"` and a name matching the route token.
#'
#' @param summary Short summary of what the route does.
#' @param description Longer description of the route.
#' @param tags Character vector of tags used to group routes.
#' @param parameters A `list` of parameters created with [openapi_param()]. A
#' single parameter may be passed on its own.
#' @param request_body The request body; see [openapi_request_body()].
#' @param responses A `list` of responses created with [openapi_response()]. A
#' single response may be passed on its own.
#' @param operation_id Unique identifier for the operation, used by client
#' generators. Must be unique across the whole document.
#' @param deprecated Whether the route is deprecated.
#' @param security Names of the security schemes (see the `security_schemes`
#' argument of `app$openapi()`) that apply to this route. A `list` is passed
#' through as-is, for schemes that take scopes.
#' @param validate Whether to validate incoming requests against the
#' documented schemas. `NULL`, the default, inherits the app-wide setting;
#' see the `validate` argument of `app$openapi()`.
#' @param ... Additional fields of the
#' [operation object](https://spec.openapis.org/oas/v3.1.0#operation-object),
#' e.g. `externalDocs`.
#'
#' @return An object of class `ambiorix_openapi_docs`.
#'
#' @examples
#' openapi_docs(
#'   summary = "Get a user by ID",
#'   tags = "users",
#'   responses = list(
#'     openapi_response(200, "The user"),
#'     openapi_response(404, "Not found")
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
  responses = NULL,
  operation_id = NULL,
  deprecated = FALSE,
  security = NULL,
  validate = NULL,
  ...
) {
  assert_that(is.null(summary) || is_string(summary))
  assert_that(is.null(description) || is_string(description))
  assert_that(is.null(tags) || is.character(tags))
  assert_that(is.null(operation_id) || is_string(operation_id))
  assert_that(is_flag(deprecated))
  assert_that(is.null(validate) || is_flag(validate))

  # a single parameter or response may be passed without wrapping it in a
  # `list()`: they are lists themselves, so iterating them would otherwise
  # walk their fields
  parameters <- as_openapi_list(parameters, is_openapi_parameter)
  responses <- as_openapi_list(responses, is_openapi_response)

  if (!is.null(parameters)) {
    assert_openapi_elements(parameters, is_openapi_parameter, "openapi_param")
  }

  if (!is.null(responses)) {
    assert_openapi_elements(responses, is_openapi_response, "openapi_response")
  }

  if (!is.null(request_body)) {
    assert_that(is_openapi_request_body(request_body))
  }

  if (!is.null(security)) {
    assert_that(is.character(security) || is.list(security))
  }

  extra <- list(...)

  if (length(extra)) {
    assert_that(has_names(extra))
  }

  structure(
    list(
      summary = summary,
      description = description,
      tags = tags,
      parameters = parameters,
      request_body = request_body,
      responses = responses,
      operation_id = operation_id,
      deprecated = deprecated,
      security = security,
      validate = validate,
      extra = extra
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

#' Accept a Single OpenAPI Object Where a List Is Expected
#'
#' @param x A `list` of objects, or a single object.
#' @param predicate Class check for a single object.
#'
#' @return A `list`, or `NULL`.
#'
#' @keywords internal
#' @noRd
as_openapi_list <- function(x, predicate) {
  if (is.null(x)) {
    return(NULL)
  }

  if (predicate(x)) {
    return(list(x))
  }

  x
}

#' Check Every Element of a List of OpenAPI Objects
#'
#' Reports the index of the offending element: `assert_that()` on the list as
#' a whole cannot say which one is wrong.
#'
#' @param x A `list`.
#' @param predicate Class check applied to every element.
#' @param constructor Name of the function elements must be created with.
#'
#' @keywords internal
#' @noRd
assert_openapi_elements <- function(x, predicate, constructor) {
  if (!is.list(x)) {
    stop(
      "`",
      constructor,
      "` objects must be passed in a `list()`",
      call. = FALSE
    )
  }

  for (i in seq_along(x)) {
    if (predicate(x[[i]])) {
      next
    }

    stop(
      "element ",
      i,
      " must be created with `",
      constructor,
      "()`",
      call. = FALSE
    )
  }

  invisible(x)
}
