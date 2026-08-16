#' OpenAPI Parameter
#'
#' Describe a single query, path, header, or cookie parameter. Path parameters
#' are documented automatically from the route's `:param` tokens with a string
#' schema; declare one here with `location = "path"` to override that default,
#' e.g. to document it as an integer.
#'
#' With validation on, a query parameter documented with
#' [openapi_schema_array()] holds every occurrence of its name, so `?tag=r`
#' and `?tag=r&tag=api` both reach the handler as an array on
#' `request$query`.
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
#' @seealso [openapi_docs()], which parameters are passed to, [openapi-schemas]
#' for the `schema` argument, and [routing-http-methods].
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

#' Print OpenAPI Objects
#'
#' Print a one line summary of an OpenAPI object. These objects are `list`s
#' under the hood, so printing them unaltered would dump the whole nested
#' structure; the summary names the object and the field that identifies it
#' instead.
#'
#' To see how an object is actually rendered into the document, enable docs
#' with `app$openapi()` and read the JSON served at `spec_path` (default
#' `/openapi.json`).
#'
#' @section What is shown:
#'
#' - Parameter: its location and name, e.g. *OpenAPI query parameter "verbose"*.
#' - Request body: its media type.
#' - Response: its status.
#' - Schema: its type, its name if it was named with [openapi_schema_ref()],
#'   or, for a bare reference, the name of the schema it points at.
#' - Docs: a rule, then the summary and tags when they are set.
#'
#' @param x Object /// Required. \cr
#'          The OpenAPI object to print. One of the classes built by
#'          [openapi_param()], [openapi_request_body()], [openapi_response()],
#'          [openapi_docs()], or the [openapi-schemas] constructors.
#'
#' @param ... Ignored /// Optional. \cr
#'            Not used, present for compatibility with the [print()] generic.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' openapi_param(name = "verbose", schema = openapi_schema_boolean())
#'
#' openapi_response(200, "The user")
#'
#' # a named schema prints its name, a bare reference says so
#' openapi_schema_ref("User", openapi_schema_object())
#'
#' openapi_schema_ref("User")
#'
#' @seealso [openapi_docs()], [openapi-schemas]
#'
#' @name openapi-print
NULL

#' @rdname openapi-print
#' @export
print.ambiorix_openapi_parameter <- function(x, ...) {
  cli::cli_alert_info(
    "OpenAPI {x$location} parameter {.val {x$name}}"
  )
  invisible(x)
}

#' OpenAPI Request Body
#'
#' Describe the body accepted by a route. Pass the result to the
#' `request_body` argument of [openapi_docs()].
#'
#' With validation enabled the body is checked against `schema` before the
#' handler runs, and a `400` is returned if it does not match. JSON,
#' form-urlencoded, and multipart bodies are checked. Other media types are
#' documented but passed through to the handler unvalidated.
#'
#' A form field sent more than once, as a multiple select is, holds every
#' value it was sent with. `request$payload$tags` is the whole selection,
#' not its first choice. Document such a property with
#' [openapi_schema_array()] to have one choice be an array too, and to have
#' each choice checked against `items`.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               An OpenAPI schema (see [openapi-schemas]) describing the body.
#'               Name it with [openapi_schema_ref()] to have it emitted once
#'               under the document's `components` and referenced wherever it
#'               is used.
#'
#' @param description String /// Optional. \cr
#'                    Human readable description of the body. \cr
#'                    Defaults to `NULL`.
#'
#' @param required Logical /// Optional. \cr
#'                 Whether the body is required. Either `TRUE` (default) or
#'                 `FALSE`. \cr
#'                 When `TRUE` and validation is enabled, a request that
#'                 arrives without a body is rejected.
#'
#' @param content_type String /// Optional. \cr
#'                     Media type of the body. \cr
#'                     Defaults to `"application/json"`. JSON, form-urlencoded,
#'                     and multipart bodies are validated; other media types
#'                     are documented only.
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
#' # name the schema to reuse it across routes: it lands in `components`
#' # and is referenced with `$ref` instead of being repeated
#' openapi_request_body(
#'   schema = openapi_schema_ref(
#'     "NewUser",
#'     openapi_schema_object(
#'       properties = list(name = openapi_schema_string()),
#'       required = "name"
#'     )
#'   ),
#'   description = "The user to create"
#' )
#'
#' # documented, but not validated: the body is not JSON
#' openapi_request_body(
#'   schema = openapi_schema_string(format = "binary"),
#'   content_type = "application/octet-stream"
#' )
#'
#' @seealso [openapi_docs()], which the body is passed to, and [openapi-schemas]
#' for the `schema` argument.
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

#' @rdname openapi-print
#' @export
print.ambiorix_openapi_request_body <- function(x, ...) {
  cli::cli_alert_info("An OpenAPI request body ({.val {x$content_type}})")
  invisible(x)
}

#' OpenAPI Response
#'
#' Describe a single response for a route. Pass one, or a `list` of them, to
#' the `responses` argument of [openapi_docs()].
#'
#' Responses are documentation only: ambiorix never checks what a handler
#' actually sends, so describing a `404` here does not make one happen. Every
#' documented route should describe at least its success response, since the
#' OpenAPI specification requires a non-empty `responses` object.
#'
#' @param status Integer or String /// Required. \cr
#'               HTTP status code, e.g. `200L`. Also accepts the string
#'               `"default"`, which describes every status not documented
#'               explicitly, or a status range such as `"2XX"`, as allowed by
#'               the OpenAPI specification.
#'
#' @param description String /// Required. \cr
#'                    Human readable description of the response. Required by
#'                    the OpenAPI specification, which is why, unlike elsewhere
#'                    in this family, it has no default.
#'
#' @param schema OpenAPI schema /// Optional. \cr
#'               An OpenAPI schema (see [openapi-schemas]) describing the
#'               response body. \cr
#'               Defaults to `NULL`, for a response with no body, e.g. a `204`.
#'
#' @param headers Named list of OpenAPI schemas /// Optional. \cr
#'                The headers sent with the response, named after the header
#'                and described with a schema, e.g.
#'                `list(Location = openapi_schema_string())`. Note that these
#'                are schemas, not [openapi_param()] objects. \cr
#'                Defaults to `NULL`.
#'
#' @param content_type String /// Optional. \cr
#'                     Media type of the response body. \cr
#'                     Defaults to `"application/json"`. Ignored when `schema`
#'                     is `NULL`.
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
#' # a response with no body
#' openapi_response(204, "Deleted")
#'
#' # a range and a catch-all, rather than a single code
#' openapi_response("4XX", "Something was wrong with the request")
#'
#' openapi_response("default", "Unexpected error")
#'
#' @seealso [openapi_docs()], which responses are passed to, and
#' [openapi-schemas] for the `schema` and `headers` arguments.
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

#' @rdname openapi-print
#' @export
print.ambiorix_openapi_response <- function(x, ...) {
  cli::cli_alert_info("An OpenAPI response ({.val {x$status}})")
  invisible(x)
}

#' OpenAPI Route Documentation
#'
#' Document a single route. Pass the result to the `docs` argument of a routing
#' method (see [routing-http-methods]). A route without `docs` does not appear
#' in the generated document at all.
#'
#' @details
#'
#' Path parameters are documented automatically from the route's `:param`
#' tokens with a string schema, so only query, header, and cookie parameters
#' need to be declared. To override an automatic path parameter (e.g. to
#' document it as an integer), declare it with [openapi_param()] using
#' `location = "path"` and a name matching the route token.
#'
#' The object is inert: nothing is rendered or checked when it is created. The
#' document is built once, when the app starts, which is when problems such as
#' two different schemas sharing a name are reported.
#'
#' @section Validation:
#'
#' Validation is off by default and turned on app-wide with
#' `app$openapi(validate = TRUE)`. When it is on, query and path parameters and
#' a JSON, form-urlencoded, or multipart request body are checked against the
#' schemas documented here before the handler runs, and a `400` is returned
#' when they do not match. Header and cookie parameters are never checked.
#'
#' The `validate` argument below overrides that app-wide setting for this one
#' route, in either direction: `FALSE` opts a route out of validation, `TRUE`
#' opts a single route in while the rest of the app stays unchecked.
#'
#' @param summary String /// Optional. \cr
#'                Short summary of what the route does; shown as the title of
#'                the operation in the Swagger UI. \cr
#'                Defaults to `NULL`.
#'
#' @param description String /// Optional. \cr
#'                    Longer description of the route, shown when the operation
#'                    is expanded. \cr
#'                    Defaults to `NULL`.
#'
#' @param tags Character vector /// Optional. \cr
#'             Tags used to group routes in the UI. Describe the tags
#'             themselves with the `tags` argument of `app$openapi()`. \cr
#'             Defaults to `NULL`.
#'
#' @param parameters List of OpenAPI parameters /// Optional. \cr
#'                   Query, path, header, and cookie parameters, each created
#'                   with [openapi_param()]. A single parameter may be passed
#'                   on its own, without wrapping it in a `list()`. \cr
#'                   Defaults to `NULL`.
#'
#' @param request_body OpenAPI request body /// Optional. \cr
#'                     The body the route accepts, created with
#'                     [openapi_request_body()]. \cr
#'                     Defaults to `NULL`.
#'
#' @param responses List of OpenAPI responses /// Optional. \cr
#'                  The responses the route may return, each created with
#'                  [openapi_response()]. A single response may be passed on
#'                  its own, without wrapping it in a `list()`. \cr
#'                  Defaults to `NULL`. Note that the OpenAPI specification
#'                  requires every operation to document at least one response.
#'
#' @param operation_id String /// Optional. \cr
#'                     Unique identifier for the operation, used by client
#'                     generators to name the method they generate. Must be
#'                     unique across the whole document; duplicates abort the
#'                     build when the app starts. \cr
#'                     Defaults to `NULL`. On a route registered with `all()`,
#'                     which answers several methods, the HTTP method is
#'                     appended to keep each operation unique, e.g.
#'                     `"listUsers_get"`.
#'
#' @param deprecated Logical /// Optional. \cr
#'                   Whether the route is deprecated. Either `FALSE` (default)
#'                   or `TRUE`, which strikes the operation through in the UI.
#'                   The route still works.
#'
#' @param security Character vector or List /// Optional. \cr
#'                 Names of the security schemes that apply to this route; see
#'                 the `security_schemes` argument of `app$openapi()`. A `list`
#'                 is passed through as-is, for schemes that take scopes, e.g.
#'                 `list(list(oauth = c("read:users")))`. \cr
#'                 Defaults to `NULL`, which inherits the app-wide `security`.
#'                 Pass `list()` to declare that this route needs no
#'                 authentication.
#'
#' @param validate Logical /// Optional. \cr
#'                 Whether to validate incoming requests against the documented
#'                 schemas; see the Validation section. \cr
#'                 Defaults to `NULL`, which inherits the app-wide setting from
#'                 the `validate` argument of `app$openapi()`.
#'
#' @param ... Key=Value pairs /// Optional. \cr
#'            Additional fields of the
#'            [operation object](https://spec.openapis.org/oas/v3.1.0#operation-object),
#'            e.g. `externalDocs`.
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
#' # a fuller route: a query parameter, a body, a security scheme, and
#' # validation turned on for this route alone
#' openapi_docs(
#'   summary = "Create a user",
#'   operation_id = "createUser",
#'   tags = "users",
#'   parameters = openapi_param(
#'     name = "dry_run",
#'     description = "Validate the payload without saving it",
#'     schema = openapi_schema_boolean()
#'   ),
#'   request_body = openapi_request_body(
#'     schema = openapi_schema_object(
#'       properties = list(
#'         name = openapi_schema_string(minLength = 1L),
#'         email = openapi_schema_string(format = "email")
#'       ),
#'       required = c("name", "email")
#'     )
#'   ),
#'   responses = list(
#'     openapi_response(201, "The created user"),
#'     openapi_response(400, "Invalid payload")
#'   ),
#'   security = "bearerAuth",
#'   validate = TRUE
#' )
#'
#' # a single parameter or response need not be wrapped in a `list()`
#' openapi_docs(responses = openapi_response(204, "Deleted"))
#'
#' @seealso [routing-http-methods] for the `docs` argument this is passed to,
#' and [openapi_param()], [openapi_request_body()], [openapi_response()],
#' [openapi-schemas] for its own arguments.
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

#' @rdname openapi-print
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
#' Lets `openapi_docs(responses = openapi_response(200, "OK"))` mean the same
#' as passing a one element `list()`. The objects are `list`s themselves, so
#' iterating an unwrapped one would walk its fields rather than treat it as a
#' single element; `predicate` is what tells the two cases apart.
#'
#' @param x List or Object /// Required. \cr
#'          A `list` of objects, or a single object.
#'
#' @param predicate Function /// Required. \cr
#'                  Class check for a single object, e.g.
#'                  `is_openapi_response`.
#'
#' @return A `list`, or `NULL` when `x` is `NULL`.
#'
#' @examples
#' # a single object is wrapped
#' as_openapi_list(openapi_response(200, "OK"), is_openapi_response)
#'
#' # a list is left alone
#' as_openapi_list(list(openapi_response(200, "OK")), is_openapi_response)
#'
#' as_openapi_list(NULL, is_openapi_response)
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
#' @param x List /// Required. \cr
#'          The elements to check.
#'
#' @param predicate Function /// Required. \cr
#'                  Class check applied to every element, e.g.
#'                  `is_openapi_response`.
#'
#' @param constructor String /// Required. \cr
#'                    Name of the function elements must be created with,
#'                    used in the error message. Written without the
#'                    parentheses, e.g. `"openapi_response"`.
#'
#' @return `x`, invisibly. Stops with the offending index otherwise.
#'
#' @examples
#' responses <- list(openapi_response(200, "OK"))
#' assert_openapi_elements(responses, is_openapi_response, "openapi_response")
#'
#' # names the element that is wrong, rather than failing on the whole list
#' try(
#'   assert_openapi_elements(
#'     list(openapi_response(200, "OK"), "not a response"),
#'     is_openapi_response,
#'     "openapi_response"
#'   )
#' )
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
