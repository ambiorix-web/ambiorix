#' Validate a Request Against Its Documentation
#'
#' Checks the request body and the query and path parameters against the
#' schemas the route documents. Parameters arrive as strings, so those
#' documented with another type are converted before they are checked, and the
#' converted value is written back onto the request. Request body is
#' parsed and stored on `request$payload`.
#'
#' Header and cookie parameters are not checked: ambiorix does not know
#' which of them a handler cares about.
#'
#' @param request The [Request].
#' @param docs The route's docs, see [openapi_docs()].
#' @param schemas Named list of the document's schemas, used to resolve
#' references.
#'
#' @return A `list` of problems, empty when the request is valid. Each is a
#'         `list(location, path, message)`, see `openapi_detail()`.
#'
#' @examples
#' \dontrun{
#' # `request` is the live Request object a handler receives
#' openapi_validate_request(
#'   request,
#'   openapi_docs(
#'     parameters = openapi_param(
#'       "limit",
#'       required = TRUE,
#'       schema = openapi_schema_integer(minimum = 1L)
#'     )
#'   )
#' )
#'
#' # `?limit=abc` yields one problem, and `req$query$limit` is left alone;
#' # `?limit=10` yields none, and `req$query$limit` becomes the integer 10
#' }
#'
#' @keywords internal
#' @noRd
openapi_validate_request <- function(request, docs, schemas = list()) {
  details <- list()

  for (param in docs$parameters) {
    if (!param$location %in% c("query", "path")) {
      next
    }

    is_query <- identical(param$location, "query")
    is_path <- identical(param$location, "path")
    value <- switch(
      EXPR = param$location,
      query = request$query[[param$name]],
      path = request$params[[param$name]]
    )

    if (is.null(value)) {
      if (param$required) {
        details <- append(
          details,
          list(
            openapi_detail(param$location, param$name, "is required")
          )
        )
      }

      next
    }

    converted <- openapi_convert(value, param$schema, schemas)

    if (inherits(converted, "ambiorix_openapi_conversion_error")) {
      details <- append(
        details,
        list(
          openapi_detail(param$location, param$name, converted$message)
        )
      )
      next
    }

    if (is_query) {
      request$query[[param$name]] <- converted
    }

    if (is_path) {
      request$params[[param$name]] <- converted
    }

    details <- append(
      details,
      openapi_detail_list(
        param$location,
        openapi_validate(converted, param$schema, schemas, param$name)
      )
    )
  }

  if (is.null(docs$request_body)) {
    return(details)
  }

  # only JSON bodies are understood
  is_json <- grepl(
    pattern = "json",
    x = docs$request_body$content_type,
    fixed = TRUE
  )

  if (!is_json) {
    return(details)
  }

  payload <- request$parse_json()
  request$payload <- payload

  if (!length(payload)) {
    if (docs$request_body$required) {
      details <- append(
        details,
        list(openapi_detail("body", "", "a request body is required"))
      )
    }

    return(details)
  }

  append(
    details,
    openapi_detail_list(
      "body",
      openapi_validate(payload, docs$request_body$schema, schemas)
    )
  )
}

#' A Single Problem With a Request
#'
#' The shape reported back to the client in the `400`. Kept as a constructor
#' rather than an inline `list()` so the three field names are spelled in one
#' place.
#'
#' @param location Where the problem is: `"body"`, `"query"`, or `"path"`.
#' @param path Path to the offending value, e.g. `"tags[2]"`.
#' @param message What is wrong with it, phrased to follow the path, e.g.
#'                `"is required"`.
#'
#' @return A `list` of `location`, `path`, and `message`.
#'
#' @examples
#' openapi_detail("query", "limit", "must be an integer")
#'
#' openapi_detail("body", "user.tags[2]", "must be a string")
#'
#' @keywords internal
#' @noRd
openapi_detail <- function(location, path, message) {
  list(location = location, path = path, message = message)
}

#' Stamp a Location Onto Every Problem
#'
#' `openapi_validate()` reports `path` and `message` but knows nothing about
#' where the value came from; the caller does. This adds that missing field to
#' a whole batch at once.
#'
#' @param location Where the problems are: `"body"`, `"query"`, or `"path"`.
#' @param problems A `list` of `list(path, message)`.
#'
#' @return A `list` of details, see `openapi_detail()`.
#'
#' @examples
#' openapi_detail_list(
#'   "body",
#'   list(
#'     list(path = "name", message = "is required"),
#'     list(path = "age", message = "must be an integer")
#'   )
#' )
#'
#' @keywords internal
#' @noRd
openapi_detail_list <- function(location, problems) {
  lapply(
    X = problems,
    FUN = function(problem) {
      openapi_detail(location, problem$path, problem$message)
    }
  )
}

#' Validate a Value Against a Schema
#'
#' The recursive core of validation. Resolves the schema, checks the type,
#' then hands off to whichever of the `openapi_check_*()` helpers the type
#' calls for; arrays and objects come back here for each element or property,
#' which is what walks a nested body.
#'
#' A wrong type short-circuits: every keyword check assumes the type is right,
#' so reporting *must be an integer* alongside *must be at least 1 character
#' long* would only be noise. Schemas that are empty, or that could not be
#' resolved, accept anything.
#'
#' @param value Value to check.
#' @param schema An OpenAPI schema.
#' @param schemas Named list of schemas, used to resolve references.
#' @param path Path to `value`, used in the messages. Empty at the top of a
#'             body, and grown by `openapi_child_path()` on the way down.
#'
#' @return A `list` of `list(path, message)`, empty when the value is valid.
#'
#' @examples
#' schema <- openapi_schema_object(
#'   properties = list(
#'     name = openapi_schema_string(minLength = 1L),
#'     tags = openapi_schema_array(openapi_schema_string())
#'   ),
#'   required = "name"
#' )
#'
#' # valid
#' openapi_validate(list(name = "ambiorix"), schema)
#'
#' # a missing property, and a nested one of the wrong type
#' openapi_validate(list(tags = list("web", 1L)), schema)
#'
#' # a wrong type short-circuits: no `minLength` complaint on top
#' openapi_validate(1L, openapi_schema_string(minLength = 5L))
#'
#' @keywords internal
#' @noRd
openapi_validate <- function(value, schema, schemas = list(), path = "") {
  schema <- openapi_resolve_schema(schema, schemas)

  if (is.null(schema) || !length(unclass(schema))) {
    return(list())
  }

  problems <- list()
  fail <- function(message) {
    problems[[length(problems) + 1L]] <<- list(path = path, message = message)
  }

  if (!is.null(schema$type)) {
    matches <- vapply(
      X = schema$type,
      FUN = function(type) openapi_is_type(value, type),
      FUN.VALUE = logical(1)
    )

    if (!any(matches)) {
      fail(sprintf("must be %s", openapi_type_label(schema$type)))

      # every other check assumes the type is right
      return(problems)
    }
  }

  if (!is.null(schema$enum)) {
    allowed <- unlist(schema$enum, use.names = FALSE)

    if (!openapi_scalar(value) %in% allowed) {
      fail(
        sprintf("must be one of %s", paste0(allowed, collapse = ", "))
      )
    }
  }

  if (is.numeric(value) && length(value) == 1L) {
    problems <- c(problems, openapi_check_number(value, schema, path))
  }

  if (openapi_is_type(value, "string")) {
    problems <- c(problems, openapi_check_string(value, schema, path))
  }

  if (openapi_is_type(value, "array")) {
    problems <- c(
      problems,
      openapi_check_array(value, schema, schemas, path)
    )
  }

  if (openapi_is_type(value, "object")) {
    problems <- c(
      problems,
      openapi_check_object(value, schema, schemas, path)
    )
  }

  problems
}

#' Check the Numeric Keywords of a Schema
#'
#' Applies to both `integer` and `number`: the keywords are the same, only the
#' type check differs, and that has already happened by the time this runs.
#' Keywords the schema does not set are skipped, so an empty schema yields
#' nothing.
#'
#' @param value A number.
#' @param schema The schema it is checked against.
#' @param path Path to `value`, used in the messages.
#'
#' @return A `list` of `list(path, message)`, empty when `value` is valid.
#'
#' @examples
#' schema <- openapi_schema_integer(minimum = 1L, maximum = 10L)
#'
#' openapi_check_number(5L, schema, "limit")
#'
#' openapi_check_number(20L, schema, "limit")
#'
#' # several keywords can fail at once
#' openapi_check_number(
#'   7L,
#'   openapi_schema_integer(maximum = 5L, multipleOf = 2L),
#'   "limit"
#' )
#'
#' @keywords internal
#' @noRd
openapi_check_number <- function(value, schema, path) {
  problems <- list()
  fail <- function(message) {
    problems[[length(problems) + 1L]] <<- list(path = path, message = message)
  }

  if (!is.null(schema$minimum) && value < schema$minimum) {
    fail(sprintf("must be greater than or equal to %s", schema$minimum))
  }

  if (!is.null(schema$maximum) && value > schema$maximum) {
    fail(sprintf("must be less than or equal to %s", schema$maximum))
  }

  if (!is.null(schema$exclusiveMinimum) && value <= schema$exclusiveMinimum) {
    fail(sprintf("must be greater than %s", schema$exclusiveMinimum))
  }

  if (!is.null(schema$exclusiveMaximum) && value >= schema$exclusiveMaximum) {
    fail(sprintf("must be less than %s", schema$exclusiveMaximum))
  }

  if (!is.null(schema$multipleOf) && value %% schema$multipleOf != 0) {
    fail(sprintf("must be a multiple of %s", schema$multipleOf))
  }

  problems
}

#' Check the String Keywords of a Schema
#'
#' Note that `format` is not checked. The specification treats it as an
#' annotation rather than a constraint, so `format = "email"` documents the
#' intent without rejecting anything; use `pattern` to actually enforce it.
#'
#' @param value A string.
#' @param schema The schema it is checked against.
#' @param path Path to `value`, used in the messages.
#'
#' @return A `list` of `list(path, message)`, empty when `value` is valid.
#'
#' @examples
#' openapi_check_string("ambiorix", openapi_schema_string(maxLength = 4L), "name")
#'
#' openapi_check_string("", openapi_schema_string(minLength = 1L), "name")
#'
#' openapi_check_string(
#'   "nope",
#'   openapi_schema_string(pattern = "^[a-z]+@[a-z]+$"),
#'   "email"
#' )
#'
#' # `format` documents, it does not constrain: no problem reported
#' openapi_check_string("nope", openapi_schema_string(format = "email"), "email")
#'
#' @keywords internal
#' @noRd
openapi_check_string <- function(value, schema, path) {
  problems <- list()
  fail <- function(message) {
    problems[[length(problems) + 1L]] <<- list(path = path, message = message)
  }

  if (!is.null(schema$minLength) && nchar(value) < schema$minLength) {
    fail(sprintf("must be at least %s character(s) long", schema$minLength))
  }

  if (!is.null(schema$maxLength) && nchar(value) > schema$maxLength) {
    fail(sprintf("must be at most %s character(s) long", schema$maxLength))
  }

  if (!is.null(schema$pattern) && !grepl(schema$pattern, value)) {
    fail(sprintf("must match the pattern %s", schema$pattern))
  }

  problems
}

#' Check the Array Keywords of a Schema
#'
#' Checks the array itself, then recurses into every element through
#' `openapi_validate()`, so a problem deep inside a nested array is still
#' reported with the index that leads to it, e.g. `tags[2]`. Indices are 1
#' based, matching R rather than JSON, since the message is read by whoever
#' wrote the R handler.
#'
#' An array with no `items` has unconstrained elements and only its own
#' keywords are checked.
#'
#' @param value An array.
#' @param schema The schema it is checked against.
#' @param schemas Named list of schemas, used to resolve references.
#' @param path Path to `value`, used in the messages.
#'
#' @return A `list` of `list(path, message)`, empty when `value` is valid.
#'
#' @examples
#' schema <- openapi_schema_array(openapi_schema_string(), minItems = 2L)
#'
#' openapi_check_array(list("web", "api"), schema, list(), "tags")
#'
#' # too short
#' openapi_check_array(list("web"), schema, list(), "tags")
#'
#' # the offending element is named by its index
#' openapi_check_array(list("web", 1L, "api"), schema, list(), "tags")
#'
#' openapi_check_array(
#'   list("web", "web"),
#'   openapi_schema_array(openapi_schema_string(), uniqueItems = TRUE),
#'   list(),
#'   "tags"
#' )
#'
#' @keywords internal
#' @noRd
openapi_check_array <- function(value, schema, schemas, path) {
  problems <- list()
  elements <- openapi_elements(value)

  if (!is.null(schema$minItems) && length(elements) < schema$minItems) {
    problems <- append(
      problems,
      list(
        list(
          path = path,
          message = sprintf("must have at least %s item(s)", schema$minItems)
        )
      )
    )
  }

  if (!is.null(schema$maxItems) && length(elements) > schema$maxItems) {
    problems <- append(
      problems,
      list(
        list(
          path = path,
          message = sprintf("must have at most %s item(s)", schema$maxItems)
        )
      )
    )
  }

  if (isTRUE(schema$uniqueItems) && anyDuplicated(elements)) {
    problems <- append(
      problems,
      list(list(path = path, message = "must not contain duplicates"))
    )
  }

  if (is.null(schema$items)) {
    return(problems)
  }

  for (i in seq_along(elements)) {
    problems <- c(
      problems,
      openapi_validate(
        elements[[i]],
        schema$items,
        schemas,
        sprintf("%s[%s]", path, i)
      )
    )
  }

  problems
}

#' Check the Object Keywords of a Schema
#'
#' Three passes: required properties that are absent, properties that are not
#' allowed when `additionalProperties` is `FALSE`, and then every declared
#' property that *is* present, recursively through `openapi_validate()`.
#'
#' A property that is absent is only ever a problem when it is required.
#' Undeclared properties are allowed by default, which is what the
#' specification says: set `additionalProperties = FALSE` to reject them.
#'
#' @param value An object.
#' @param schema The schema it is checked against.
#' @param schemas Named list of schemas, used to resolve references.
#' @param path Path to `value`, used in the messages. Property names are
#'             appended with a `.`, e.g. `"user.name"`.
#'
#' @return A `list` of `list(path, message)`, empty when `value` is valid.
#'
#' @examples
#' schema <- openapi_schema_object(
#'   properties = list(
#'     name = openapi_schema_string(),
#'     age = openapi_schema_integer(minimum = 0L)
#'   ),
#'   required = "name"
#' )
#'
#' openapi_check_object(list(name = "ambiorix"), schema, list(), "body")
#'
#' openapi_check_object(list(age = -1L), schema, list(), "body")
#'
#' # undeclared properties are allowed unless the schema says otherwise
#' openapi_check_object(list(name = "a", extra = 1L), schema, list(), "body")
#'
#' openapi_check_object(
#'   list(name = "a", extra = 1L),
#'   openapi_schema_object(
#'     properties = list(name = openapi_schema_string()),
#'     additionalProperties = FALSE
#'   ),
#'   list(),
#'   "body"
#' )
#'
#' @keywords internal
#' @noRd
openapi_check_object <- function(value, schema, schemas, path) {
  problems <- list()

  for (name in schema$required) {
    if (!is.null(value[[name]])) {
      next
    }

    problems <- append(
      problems,
      list(
        list(path = openapi_child_path(path, name), message = "is required")
      )
    )
  }

  if (identical(schema$additionalProperties, FALSE)) {
    unknown <- setdiff(names(value), names(schema$properties))

    for (name in unknown) {
      problems <- append(
        problems,
        list(
          list(
            path = openapi_child_path(path, name),
            message = "is not an allowed property"
          )
        )
      )
    }
  }

  for (name in names(schema$properties)) {
    if (is.null(value[[name]])) {
      next
    }

    problems <- c(
      problems,
      openapi_validate(
        value[[name]],
        schema$properties[[name]],
        schemas,
        openapi_child_path(path, name)
      )
    )
  }

  problems
}

#' Convert a Parameter to Its Documented Type
#'
#' Query and path parameters always arrive as strings. Documenting one as an
#' integer would therefore always fail the type check, so the value is
#' converted first, and the converted value is written back onto the request:
#' a handler for a route documented with `openapi_schema_integer()` receives an
#' integer, not `"10"`.
#'
#' Anything not worth converting is returned untouched: a string schema, a
#' union of types, an unresolvable reference, or a value that is not a single
#' string. A conversion that cannot succeed is a validation problem rather
#' than an error, so it comes back as a condition-like object the caller turns
#' into a message.
#'
#' @param value The value, a string.
#' @param schema The parameter's schema.
#' @param schemas Named list of schemas, used to resolve references.
#'
#' @return The converted value, or an object of class
#' `ambiorix_openapi_conversion_error`.
#'
#' @examples
#' openapi_convert("10", openapi_schema_integer())
#'
#' openapi_convert("1.5", openapi_schema_number())
#'
#' openapi_convert("true", openapi_schema_boolean())
#'
#' # strings are left alone
#' openapi_convert("10", openapi_schema_string())
#'
#' # not convertible: reported rather than thrown
#' openapi_convert("abc", openapi_schema_integer())
#'
#' @keywords internal
#' @noRd
openapi_convert <- function(value, schema, schemas = list()) {
  schema <- openapi_resolve_schema(schema, schemas)

  if (is.null(schema) || !is.character(value) || length(value) != 1L) {
    return(value)
  }

  type <- schema$type

  if (is.null(type) || length(type) != 1L) {
    return(value)
  }

  converted <- switch(
    type,
    integer = suppressWarnings(as.integer(value)),
    number = suppressWarnings(as.numeric(value)),
    boolean = openapi_as_logical(value),
    value
  )

  if (!is.null(converted) && !is.na(converted)) {
    return(converted)
  }

  structure(
    list(message = sprintf("must be %s", openapi_type_label(type))),
    class = "ambiorix_openapi_conversion_error"
  )
}

#' Read a Query String Boolean
#'
#' Deliberately stricter than [as.logical()], which accepts `"T"` and `"yes"`
#' and quietly returns `NA` for anything else. Only the spellings a client
#' would actually send are accepted: JSON's `true`/`false`, R's
#' `TRUE`/`FALSE`, and `1`/`0`.
#'
#' @param x A single string.
#'
#' @return `TRUE`, `FALSE`, or `NA` when `x` is none of the accepted
#'         spellings.
#'
#' @examples
#' openapi_as_logical("true")
#'
#' openapi_as_logical("0")
#'
#' # not an accepted spelling
#' openapi_as_logical("yes")
#'
#' @keywords internal
#' @noRd
openapi_as_logical <- function(x) {
  if (x %in% c("true", "TRUE", "1")) {
    return(TRUE)
  }

  if (x %in% c("false", "FALSE", "0")) {
    return(FALSE)
  }

  NA
}

#' Resolve a Reference to a Named Schema
#'
#' Validation works on the schema objects themselves, where a reference is an
#' `openapi_name` attribute on an otherwise empty schema, and on rendered
#' schemas, where it is a `$ref` string. Both forms are looked up here.
#'
#' A name that is in neither resolves to `NULL`, which callers treat as "no
#' constraints" rather than an error: a dangling reference is already reported
#' as a note when the document is built.
#'
#' @param schema An OpenAPI schema, possibly a bare reference.
#' @param schemas Named list of schemas.
#'
#' @return The resolved schema, `NULL` when it cannot be found, or `schema`
#'         itself when it is not a reference.
#'
#' @examples
#' schemas <- list(User = openapi_schema_object(
#'   properties = list(id = openapi_schema_integer())
#' ))
#'
#' openapi_resolve_schema(openapi_schema_ref("User"), schemas)
#'
#' # a `$ref` string resolves too
#' openapi_resolve_schema(list(`$ref` = "#/components/schemas/User"), schemas)
#'
#' # not a reference: returned as-is
#' openapi_resolve_schema(openapi_schema_string(), schemas)
#'
#' # unknown name
#' openapi_resolve_schema(openapi_schema_ref("Nope"), schemas)
#'
#' @keywords internal
#' @noRd
openapi_resolve_schema <- function(schema, schemas) {
  if (is.null(schema)) {
    return(NULL)
  }

  name <- attr(schema, "openapi_name")

  # a bare reference holds no keywords of its own
  if (!is.null(name) && !length(unclass(schema))) {
    return(schemas[[name]])
  }

  if (!is.null(schema[["$ref"]])) {
    return(schemas[[basename(schema[["$ref"]])]])
  }

  schema
}

#' Does a Value Have the Given JSON Type?
#'
#' The default JSON parser collapses a one element array to a scalar, so those
#' two cannot be told apart after [parse_json()]. When a value is marked
#' `AsIs` (for example by a custom parser with `length1_array_asis`), it is
#' treated as an array rather than a scalar.
#'
#' The other awkward case is `integer`, which JSON does not have: `1` and `1.0`
#' both parse to a number, so an integer is a number that equals its own
#' truncation.
#'
#' Types that are not part of the specification are not checked, and pass.
#'
#' @param value Value to check.
#' @param type A JSON type, e.g. `"string"`.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' openapi_is_type("ambiorix", "string")
#'
#' # JSON has no integer type of its own
#' openapi_is_type(1, "integer")
#'
#' openapi_is_type(1.5, "integer")
#'
#' # AsIs marks a one element array so it is not a scalar
#' openapi_is_type(I(1L), "array")
#'
#' openapi_is_type(I(1L), "integer")
#'
#' openapi_is_type(list(id = 1L), "object")
#'
#' @keywords internal
#' @noRd
openapi_is_type <- function(value, type) {
  if (identical(type, "null")) {
    return(is.null(value))
  }

  if (is.null(value)) {
    return(FALSE)
  }

  scalar <- !inherits(value, "AsIs") && length(value) == 1L && !is.list(value)

  switch(
    EXPR = type,
    string = scalar && is.character(value),
    # JSON has no integer type of its own: 1 and 1.0 both parse to a number
    integer = scalar &&
      is.numeric(value) &&
      !is.na(value) &&
      value == trunc(value),
    number = scalar && is.numeric(value),
    boolean = scalar && is.logical(value) && !is.na(value),
    array = inherits(value, "AsIs") ||
      (is.list(value) && is.null(names(value))) ||
      (is.atomic(value) && length(value) != 1L),
    object = is.list(value) &&
      !inherits(value, "AsIs") &&
      !is.null(names(value)),
    # unknown types are not checked
    TRUE
  )
}

#' Name a JSON Type for an Error Message
#'
#' Supplies the article, so the message reads *must be an integer* rather than
#' *must be integer*. Several types are joined with "or", for a schema that
#' documents a union. An unknown type is used as-is.
#'
#' @param type Character vector of JSON types.
#'
#' @return A single character string.
#'
#' @examples
#' openapi_type_label("integer")
#'
#' openapi_type_label(c("string", "null"))
#'
#' @keywords internal
#' @noRd
openapi_type_label <- function(type) {
  labels <- c(
    array = "an array",
    boolean = "a boolean",
    integer = "an integer",
    null = "null",
    number = "a number",
    object = "an object",
    string = "a string"
  )

  paste0(labels[type] %||% type, collapse = " or ")
}

#' Elements of an Array
#'
#' A parsed JSON array reaches validation in three shapes: an atomic vector, a
#' list, or either of those marked `AsIs` because it held a single element.
#' This flattens all three to a plain list, so that iterating an array is the
#' same regardless of how it arrived.
#'
#' @param x A parsed JSON array.
#'
#' @return A `list`.
#'
#' @examples
#' openapi_elements(c("web", "api"))
#'
#' openapi_elements(list("web", "api"))
#'
#' # a one element array, as the parser marks it
#' openapi_elements(I("web"))
#'
#' @keywords internal
#' @noRd
openapi_elements <- function(x) {
  if (inherits(x, "AsIs")) {
    x <- unclass(x)
  }

  if (is.list(x)) {
    return(x)
  }

  as.list(x)
}

#' Strip the AsIs Marker From a Value
#'
#' The counterpart to `openapi_elements()`, for comparisons rather than
#' iteration. `I("a") %in% c("a")` is `TRUE`, but the marker leaks into
#' anything built from the result, so it is dropped before the `enum` check
#' compares a value against the allowed set.
#'
#' @param x A parsed JSON value.
#'
#' @return `x`, without its `AsIs` class.
#'
#' @examples
#' openapi_scalar(I("todo"))
#'
#' # anything else is untouched
#' openapi_scalar("todo")
#'
#' @keywords internal
#' @noRd
openapi_scalar <- function(x) {
  if (inherits(x, "AsIs")) {
    return(unclass(x))
  }

  x
}

#' Path to a Property of an Object
#'
#' Joins with a `.`, unless the parent path is empty, which is the case at the
#' top of a request body: the first level of properties is reported as `name`
#' rather than `.name`.
#'
#' @param path Path to the parent, possibly `""`.
#' @param name Name of the property.
#'
#' @return A single character string.
#'
#' @examples
#' openapi_child_path("user", "name")
#'
#' # at the top of a body there is no parent to join to
#' openapi_child_path("", "name")
#'
#' openapi_child_path("user.address", "city")
#'
#' @keywords internal
#' @noRd
openapi_child_path <- function(path, name) {
  if (!nzchar(path)) {
    return(name)
  }

  paste0(path, ".", name)
}
