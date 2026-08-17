#' Validate a Request Against Its Documentation
#'
#' Checks the request body and the query and path parameters against the
#' schemas the route documents. Parameters arrive as strings, so those
#' documented with another type are converted before they are checked, and the
#' converted value is written back onto the request. Request body is
#' parsed and stored on `request$payload`.
#'
#' Supported body media types: `application/json`,
#' `application/x-www-form-urlencoded`, and `multipart/form-data`. Other
#' types are documented only and skipped here.
#'
#' A body the parser cannot read at all is a problem like any other, rather
#' than an error: it is reported as `could not be parsed as <media type>`, and
#' the parser's own message is logged for whoever wrote the app.
#'
#' A form field, like a query parameter, can arrive more than once: a
#' multiple select posts one occurrence per choice. Both are parsed into a
#' flat list that repeats the name, which `[[` cannot read past, so every
#' occurrence of a name is gathered by `openapi_field()` and the name is
#' left holding a single value: the scalar it was sent as, or an array of
#' every occurrence. A field documented as an array is always an array,
#' even when it arrived once.
#'
#' Header and cookie parameters are not checked: ambiorix does not know
#' which of them a handler cares about.
#'
#' @param request Request /// Required. \cr
#'                The [Request] to check.
#'
#' @param docs OpenAPI docs /// Required. \cr
#'             The route's docs, see [openapi_docs()].
#'
#' @param schemas Named list of OpenAPI schemas /// Optional. \cr
#'                The document's schemas, used to resolve references. \cr
#'                Defaults to `list()`.
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

    source <- switch(
      EXPR = param$location,
      query = request$query,
      path = request$params
    )

    value <- openapi_field(
      values = openapi_occurrences(x = source, name = param$name),
      schema = param$schema,
      schemas = schemas
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

    source <- openapi_set_field(x = source, name = param$name, value = value)

    switch(
      EXPR = param$location,
      query = {
        request$query <- source
      },
      path = {
        request$params <- source
      }
    )

    details <- append(
      details,
      openapi_detail_list(
        param$location,
        openapi_validate(value, param$schema, schemas, param$name)
      )
    )
  }

  body <- docs$request_body

  if (is.null(body)) {
    return(details)
  }

  payload <- tryCatch(
    expr = switch(
      EXPR = body$content_type,
      "application/json" = request$parse_json(),
      "application/x-www-form-urlencoded" = openapi_form(
        request$parse_form_urlencoded(),
        body$schema,
        schemas
      ),
      "multipart/form-data" = openapi_form(
        request$parse_multipart(),
        body$schema,
        schemas
      ),
      # otherwise, media type not supported. just return:
      return(details)
    ),
    error = function(error) {
      .globals$errorLog$log(
        "Could not parse request body:",
        conditionMessage(error)
      )

      error
    }
  )

  if (inherits(payload, "error")) {
    return(
      append(
        details,
        list(
          openapi_detail(
            "body",
            "",
            sprintf("could not be parsed as %s", body$content_type)
          )
        )
      )
    )
  }

  request$payload <- payload

  if (!length(payload)) {
    if (body$required) {
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
      openapi_validate(payload, body$schema, schemas)
    )
  )
}

#' Reshape a Parsed Form Into an Object
#'
#' A form body is parsed into a flat list that repeats a name once per
#' occurrence, which is not an object: `payload[["tags"]]` would read the
#' first `tags` and never know of the rest. Each name is gathered into one
#' value here, so what comes back is an ordinary object that
#' `openapi_validate()` walks like any other, and that a handler can read
#' with `[[`.
#'
#' Undocumented fields are shaped too, only without conversion: the schema
#' decides types, not whether a field survives, so
#' `additionalProperties = FALSE` still has something to complain about.
#'
#' @param payload Named list /// Required. \cr
#'                The parsed body, from [parse_form_urlencoded()] or
#'                [parse_multipart()]. One element per occurrence, so a name
#'                may repeat.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The request body schema, whose `properties` shape the
#'               fields.
#'
#' @param schemas Named list of OpenAPI schemas /// Optional. \cr
#'                The document's schemas, used to resolve references. \cr
#'                Defaults to `list()`.
#'
#' @return A named list, one element per distinct field name.
#'
#' @examples
#' openapi_form(
#'   payload = list(name = "Ada", tag = "r", tag = "api"),
#'   schema = openapi_schema_object(
#'     properties = list(
#'       name = openapi_schema_string(),
#'       tag = openapi_schema_array(openapi_schema_string())
#'     )
#'   )
#' )
#'
#' @keywords internal
#' @noRd
openapi_form <- function(payload, schema, schemas = list()) {
  properties <- openapi_resolve_schema(schema, schemas)$properties
  fields <- list()

  for (name in unique(names(payload))) {
    value <- openapi_field(
      openapi_occurrences(payload, name),
      properties[[name]],
      schemas
    )

    if (is.null(value)) {
      next
    }

    fields[[name]] <- value
  }

  fields
}

#' Every Occurrence of a Name
#'
#' `x[[name]]` stops at the first match, which loses the rest of a repeated
#' form field or query parameter. This keeps them all.
#'
#' @param x Named list /// Required. \cr
#'          A parsed form body, query, or path parameter list.
#'
#' @param name String /// Required. \cr
#'             The field name to gather.
#'
#' @return An unnamed `list`, empty when the name is absent.
#'
#' @examples
#' openapi_occurrences(
#'   x = list(tag = "a", n = "1", tag = "b"),
#'   name = "tag"
#' )
#'
#' openapi_occurrences(
#'   x = list(n = "1"),
#'   name = "tag"
#' )
#'
#' @keywords internal
#' @noRd
openapi_occurrences <- function(x, name) {
  nms <- names(x)

  if (is.null(nms)) {
    return(list())
  }

  unname(x[nms == name])
}

#' The Value of a Field, Shaped by Its Schema
#'
#' Query parameters and form fields both arrive as strings, one entry per
#' occurrence, and both are turned into a single value here.
#'
#' Blank occurrences are dropped first: an empty text input or a bare
#' `?limit=` says nothing, and a field left with nothing is absent, which is
#' what makes `required` the one place emptiness is reported.
#'
#' What is left is a scalar, unless the schema documents an array or more
#' than one occurrence arrived. Documenting an array is therefore enough to
#' get one from a single choice, and sending a field twice where the schema
#' documents a scalar is left as an array on purpose: the type check then
#' reports it, rather than a silent first-wins.
#'
#' Conversion is per element and best effort. A value that will not convert
#' is kept as the string it was, so the type check reports it once instead of
#' twice.
#'
#' @param values List /// Required. \cr
#'               Every occurrence of one field, from
#'               `openapi_occurrences()`.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The field's schema. `NULL` for a field the document does
#'               not describe, which is then shaped but not converted.
#'
#' @param schemas Named list of OpenAPI schemas /// Optional. \cr
#'                The document's schemas, used to resolve references. \cr
#'                Defaults to `list()`.
#'
#' @return The field's value, or `NULL` when it has no usable occurrence.
#'
#' @examples
#' openapi_field(
#'   values = list("36"),
#'   schema = openapi_schema_integer()
#' )
#'
#' # documented as an array: one occurrence is still an array
#' openapi_field(
#'   values = list("a"),
#'   schema = openapi_schema_array(openapi_schema_string())
#' )
#'
#' openapi_field(
#'   values = list("1", "2"),
#'   schema = openapi_schema_array(openapi_schema_integer())
#' )
#'
#' # blank occurrences are dropped
#' openapi_field(
#'   values = list(""),
#'   schema = openapi_schema_string()
#' )
#'
#' @keywords internal
#' @noRd
openapi_field <- function(values, schema, schemas = list()) {
  # an empty text input posts `""`, and a query parameter with no value at
  # all parses to `NA_character_`.
  is_blank <- function(value) {
    if (is.null(value)) {
      return(TRUE)
    }

    if (!is.character(value) || length(value) != 1L) {
      return(FALSE)
    }

    is.na(value) || !nzchar(value)
  }

  values <- values[
    !vapply(X = values, FUN = is_blank, FUN.VALUE = logical(1))
  ]

  if (!length(values)) {
    return(NULL)
  }

  schema <- openapi_resolve_schema(schema, schemas)
  is_array <- identical(schema$type, "array")

  if (!is_array && length(values) == 1L) {
    return(openapi_convert(values[[1]], schema, schemas))
  }

  field <- lapply(
    X = unname(values),
    FUN = openapi_convert,
    schema = if (is_array) schema$items else schema,
    schemas = schemas
  )

  openapi_array(elements = field)
}

#' Rebuild Converted Occurrences as an Array
#'
#' Scalars of one type become a vector, which is what an R handler wants of
#' a multiple select. Anything else, file parts especially, stays a list. A
#' single element is marked `AsIs` so it reads as an array of one rather
#' than as a scalar.
#'
#' @param elements List /// Required. \cr
#'                 The converted occurrences of one field, unnamed.
#'
#' @return An array value.
#'
#' @examples
#' openapi_array(elements = list("a", "b"))
#'
#' openapi_array(elements = list("a"))
#'
#' # mixed types cannot be a vector
#' openapi_array(elements = list("a", 1L))
#'
#' @keywords internal
#' @noRd
openapi_array <- function(elements) {
  scalars <- vapply(
    X = elements,
    FUN = function(element) is.atomic(element) && length(element) == 1L,
    FUN.VALUE = logical(1)
  )
  types <- vapply(X = elements, FUN = typeof, FUN.VALUE = character(1))

  if (!all(scalars) || length(unique(types)) > 1L) {
    return(elements)
  }

  values <- unlist(elements, recursive = FALSE, use.names = FALSE)

  if (length(values) == 1L) {
    return(I(values))
  }

  values
}

#' Write a Field Back, Collapsing Its Occurrences
#'
#' The shaped value replaces the first occurrence and any others are
#' dropped, so `request$query[[name]]` reads what was validated instead of
#' the raw first string.
#'
#' @param x Named list /// Required. \cr
#'          A parsed query or path parameter list.
#'
#' @param name String /// Required. \cr
#'             The field name to write.
#'
#' @param value Object /// Required. \cr
#'              The field's value, from `openapi_field()`.
#'
#' @return `x`, with one entry for `name`.
#'
#' @examples
#' openapi_set_field(
#'   x = list(tag = "a", n = "1", tag = "b"),
#'   name = "tag",
#'   value = c("a", "b")
#' )
#'
#' @keywords internal
#' @noRd
openapi_set_field <- function(x, name, value) {
  at <- which(names(x) == name)
  x[[at[[1]]]] <- value

  if (length(at) == 1L) {
    return(x)
  }

  x[-at[-1L]]
}

#' The Response Sent for an Invalid Request
#'
#' What a request that does not match its documentation is answered with,
#' unless the app supplies its own with `app$openapi(on_invalid =)`.
#'
#' `400` rather than the more precise `422`, for a reason that is entirely
#' cosmetic. httpuv fills the status line's reason phrase from a table that
#' was never extended past the codes in RFC 2616, and returns the string
#' `"Dunno"` for anything it does not hold; `422` arrived in RFC 4918 and is
#' not in that table, so the response reaches the client as
#' `HTTP/1.1 422 Dunno`. See `getStatusDescription()` in
#' <https://github.com/rstudio/httpuv/blob/c758c542f9e3216264f332d7d9e6675adb79798a/src/webapplication.cpp#L32>.
#' Nothing can be done about it from here either: the response `list` httpuv
#' takes from R carries `status`, `headers`, and `body`, with no slot for a
#' reason phrase to override.
#'
#' On the merits `422` is the better answer, since the request parsed and only
#' its contents are wrong, and `400` is otherwise the answer for a request that
#' could not be understood at all. An app that would rather be precise than
#' tidy on the wire passes `app$openapi(on_invalid =)`: no client reads the
#' reason phrase, and HTTP/2 drops it altogether.
#'
#' @param req Request /// Required. \cr
#'            The [Request] that was checked.
#'
#' @param res Response /// Required. \cr
#'            The [Response] to send.
#'
#' @param details List /// Required. \cr
#'                The problems found, see `openapi_detail()`.
#'
#' @return A response.
#'
#' @keywords internal
#' @noRd
openapi_invalid_response <- function(req, res, details) {
  res$set_status(400L)$json(
    list(
      error = "Invalid request",
      details = details
    )
  )
}

#' A Single Problem With a Request
#'
#' The shape reported back to the client in the `400`. Kept as a constructor
#' rather than an inline `list()` so the three field names are spelled in one
#' place.
#'
#' @param location String /// Required. \cr
#'                 Where the problem is: `"body"`, `"query"`, or `"path"`.
#'
#' @param path String /// Required. \cr
#'             Path to the offending value, e.g. `"tags[2]"`.
#'
#' @param message String /// Required. \cr
#'                What is wrong with it, phrased to follow the path, e.g.
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
#' @param location String /// Required. \cr
#'                 Where the problems are: `"body"`, `"query"`, or `"path"`.
#'
#' @param problems List /// Required. \cr
#'                 The problems, each a `list(path, message)`.
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
#' resolved, accept anything. A multipart file part (`filename` in its names)
#' documented with `format: binary` or `byte` is accepted as-is.
#'
#' @param value Object /// Required. \cr
#'              The value to check.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The schema it is checked against.
#'
#' @param schemas Named list of OpenAPI schemas /// Optional. \cr
#'                The document's schemas, used to resolve references. \cr
#'                Defaults to `list()`.
#'
#' @param path String /// Optional. \cr
#'             Path to `value`, used in the messages. Empty at the top of a
#'             body, and grown by `openapi_child_path()` on the way down. \cr
#'             Defaults to `""`.
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

  # multipart file part documented as string + format binary/byte
  is_file_part <- is.list(value) &&
    "filename" %in% names(value) &&
    !is.null(schema$format) &&
    schema$format %in% c("binary", "byte")

  if (is_file_part) {
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
#' @param value Numeric /// Required. \cr
#'              The number to check.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The schema it is checked against.
#'
#' @param path String /// Required. \cr
#'             Path to `value`, used in the messages.
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
#' @param value String /// Required. \cr
#'              The string to check.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The schema it is checked against.
#'
#' @param path String /// Required. \cr
#'             Path to `value`, used in the messages.
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
#' @param value Array /// Required. \cr
#'              The array to check.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The schema it is checked against.
#'
#' @param schemas Named list of OpenAPI schemas /// Required. \cr
#'                The document's schemas, used to resolve references.
#'
#' @param path String /// Required. \cr
#'             Path to `value`, used in the messages.
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
#' @param value Named list /// Required. \cr
#'              The object to check.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The schema it is checked against.
#'
#' @param schemas Named list of OpenAPI schemas /// Required. \cr
#'                The document's schemas, used to resolve references.
#'
#' @param path String /// Required. \cr
#'             Path to `value`, used in the messages. Property names are
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
#' union of types, an unresolvable reference, a multipart file part, or any
#' other value that is not a single string.
#'
#' A conversion that cannot succeed is not an error either. The value is
#' returned as the string it was, and the type check that follows reports it:
#' `"abc"` documented as an integer is *must be an integer* whether the
#' conversion or the check is what noticed, and only one of them should say
#' so.
#'
#' @param value String /// Required. \cr
#'              The value to convert.
#'
#' @param schema OpenAPI schema /// Required. \cr
#'               The field's schema.
#'
#' @param schemas Named list of OpenAPI schemas /// Optional. \cr
#'                The document's schemas, used to resolve references. \cr
#'                Defaults to `list()`.
#'
#' @return The converted value, or `value` itself.
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
#' # not convertible: left for the type check to report
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

  value
}

#' Read a Query String Boolean
#'
#' Deliberately stricter than [as.logical()], which accepts `"T"` and `"yes"`
#' and quietly returns `NA` for anything else. Only the spellings a client
#' would actually send are accepted: JSON's `true`/`false`, R's
#' `TRUE`/`FALSE`, and `1`/`0`.
#'
#' @param x String /// Required. \cr
#'          The spelling to read.
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
#' @param schema OpenAPI schema /// Required. \cr
#'               The schema to resolve, possibly a bare reference.
#'
#' @param schemas Named list of OpenAPI schemas /// Required. \cr
#'                The document's schemas, keyed by name.
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
#' @param value Object /// Required. \cr
#'              The value to check.
#'
#' @param type String /// Required. \cr
#'             A JSON type, e.g. `"string"`.
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
#' @param type Character vector /// Required. \cr
#'             The JSON types to name.
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
#' @param x Array /// Required. \cr
#'          A parsed JSON array.
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
#' @param x Object /// Required. \cr
#'          A parsed JSON value.
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
#' @param path String /// Required. \cr
#'             Path to the parent, possibly `""`.
#'
#' @param name String /// Required. \cr
#'             Name of the property.
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
