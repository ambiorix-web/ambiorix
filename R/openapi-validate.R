#' Validate a Request Against Its Documentation
#'
#' Checks the request body and the query and path parameters against the
#' schemas the route documents. Parameters arrive as strings, so those
#' documented with another type are converted before they are checked, and the
#' converted value is written back onto the request.
#'
#' Header and cookie parameters are not checked: ambiorix does not know
#' which of them a handler cares about.
#'
#' @param request The [Request].
#' @param docs The route's docs, see [openapi_docs()].
#' @param schemas Named list of the document's schemas, used to resolve
#' references.
#'
#' @return A `list` of problems, empty when the request is valid.
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
    value <- if (is_query) {
      request$query[[param$name]]
    } else {
      request$params[[param$name]]
    }

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
    } else {
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
  if (!grepl("json", docs$request_body$content_type, fixed = TRUE)) {
    return(details)
  }

  body <- openapi_parse_body(request)

  if (is.null(body)) {
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
      openapi_validate(body, docs$request_body$schema, schemas)
    )
  )
}

#' Parse a Request Body for Validation
#'
#' Parsed with options that keep the shape of the JSON: a one element array
#' would otherwise be indistinguishable from a scalar, and an array of objects
#' would become a data frame.
#'
#' This is deliberately not shared with [parse_json()]: handlers must keep
#' seeing exactly what they see without validation.
#'
#' @param request The [Request].
#'
#' @return The parsed body, or `NULL` when there is none.
#'
#' @keywords internal
#' @noRd
openapi_parse_body <- function(request) {
  on.exit(request$rook.input$rewind())
  body <- request$rook.input$read()

  if (identical(body, raw())) {
    return(NULL)
  }

  parsed <- tryCatch(
    yyjsonr::read_json_raw(body, opts = openapi_parse_opts()),
    error = function(error) error
  )

  if (inherits(parsed, "error")) {
    return(NULL)
  }

  parsed
}

#' @keywords internal
#' @noRd
openapi_parse_opts <- function() {
  yyjsonr::opts_read_json(
    length1_array_asis = TRUE,
    arr_of_objs_to_df = FALSE,
    obj_of_arrs_to_df = FALSE,
    arr_of_arrs_to_matrix = FALSE
  )
}

#' A Single Problem With a Request
#'
#' @param location Where the problem is: `"body"`, `"query"`, or `"path"`.
#' @param path Path to the offending value, e.g. `"tags[2]"`.
#' @param message What is wrong with it.
#'
#' @keywords internal
#' @noRd
openapi_detail <- function(location, path, message) {
  list(location = location, path = path, message = message)
}

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
#' @param value Value to check.
#' @param schema An OpenAPI schema.
#' @param schemas Named list of schemas, used to resolve references.
#' @param path Path to `value`, used in the messages.
#'
#' @return A `list` of `list(path, message)`, empty when the value is valid.
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
#' Query and path parameters always arrive as strings.
#'
#' @param value The value, a string.
#' @param schema The parameter's schema.
#' @param schemas Named list of schemas, used to resolve references.
#'
#' @return The converted value, or an object of class
#' `ambiorix_openapi_conversion_error`.
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
#' @param schema An OpenAPI schema, possibly a bare reference.
#' @param schemas Named list of schemas.
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
#' The parser cannot always tell a JSON scalar from a one element array, so
#' bodies are parsed with `length1_array_asis`: an array of one is marked
#' `AsIs` and a scalar is not.
#'
#' @param value Value to check.
#' @param type A JSON type, e.g. `"string"`.
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
#' @param x A parsed JSON array.
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

#' @keywords internal
#' @noRd
openapi_scalar <- function(x) {
  if (inherits(x, "AsIs")) {
    return(unclass(x))
  }

  x
}

#' @keywords internal
#' @noRd
openapi_child_path <- function(path, name) {
  if (!nzchar(path)) {
    return(name)
  }

  paste0(path, ".", name)
}
