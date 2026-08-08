#' OpenAPI Schemas
#'
#' Build [OpenAPI Schema Objects](https://spec.openapis.org/oas/v3.1.0#schema-object)
#' used to describe request bodies, responses, and parameters.
#'
#' A schema is a plain R object: define it once and reuse it everywhere.
#'
#' @section Keywords:
#'
#' Every constructor passes `...` straight into the schema, so any
#' [JSON Schema](https://json-schema.org/understanding-json-schema/reference)
#' keyword can be used, spelled exactly as in the specification:
#'
#' ```r
#' openapi_schema_string(format = "email", maxLength = 100L)
#' openapi_schema_integer(minimum = 1L, example = 3L)
#' openapi_schema_string(enum = c("todo", "doing", "done"))
#' ```
#'
#' Keywords that are not part of the specification are ignored by tools
#' reading the document, so unknown names emit a warning. Extension
#' keywords (`x-*`) are always allowed.
#'
#' @param type The schema type, e.g. `"string"`. Only needed with
#' `openapi_schema()`; the other constructors set it for you.
#' @param items An OpenAPI schema describing the type of every element in
#' the array.
#' @param properties A named `list` of OpenAPI schemas describing the
#' properties of an object.
#' @param required Properties of the object that must be present. Either a
#' character vector of property names, or `TRUE` to mark every declared
#' property as required. Note that with `TRUE` a property added later
#' silently becomes required too.
#' @param name Name under which the schema is placed in the document's
#' `components`.
#' @param schema The OpenAPI schema to name. If `NULL`, the result is a bare
#' reference to a schema named elsewhere.
#' @param ... Additional JSON Schema keywords, see the Keywords section.
#'
#' @return An object of class `ambiorix_openapi_schema`; a `list` that mirrors
#' an OpenAPI schema object.
#'
#' @examples
#' openapi_schema_string()
#'
#' openapi_schema_object(
#'   properties = list(
#'     id = openapi_schema_integer(),
#'     name = openapi_schema_string(minLength = 1L),
#'     tags = openapi_schema_array(openapi_schema_string())
#'   ),
#'   required = c("id", "name")
#' )
#'
#' # name a schema to reuse it: it is placed in `components` and
#' # referenced with `$ref` wherever it is used
#' openapi_schema_ref(
#'   "User",
#'   openapi_schema_object(
#'     properties = list(id = openapi_schema_integer()),
#'     required = "id"
#'   )
#' )
#'
#' # a bare reference to a schema named elsewhere
#' openapi_schema_ref("User")
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
openapi_schema <- function(type = NULL, ...) {
  keywords <- list(...)

  if (length(keywords)) {
    assert_that(has_names(keywords))
    warn_unknown_keywords(names(keywords))
  }

  if (!is.null(type)) {
    assert_that(is.character(type))
    keywords <- c(list(type = type), keywords)
  }

  new_openapi_schema(keywords)
}

#' @rdname openapi-schemas
#' @export
openapi_schema_string <- function(...) {
  openapi_schema(type = "string", ...)
}

#' @rdname openapi-schemas
#' @export
openapi_schema_integer <- function(...) {
  openapi_schema(type = "integer", ...)
}

#' @rdname openapi-schemas
#' @export
openapi_schema_number <- function(...) {
  openapi_schema(type = "number", ...)
}

#' @rdname openapi-schemas
#' @export
openapi_schema_boolean <- function(...) {
  openapi_schema(type = "boolean", ...)
}

#' @rdname openapi-schemas
#' @export
openapi_schema_array <- function(items, ...) {
  assert_that(not_missing(items))
  assert_that(is_openapi_schema(items))

  openapi_schema(type = "array", items = items, ...)
}

#' @rdname openapi-schemas
#' @export
openapi_schema_object <- function(properties = list(), required = NULL, ...) {
  assert_that(is.list(properties))

  if (length(properties)) {
    assert_that(has_names(properties))
    assert_that(is_openapi_schema_list(properties))
  }

  keywords <- list(...)
  required <- openapi_required_names(
    required = required,
    properties = names(properties),
    # composition keywords may introduce properties that are not
    # declared locally, in which case we cannot check the names
    check = !any(names(keywords) %in% OPENAPI_COMPOSITION_KEYWORDS)
  )

  out <- list(type = "object")

  # an object with no properties is a valid, if permissive, schema:
  # only declare `properties` when there are some
  if (length(properties)) {
    out$properties <- properties
  }

  if (length(required)) {
    out$required <- required
  }

  if (length(keywords)) {
    assert_that(has_names(keywords))
    warn_unknown_keywords(names(keywords))
    out[names(keywords)] <- keywords
  }

  new_openapi_schema(out)
}

#' @rdname openapi-schemas
#' @export
openapi_schema_ref <- function(name, schema = NULL) {
  assert_that(not_missing(name))
  assert_that(is_string(name))
  assert_that(is_openapi_component_name(name))

  if (is.null(schema)) {
    # a bare reference: the schema itself is declared elsewhere
    schema <- new_openapi_schema(list())
  }

  assert_that(is_openapi_schema(schema))

  # the name is an attribute rather than a keyword so it can never
  # leak into the document
  attr(schema, "openapi_name") <- name
  schema
}

#' @export
print.ambiorix_openapi_schema <- function(x, ...) {
  name <- attr(x, "openapi_name")

  if (!is.null(name) && !length(x)) {
    cli::cli_alert_info("A reference to the OpenAPI schema {.val {name}}")
    return(invisible(x))
  }

  type <- x$type %||% "any"

  if (is.null(name)) {
    cli::cli_alert_info("An OpenAPI schema of type {.val {type}}")
    return(invisible(x))
  }

  cli::cli_alert_info(
    "An OpenAPI schema of type {.val {type}}, named {.val {name}}"
  )
  invisible(x)
}

#' Resolve the Required Property Names of an Object Schema
#'
#' @param required `TRUE`, or a character vector of property names.
#' @param properties Names of the declared properties.
#' @param check Whether to check `required` against `properties`.
#'
#' @return Character vector, possibly empty.
#'
#' @keywords internal
#' @noRd
openapi_required_names <- function(required, properties, check = TRUE) {
  if (is.null(required)) {
    return(character())
  }

  if (is.logical(required)) {
    assert_that(is_flag(required))

    if (!required) {
      return(character())
    }

    return(properties %||% character())
  }

  assert_that(is.character(required))
  assert_that(!anyNA(required))

  required <- unique(required)

  if (!check) {
    return(required)
  }

  unknown <- setdiff(required, properties)

  if (length(unknown)) {
    stop(
      "`required` names undeclared ",
      if (length(unknown) > 1L) "properties: " else "property: ",
      paste0("`", unknown, "`", collapse = ", "),
      call. = FALSE
    )
  }

  required
}

#' Warn About Keywords That Are Not in the Specification
#'
#' Unknown keywords are ignored by anything reading the document, so a
#' typo would otherwise go unnoticed.
#'
#' @param x Character vector of keyword names.
#'
#' @keywords internal
#' @noRd
warn_unknown_keywords <- function(x) {
  unknown <- x[!x %in% OPENAPI_KEYWORDS & !grepl("^x-", x)]

  if (!length(unknown)) {
    return(invisible(NULL))
  }

  cli::cli_alert_warning(
    paste(
      "Unknown OpenAPI schema keyword{?s}: {.val {unknown}}:",
      "ignored by anything reading the document."
    )
  )

  invisible(NULL)
}

#' Keywords Whose Value Is an Array
#'
#' These are wrapped with [as.list()] when rendered so that a single
#' element still serialises to a JSON array.
#'
#' @keywords internal
#' @noRd
OPENAPI_ARRAY_KEYWORDS <- c(
  "allOf",
  "anyOf",
  "enum",
  "examples",
  "oneOf",
  "prefixItems",
  "required"
)

#' Keywords Introducing Properties That Are Not Declared Locally
#'
#' @keywords internal
#' @noRd
OPENAPI_COMPOSITION_KEYWORDS <- c("allOf", "anyOf", "oneOf", "not", "$ref")

#' Known JSON Schema & OpenAPI Keywords
#'
#' Used to warn about typos, see `warn_unknown_keywords()`.
#'
#' @keywords internal
#' @noRd
OPENAPI_KEYWORDS <- c(
  # core
  "$ref",
  "$comment",
  "type",
  "enum",
  "const",
  "default",
  "format",
  "title",
  "description",
  "example",
  "examples",
  "deprecated",
  "readOnly",
  "writeOnly",
  # composition & conditionals
  "allOf",
  "anyOf",
  "oneOf",
  "not",
  "if",
  "then",
  "else",
  # objects
  "properties",
  "patternProperties",
  "additionalProperties",
  "required",
  "propertyNames",
  "minProperties",
  "maxProperties",
  "dependentRequired",
  "dependentSchemas",
  # arrays
  "items",
  "prefixItems",
  "contains",
  "minItems",
  "maxItems",
  "minContains",
  "maxContains",
  "uniqueItems",
  # numbers
  "minimum",
  "maximum",
  "exclusiveMinimum",
  "exclusiveMaximum",
  "multipleOf",
  # strings
  "minLength",
  "maxLength",
  "pattern",
  "contentEncoding",
  "contentMediaType",
  # openapi extras
  "discriminator",
  "externalDocs",
  "xml"
)
