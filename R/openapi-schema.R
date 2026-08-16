#' OpenAPI Schemas
#'
#' Build [OpenAPI Schema Objects](https://spec.openapis.org/oas/v3.1.0#schema-object)
#' used to describe request bodies, responses, and parameters.
#'
#' A schema is a plain R object: define it once and reuse it everywhere.
#'
#' @section Constructors:
#'
#' - `openapi_schema_string()`, `openapi_schema_integer()`,
#'   `openapi_schema_number()`, `openapi_schema_boolean()`: a scalar of that
#'   type. These are thin wrappers that set `type` and pass everything else
#'   through.
#' - `openapi_schema_array(items)`: an array in which every element matches
#'   `items`, itself a schema.
#' - `openapi_schema_object(properties, required)`: an object with the named
#'   `properties`, each a schema. An object with no properties is valid, if
#'   permissive: it accepts any object.
#' - `openapi_schema_ref(name, schema)`: names a schema. The schema is emitted
#'   once, under the document's `components`, and every use of it becomes a
#'   `$ref` pointing there. Called with only a `name`, it is instead a bare
#'   reference to a schema defined elsewhere in the document.
#' - `openapi_schema()`: the escape hatch, for a type the wrappers do not
#'   cover (e.g. `"null"`), several types at once, or a schema built purely
#'   from composition keywords.
#'
#' Two different schemas sharing one `name` is an error, reported when the app
#' starts rather than when the document is served.
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
#' @param type String or Character vector /// Optional. \cr
#'             The schema type, e.g. `"string"`. Only needed with
#'             `openapi_schema()`; the other constructors set it for you. A
#'             vector documents a value that may be any of several types, e.g.
#'             `c("string", "null")`. \cr
#'             Defaults to `NULL`, a schema that accepts any type.
#'
#' @param items OpenAPI schema /// Required. \cr
#'              A schema describing the type of every element in the array.
#'
#' @param properties Named list of OpenAPI schemas /// Optional. \cr
#'                   The properties of the object, named after the property. \cr
#'                   Defaults to `list()`, an object that accepts anything.
#'
#' @param required Character vector or Logical /// Optional. \cr
#'                 Properties of the object that must be present. Either a
#'                 character vector of property names, which must all be
#'                 declared in `properties`, or `TRUE` to mark every declared
#'                 property as required. \cr
#'                 Defaults to `NULL`. Note that with `TRUE` a property added
#'                 later silently becomes required too.
#'
#' @param name String /// Required. \cr
#'             Name under which the schema is placed in the document's
#'             `components`. May contain only letters, digits, `.`, `_`, and
#'             `-`.
#'
#' @param schema OpenAPI schema /// Optional. \cr
#'               The schema to name. \cr
#'               Defaults to `NULL`, which makes the result a bare reference to
#'               a schema named elsewhere in the document.
#'
#' @param ... Key=Value pairs /// Optional. \cr
#'            Additional JSON Schema keywords, see the Keywords section.
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
#' # the escape hatch: a type the wrappers do not cover, and a value that
#' # may be a string or absent
#' openapi_schema(type = c("string", "null"))
#'
#' @seealso [openapi_param()], [openapi_response()], [openapi_request_body()],
#' and [openapi_docs()], which schemas are passed to.
#'
#' @name openapi-schemas
NULL

#' Give a List the OpenAPI Schema Class
#'
#' The single place the class is attached, so every schema is built the same
#' way. Deliberately does no checking: the exported constructors validate
#' their arguments before calling this.
#'
#' @param x Named list /// Required. \cr
#'          The JSON Schema keywords.
#'
#' @return An object of class `ambiorix_openapi_schema`.
#'
#' @examples
#' new_openapi_schema(list(type = "string", maxLength = 10L))
#'
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

#' @rdname openapi-print
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
#' Turns the two forms `openapi_schema_object(required = )` accepts into the
#' character vector the specification wants, and rejects names that no
#' declared property matches: a typo there would silently require a property
#' that can never be present.
#'
#' @param required Logical or Character vector /// Required. \cr
#'                 `TRUE`, `FALSE`, `NULL`, or the property names that are
#'                 required.
#'
#' @param properties Character vector /// Required. \cr
#'                   Names of the declared properties.
#'
#' @param check Logical /// Required. \cr
#'              Whether to check `required` against `properties`. `FALSE`
#'              when a composition keyword may introduce properties that are
#'              not declared locally, in which case the names cannot be
#'              known.
#'
#' @return Character vector, possibly empty. Stops when `check` is `TRUE` and
#'         `required` names a property that was not declared.
#'
#' @examples
#' openapi_required_names(c("id", "name"), properties = c("id", "name", "age"))
#'
#' # `TRUE` means "every declared property"
#' openapi_required_names(TRUE, properties = c("id", "name"))
#'
#' openapi_required_names(NULL, properties = "id")
#'
#' # a name no property matches is an error, unless checking is off
#' try(openapi_required_names("nmae", properties = "name"))
#'
#' openapi_required_names("nmae", properties = "name", check = FALSE)
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
#' Extension keywords (`x-*`) are always allowed, since the specification
#' reserves them for exactly this.
#'
#' @param x Character vector /// Required. \cr
#'          The keyword names to check.
#'
#' @return `NULL`, invisibly. Called for the warning it emits.
#'
#' @examples
#' warn_unknown_keywords(c("type", "format"))
#'
#' # warns: `maxLenght` is a typo, and would be silently ignored
#' warn_unknown_keywords("maxLenght")
#'
#' # extension keywords never warn
#' warn_unknown_keywords("x-internal")
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
#' These are wrapped with [as.list()] by `openapi_render_keywords()` so that a
#' single element still serialises to a JSON array: the serialiser unboxes
#' length one vectors, so `required = "id"` would otherwise emit `"id"` where
#' `["id"]` is required.
#'
#' @format Character vector of keyword names.
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
#' When an object schema uses one of these, `openapi_schema_object()` cannot
#' know the full set of property names, so it stops checking `required`
#' against them; see `openapi_required_names()`.
#'
#' @format Character vector of keyword names.
#'
#' @keywords internal
#' @noRd
OPENAPI_COMPOSITION_KEYWORDS <- c("allOf", "anyOf", "oneOf", "not", "$ref")

#' Known JSON Schema & OpenAPI Keywords
#'
#' Used to warn about typos, see `warn_unknown_keywords()`. Not a validation
#' list: anything here is passed through untouched, whether or not it makes
#' sense for the schema's type.
#'
#' @format Character vector of keyword names.
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
