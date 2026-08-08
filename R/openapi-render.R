#' Render an Object to Its OpenAPI Representation
#'
#' The generic every OpenAPI object implements: it turns the object into the
#' plain `list` that is serialised into the document.
#'
#' `ctx` is an accumulator created by `new_openapi_ctx()`: it collects the
#' named schemas met along the way (which become the document's `components`)
#' together with any diagnostics. It lives and dies within a single
#' `build_openapi()` call.
#'
#' @param x Object to render.
#' @param ctx Build context, see `new_openapi_ctx()`.
#' @param ... Passed to methods.
#'
#' @return A `list`.
#'
#' @keywords internal
#' @noRd
as_openapi <- function(x, ctx, ...) {
  UseMethod("as_openapi")
}

#' @export
as_openapi.default <- function(x, ctx, ...) {
  stop(
    "Cannot render an object of class `",
    paste0(class(x), collapse = "/"),
    "` into an OpenAPI document",
    call. = FALSE
  )
}

#' Build Context
#'
#' Accumulates the named schemas and the diagnostics of a single document
#' build. An environment so that nested nodes can register themselves as they
#' are rendered.
#'
#' @return An environment.
#'
#' @keywords internal
#' @noRd
new_openapi_ctx <- function() {
  ctx <- new.env(parent = emptyenv())
  ctx$schemas <- named_list()
  ctx$refs <- character()
  ctx$notes <- character()
  ctx$errors <- character()
  ctx
}

#' Path of a Schema in the Document's Components
#'
#' @param name Name of the schema.
#'
#' @keywords internal
#' @noRd
openapi_ref_path <- function(name) {
  paste0("#/components/schemas/", name)
}

#' @export
as_openapi.ambiorix_openapi_schema <- function(x, ctx, ...) {
  name <- attr(x, "openapi_name")

  if (is.null(name)) {
    return(openapi_render_keywords(x, ctx))
  }

  ctx$refs <- unique(c(ctx$refs, name))

  # a bare reference: nothing to register, the schema is declared elsewhere
  if (!length(unclass(x))) {
    return(list(`$ref` = openapi_ref_path(name)))
  }

  body <- openapi_render_keywords(x, ctx)
  previous <- ctx$schemas[[name]]

  if (is.null(previous)) {
    ctx$schemas[[name]] <- body
  } else if (!identical(previous, body)) {
    ctx$errors <- c(
      ctx$errors,
      sprintf(
        paste(
          "Two different schemas are named %s.",
          "Schema names must be unique: rename one of them."
        ),
        name
      )
    )
  }

  list(`$ref` = openapi_ref_path(name))
}

#' Render the Keywords of a Schema
#'
#' Keywords whose value is an array in the specification are wrapped with
#' [as.list()]: the serialiser unboxes length one vectors, so `"title"` would
#' otherwise be emitted where `["title"]` is required.
#'
#' @param x An OpenAPI schema.
#' @param ctx Build context.
#'
#' @keywords internal
#' @noRd
openapi_render_keywords <- function(x, ctx) {
  out <- list()

  for (keyword in names(x)) {
    value <- openapi_render_value(x[[keyword]], ctx)

    if (is.null(value)) {
      next
    }

    if (keyword %in% OPENAPI_ARRAY_KEYWORDS) {
      value <- as.list(value)
    }

    out[[keyword]] <- value
  }

  out
}

#' Render Any Value Met Inside a Schema
#'
#' Walks arbitrarily nested values so that schemas held by *any* keyword are
#' rendered, not just those held by `properties` and `items`: `oneOf`,
#' `additionalProperties`, `not`, `patternProperties` and friends all carry
#' schemas too.
#'
#' @param x Value to render.
#' @param ctx Build context.
#'
#' @keywords internal
#' @noRd
openapi_render_value <- function(x, ctx) {
  if (is_openapi_schema(x)) {
    return(as_openapi(x, ctx))
  }

  if (!is.list(x)) {
    return(x)
  }

  lapply(X = x, FUN = openapi_render_value, ctx = ctx)
}

#' @export
as_openapi.ambiorix_openapi_parameter <- function(x, ctx, ...) {
  out <- list(
    name = x$name,
    `in` = x$location,
    required = x$required
  )

  if (!is.null(x$description)) {
    out$description <- x$description
  }

  out$schema <- as_openapi(x$schema, ctx)

  if (length(x$extra)) {
    out[names(x$extra)] <- openapi_render_value(x$extra, ctx)
  }

  out
}

#' @export
as_openapi.ambiorix_openapi_request_body <- function(x, ctx, ...) {
  content <- list()
  content[[x$content_type]] <- list(schema = as_openapi(x$schema, ctx))

  out <- list(
    required = x$required,
    content = content
  )

  if (!is.null(x$description)) {
    out$description <- x$description
  }

  out
}

#' @export
as_openapi.ambiorix_openapi_response <- function(x, ctx, ...) {
  out <- list(description = x$description)

  if (!is.null(x$schema)) {
    content <- list()
    content[[x$content_type]] <- list(schema = as_openapi(x$schema, ctx))
    out$content <- content
  }

  if (!is.null(x$headers)) {
    out$headers <- lapply(
      X = x$headers,
      FUN = function(schema) list(schema = as_openapi(schema, ctx))
    )
  }

  out
}

#' Render a Route's Documentation Into an Operation Object
#'
#' @param x The route's docs.
#' @param ctx Build context.
#' @param path The route's full path, used to derive path parameters.
#'
#' @export
as_openapi.ambiorix_openapi_docs <- function(x, ctx, path = "", ...) {
  operation <- list()

  if (!is.null(x$summary)) {
    operation$summary <- x$summary
  }

  if (!is.null(x$description)) {
    operation$description <- x$description
  }

  if (!is.null(x$operation_id)) {
    operation$operationId <- x$operation_id
  }

  if (!is.null(x$tags)) {
    operation$tags <- as.list(x$tags)
  }

  if (isTRUE(x$deprecated)) {
    operation$deprecated <- TRUE
  }

  if (!is.null(x$security)) {
    operation$security <- openapi_render_security(x$security)
  }

  parameters <- openapi_render_parameters(x$parameters, ctx, path)

  if (length(parameters)) {
    operation$parameters <- parameters
  }

  if (!is.null(x$request_body)) {
    operation$requestBody <- as_openapi(x$request_body, ctx)
  }

  responses <- list()

  for (response in x$responses) {
    responses[[response$status]] <- as_openapi(response, ctx)
  }

  # the OpenAPI specification requires at least one response
  if (!length(responses)) {
    responses[["default"]] <- list(description = "Default response")
  }

  operation$responses <- responses

  if (length(x$extra)) {
    operation[names(x$extra)] <- openapi_render_value(x$extra, ctx)
  }

  operation
}

#' Merge Declared and Automatic Path Parameters
#'
#' Path parameters are derived from the route's `:param` tokens; a declared
#' parameter with `location = "path"` overrides the default for its token.
#'
#' @param parameters Declared parameters.
#' @param ctx Build context.
#' @param path The route's full path.
#'
#' @keywords internal
#' @noRd
openapi_render_parameters <- function(parameters, ctx, path) {
  overrides <- list()
  others <- list()

  for (param in parameters) {
    if (identical(param$location, "path")) {
      overrides[[param$name]] <- param
      next
    }

    others <- append(others, list(param))
  }

  out <- list()

  for (name in openapi_path_params(path)) {
    override <- overrides[[name]]
    overrides[[name]] <- NULL

    if (!is.null(override)) {
      out <- append(out, list(as_openapi(override, ctx)))
      next
    }

    out <- append(
      out,
      list(
        list(
          name = name,
          `in` = "path",
          required = TRUE,
          schema = list(type = "string")
        )
      )
    )
  }

  # path parameters that match no token in the route are dropped
  if (length(overrides)) {
    ctx$notes <- c(
      ctx$notes,
      sprintf(
        "Ignoring path parameter(s) %s for path `%s`: no matching `:param` token in the route.",
        paste0("`", names(overrides), "`", collapse = ", "),
        path
      )
    )
  }

  for (param in others) {
    out <- append(out, list(as_openapi(param, ctx)))
  }

  out
}

#' Convert an Ambiorix Path to an OpenAPI Path
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

#' Path Parameters of a Route
#'
#' Derived from the path itself rather than from `Route$params`, which is not
#' populated when a custom path to pattern converter is in use.
#'
#' @param path A route path.
#'
#' @return Character vector, possibly empty.
#'
#' @keywords internal
#' @noRd
openapi_path_params <- function(path) {
  matches <- regmatches(path, gregexpr(":([^/]+)", path))[[1]]
  sub("^:", "", matches)
}

#' Render a Security Requirement
#'
#' A character vector names the schemes that must *all* be satisfied; a list
#' is passed through as-is, for schemes that take scopes.
#'
#' @param security Character vector or `list`.
#'
#' @keywords internal
#' @noRd
openapi_render_security <- function(security) {
  if (is.list(security)) {
    return(security)
  }

  requirement <- list()

  for (scheme in security) {
    # an empty scope array
    requirement[[scheme]] <- list()
  }

  list(requirement)
}

#' Render the Document's Servers
#'
#' @param servers Character vector of URLs, or a `list` of server objects.
#'
#' @keywords internal
#' @noRd
openapi_render_servers <- function(servers) {
  if (is.character(servers)) {
    return(lapply(X = servers, FUN = function(url) list(url = url)))
  }

  servers
}

#' Render the Document's Tags
#'
#' A named character vector maps tag names to their descriptions.
#'
#' @param tags Character vector, optionally named, or a `list` of tag objects.
#'
#' @keywords internal
#' @noRd
openapi_render_tags <- function(tags) {
  if (!is.character(tags)) {
    return(tags)
  }

  names <- names(tags)

  lapply(
    X = seq_along(tags),
    FUN = function(i) {
      # unnamed: the value is the tag name and there is no description
      if (is.null(names) || !nzchar(names[[i]])) {
        return(list(name = tags[[i]]))
      }

      list(name = names[[i]], description = tags[[i]])
    }
  )
}

#' Build an OpenAPI Document From a List of Routes
#'
#' Diagnostics are emitted here rather than when the document is served: this
#' runs at startup, where the developer will see them.
#'
#' @param routes A list of routes as stored in `Routing`'s private `.routes`.
#' @param doc A list of document level fields: `info`, `servers`, `tags`,
#' `security_schemes`, and `security`.
#'
#' @return A list representing an OpenAPI 3.1.0 document.
#'
#' @keywords internal
#' @noRd
build_openapi <- function(routes, doc = list()) {
  info <- doc$info %||% list()
  info$title <- info$title %||% "API"
  info$version <- info$version %||% "1.0.0"

  ctx <- new_openapi_ctx()

  # named so an empty `paths` serialises to `{}`, not `[]`
  paths <- named_list()
  operation_ids <- character(0)

  for (route in routes) {
    if (is.null(route$docs) || !is_openapi_docs(route$docs)) {
      next
    }

    full_path <- paste0(route$route$basepath, route$path)
    oapi_path <- openapi_path(full_path)
    operation <- as_openapi(route$docs, ctx, path = full_path)

    if (is.null(paths[[oapi_path]])) {
      paths[[oapi_path]] <- list()
    }

    for (method in route$method) {
      method_operation <- operation

      # `operationId` must be unique across the document, so a route
      # answering several methods needs one identifier per method
      if (!is.null(operation$operationId) && length(route$method) > 1L) {
        method_operation$operationId <- paste0(
          operation$operationId,
          "_",
          tolower(method)
        )
      }

      if (!is.null(method_operation$operationId)) {
        operation_ids <- c(operation_ids, method_operation$operationId)
      }

      paths[[oapi_path]][[tolower(method)]] <- method_operation
    }
  }

  duplicated_ids <- unique(operation_ids[duplicated(operation_ids)])

  if (length(duplicated_ids)) {
    ctx$errors <- c(
      ctx$errors,
      sprintf(
        "Duplicated `operation_id`: %s. They must be unique across the document.",
        paste0("`", duplicated_ids, "`", collapse = ", ")
      )
    )
  }

  dangling <- setdiff(ctx$refs, names(ctx$schemas))

  if (length(dangling)) {
    ctx$notes <- c(
      ctx$notes,
      sprintf(
        "Reference(s) to undefined schema(s): %s.",
        paste0("`", dangling, "`", collapse = ", ")
      )
    )
  }

  openapi_report(ctx)

  out <- list(
    openapi = "3.1.0",
    info = info
  )

  if (!is.null(doc$servers)) {
    out$servers <- openapi_render_servers(doc$servers)
  }

  if (!is.null(doc$tags)) {
    out$tags <- openapi_render_tags(doc$tags)
  }

  if (!is.null(doc$security)) {
    out$security <- openapi_render_security(doc$security)
  }

  out$paths <- paths

  components <- named_list()

  if (!is.null(doc$security_schemes)) {
    components$securitySchemes <- doc$security_schemes
  }

  if (length(ctx$schemas)) {
    components$schemas <- ctx$schemas
  }

  if (length(components)) {
    out$components <- components
  }

  out
}

#' Emit the Diagnostics Collected While Building a Document
#'
#' @param ctx Build context.
#'
#' @keywords internal
#' @noRd
openapi_report <- function(ctx) {
  for (note in ctx$notes) {
    cli::cli_alert_warning(note)
  }

  if (!length(ctx$errors)) {
    return(invisible(NULL))
  }

  stop(
    "Cannot build the OpenAPI document:\n",
    paste0("* ", ctx$errors, collapse = "\n"),
    call. = FALSE
  )
}

#' Named Schemas of a Document, Keyed by Name
#'
#' Retains the schema objects themselves, which request validation needs to
#' resolve `$ref`s.
#'
#' @param routes A list of routes.
#'
#' @return A named `list` of OpenAPI schemas.
#'
#' @keywords internal
#' @noRd
openapi_named_schemas <- function(routes) {
  out <- named_list()

  collect <- function(x) {
    if (is_openapi_schema(x)) {
      name <- attr(x, "openapi_name")

      if (!is.null(name) && length(unclass(x)) && is.null(out[[name]])) {
        out[[name]] <<- x
      }
    }

    if (!is.list(x)) {
      return(invisible(NULL))
    }

    for (element in x) {
      collect(element)
    }

    invisible(NULL)
  }

  for (route in routes) {
    if (is.null(route$docs) || !is_openapi_docs(route$docs)) {
      next
    }

    collect(route$docs)
  }

  out
}
