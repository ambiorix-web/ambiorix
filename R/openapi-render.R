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

#' Refuse to Render an Unknown Object
#'
#' The fallback method. Reached when something that is not an OpenAPI object
#' finds its way into a document, e.g. a bare `list()` passed where
#' `openapi_response()` was expected.
#'
#' @param x Object to render.
#' @param ctx Build context.
#' @param ... Unused.
#'
#' @return Never returns; always stops.
#'
#' @examples
#' try(as_openapi(list(a = 1), new_openapi_ctx()))
#'
#' @keywords internal
#' @noRd
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
#' @return An environment with four bindings: `schemas`, the named schemas met
#'         so far, keyed by name; `refs`, every name referenced, used to spot
#'         references to schemas that are never defined; `notes`, warnings; and
#'         `errors`, which abort the build.
#'
#' @examples
#' ctx <- new_openapi_ctx()
#'
#' # rendering a named schema registers it and yields a reference
#' as_openapi(openapi_schema_ref("User", openapi_schema_object()), ctx)
#'
#' names(ctx$schemas)
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
#' The JSON pointer a `$ref` uses to reach a named schema. Every named schema
#' is emitted under `components/schemas`, so the pointer is derived from the
#' name alone.
#'
#' @param name Name of the schema.
#'
#' @return A single character string.
#'
#' @examples
#' openapi_ref_path("User")
#'
#' @keywords internal
#' @noRd
openapi_ref_path <- function(name) {
  paste0("#/components/schemas/", name)
}

#' Render a Schema, Registering It If It Is Named
#'
#' An anonymous schema is rendered inline. A named one is registered on `ctx`
#' and replaced by a `$ref`, so it appears once in the document however many
#' routes use it. A bare reference registers nothing: the schema is defined
#' wherever it was named with a body.
#'
#' Two different bodies under one name is an error rather than a silent
#' last-one-wins, and is recorded on `ctx` so the build can report every
#' problem at once instead of stopping at the first.
#'
#' @param x An OpenAPI schema.
#' @param ctx Build context.
#' @param ... Unused.
#'
#' @return A `list`: the rendered keywords, or `list("$ref" = ...)`.
#'
#' @examples
#' ctx <- new_openapi_ctx()
#'
#' # anonymous: rendered inline
#' as_openapi(openapi_schema_string(maxLength = 10L), ctx)
#'
#' # named: registered on `ctx`, replaced by a reference
#' as_openapi(openapi_schema_ref("User", openapi_schema_object()), ctx)
#'
#' ctx$schemas
#'
#' @keywords internal
#' @noRd
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
#' Keywords whose value is `NULL` are dropped rather than emitted as JSON
#' `null`, which the specification would read as a meaningful value.
#'
#' @param x An OpenAPI schema.
#' @param ctx Build context.
#'
#' @return A `list` of rendered keywords.
#'
#' @examples
#' ctx <- new_openapi_ctx()
#'
#' # `required` is wrapped, so a single name still emits `["id"]`
#' openapi_render_keywords(
#'   openapi_schema_object(
#'     properties = list(id = openapi_schema_integer()),
#'     required = "id"
#'   ),
#'   ctx
#' )
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
#' @return `x` with every schema inside it replaced by its rendered form.
#'         Values that are not schemas are returned untouched.
#'
#' @examples
#' ctx <- new_openapi_ctx()
#'
#' # the schema nested under `oneOf` is rendered, not just copied
#' openapi_render_value(
#'   list(oneOf = list(openapi_schema_string(), openapi_schema_integer())),
#'   ctx
#' )
#'
#' # anything that is not a schema passes straight through
#' openapi_render_value(list(example = "hello"), ctx)
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

#' Render a Parameter Into a Parameter Object
#'
#' `location` becomes `in`, which cannot be an R argument name because it is
#' reserved.
#'
#' @param x An OpenAPI parameter.
#' @param ctx Build context.
#' @param ... Unused.
#'
#' @return A `list`: an OpenAPI
#'         [parameter object](https://spec.openapis.org/oas/v3.1.0#parameter-object).
#'
#' @examples
#' as_openapi(
#'   openapi_param(
#'     name = "verbose",
#'     description = "Return extra fields",
#'     schema = openapi_schema_boolean()
#'   ),
#'   new_openapi_ctx()
#' )
#'
#' @keywords internal
#' @noRd
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

#' Render a Request Body Into a Request Body Object
#'
#' The flat `schema`/`content_type` pair is nested into the `content` map the
#' specification uses, keyed by media type.
#'
#' @param x An OpenAPI request body.
#' @param ctx Build context.
#' @param ... Unused.
#'
#' @return A `list`: an OpenAPI
#'         [request body object](https://spec.openapis.org/oas/v3.1.0#request-body-object).
#'
#' @examples
#' as_openapi(
#'   openapi_request_body(
#'     schema = openapi_schema_object(
#'       properties = list(name = openapi_schema_string())
#'     ),
#'     description = "The user to create"
#'   ),
#'   new_openapi_ctx()
#' )
#'
#' @keywords internal
#' @noRd
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

#' Render a Response Into a Response Object
#'
#' `status` is not part of the result: it is the key the response is stored
#' under in the operation's `responses` map, which the caller supplies.
#'
#' A response with no schema emits no `content` at all, rather than an empty
#' one, which is how a body-less response such as a `204` is described.
#'
#' @param x An OpenAPI response.
#' @param ctx Build context.
#' @param ... Unused.
#'
#' @return A `list`: an OpenAPI
#'         [response object](https://spec.openapis.org/oas/v3.1.0#response-object).
#'
#' @examples
#' ctx <- new_openapi_ctx()
#'
#' as_openapi(
#'   openapi_response(
#'     201,
#'     "The created user",
#'     headers = list(Location = openapi_schema_string())
#'   ),
#'   ctx
#' )
#'
#' # no schema, so no `content`
#' as_openapi(openapi_response(204, "Deleted"), ctx)
#'
#' @keywords internal
#' @noRd
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
#' The top of the render: everything a route documents ends up here, nested
#' under the path and HTTP method by `build_openapi()`.
#'
#' Fields that were not set are left out rather than emitted as `null`, with
#' one exception: the specification requires a non-empty `responses`, so a
#' route that documents none gets a placeholder `default`.
#'
#' @param x The route's docs.
#' @param ctx Build context.
#' @param path The route's full path, used to derive path parameters.
#' @param ... Unused.
#'
#' @return A `list`: an OpenAPI
#'         [operation object](https://spec.openapis.org/oas/v3.1.0#operation-object).
#'
#' @examples
#' ctx <- new_openapi_ctx()
#'
#' as_openapi(
#'   openapi_docs(
#'     summary = "Get a user by ID",
#'     responses = openapi_response(200, "The user")
#'   ),
#'   ctx,
#'   path = "/users/:id"
#' )
#'
#' # a route documenting no responses still gets a `default` one
#' as_openapi(openapi_docs(summary = "Ping"), ctx)
#'
#' @keywords internal
#' @noRd
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
#' Path parameters come out first, in the order the tokens appear in the
#' route, followed by the query, header, and cookie parameters in the order
#' they were declared. A declared path parameter whose name matches no token
#' is dropped with a note: it would document a parameter that can never be
#' sent.
#'
#' @param parameters Declared parameters.
#' @param ctx Build context.
#' @param path The route's full path.
#'
#' @return A `list` of rendered parameter objects, possibly empty.
#'
#' @examples
#' ctx <- new_openapi_ctx()
#'
#' # `:id` is documented automatically, as a string
#' openapi_render_parameters(NULL, ctx, "/users/:id")
#'
#' # declaring it overrides that default
#' openapi_render_parameters(
#'   list(openapi_param("id", location = "path", schema = openapi_schema_integer())),
#'   ctx,
#'   "/users/:id"
#' )
#'
#' # a path parameter matching no token is dropped, with a warning
#' openapi_render_parameters(
#'   list(openapi_param("nope", location = "path")),
#'   ctx,
#'   "/users/:id"
#' )
#'
#' ctx$notes
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
#' Turns `/users/:id` into `/users/{id}`: ambiorix marks path parameters with
#' a leading `:`, the specification wraps them in braces.
#'
#' @param path A route path.
#'
#' @return A single character string.
#'
#' @examples
#' openapi_path("/users/:id")
#'
#' openapi_path("/users/:id/posts/:post_id")
#'
#' # a path with no parameters is unchanged
#' openapi_path("/health")
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
#' @examples
#' openapi_path_params("/users/:id/posts/:post_id")
#'
#' openapi_path_params("/health")
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
#' Each scheme is rendered with an empty scope array, which is what the
#' specification wants for schemes that do not use scopes. Pass a `list` to
#' supply scopes yourself.
#'
#' @param security Character vector or `list`.
#'
#' @return A `list`: an OpenAPI
#'         [security requirement](https://spec.openapis.org/oas/v3.1.0#security-requirement-object)
#'         array. Empty when `security` is `list()`, which declares that no
#'         authentication is needed.
#'
#' @examples
#' openapi_render_security("bearerAuth")
#'
#' # several schemes that must all be satisfied
#' openapi_render_security(c("bearerAuth", "apiKey"))
#'
#' # a list passes through, for schemes that take scopes
#' openapi_render_security(list(list(oauth = c("read:users"))))
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
#' A character vector is the shorthand: each URL becomes a server object with
#' nothing but a `url`. A `list` is passed through, for servers that also need
#' a `description` or `variables`.
#'
#' @param servers Character vector of URLs, or a `list` of server objects.
#'
#' @return A `list` of
#'         [server objects](https://spec.openapis.org/oas/v3.1.0#server-object).
#'
#' @examples
#' openapi_render_servers("https://api.example.com")
#'
#' openapi_render_servers(
#'   list(
#'     list(url = "https://api.example.com", description = "Production"),
#'     list(url = "http://localhost:3000", description = "Local")
#'   )
#' )
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
#' A named character vector maps tag names to their descriptions; an unnamed
#' element is a tag with no description. Names may be mixed with unnamed
#' elements in one vector.
#'
#' @param tags Character vector, optionally named, or a `list` of tag objects.
#'
#' @return A `list` of
#'         [tag objects](https://spec.openapis.org/oas/v3.1.0#tag-object).
#'
#' @examples
#' openapi_render_tags(c("users", "posts"))
#'
#' openapi_render_tags(c(users = "Everything about users"))
#'
#' # named and unnamed together
#' openapi_render_tags(c(users = "Everything about users", "posts"))
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
#' @examples
#' routes <- list(
#'   list(
#'     route = Route$new("/users/:id"),
#'     path = "/users/:id",
#'     method = "GET",
#'     docs = openapi_docs(
#'       summary = "Get a user by ID",
#'       responses = openapi_response(
#'         200,
#'         "The user",
#'         schema = openapi_schema_ref(
#'           "User",
#'           openapi_schema_object(
#'             properties = list(id = openapi_schema_integer()),
#'             required = "id"
#'           )
#'         )
#'       )
#'     )
#'   ),
#'   # undocumented routes are skipped
#'   list(route = Route$new("/health"), path = "/health", method = "GET")
#' )
#'
#' doc <- build_openapi(routes, list(info = list(title = "My API")))
#'
#' names(doc$paths)
#'
#' # the named schema was hoisted into `components`
#' names(doc$components$schemas)
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
#' Notes are warnings: the document is still built, minus whatever could not
#' be made sense of. Errors abort the build, and every one collected is
#' reported at once rather than stopping at the first.
#'
#' @param ctx Build context.
#'
#' @return `NULL`, invisibly. Stops if `ctx` holds any errors.
#'
#' @examples
#' ctx <- new_openapi_ctx()
#' ctx$notes <- "Reference(s) to undefined schema(s): `User`."
#'
#' # notes only: warns, carries on
#' openapi_report(ctx)
#'
#' ctx$errors <- "Two different schemas are named User."
#'
#' try(openapi_report(ctx))
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
#' resolve `$ref`s. This is why it is separate from the `components` the
#' renderer builds: those have already been flattened into plain `list`s.
#'
#' Walks the whole docs object rather than looking in known places, so a named
#' schema nested inside `oneOf`, `items`, or a property is found too. The
#' first definition of a name wins; a bare reference never defines anything.
#'
#' @param routes A list of routes, as stored in `Routing`'s private `.routes`.
#'
#' @return A named `list` of OpenAPI schemas, keyed by name.
#'
#' @examples
#' routes <- list(
#'   list(
#'     docs = openapi_docs(
#'       request_body = openapi_request_body(
#'         schema = openapi_schema_array(
#'           # nested two levels deep, and still found
#'           openapi_schema_ref("User", openapi_schema_object())
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' names(openapi_named_schemas(routes))
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
