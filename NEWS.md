# ambiorix 4.0.0

**Breaking Changes**

- `parse_json()` no longer collapses JSON into data frames or matrices:
  `obj_of_arrs_to_df`, `arr_of_objs_to_df`, and `arr_of_arrs_to_matrix` are
  now off by default, so a body keeps the structure it was sent with — an
  array of objects is a list of named lists, never a data frame. The same
  JSON shape now always parses to the same R shape, whatever values it
  holds; previously the R type of a parsed body depended on whether the
  values happened to collapse. An array of one is read marked `AsIs`, so
  `["a"]` and `"a"` stay apart: `I("a")` compares and serialises like the
  string, and is written back as `["a"]`. Restore the old reading per call
  with `req$parse_json(arr_of_objs_to_df = TRUE)`, or globally with
  `options(AMBIORIX_JSON_PARSER = ...)`.
- `parse_json()` reads an integer too large for R's integer type as a
  double, where it read it as a string: `{"id": 3000000000}` is now
  numeric, and can be documented and validated as an integer. Whole numbers
  past 2^53 are rounded; pass `int64 = "string"` for the old reading, or
  `int64 = "bit64"` with the bit64 package attached for exact 64-bit
  integers.
- Websocket messages are parsed by the same parser as request bodies, so
  the two changes above apply to the message an `app$receive()` handler
  gets: an array of objects is a list of named lists, not a data frame,
  and a large integer is a number, not a string. A parser set with
  `options(AMBIORIX_JSON_PARSER = ...)` now reads messages too, the way
  `AMBIORIX_SERIALISER` already wrote them. A binary frame is parsed as
  well, where it errored.
- `parse_json()`, `parse_form_urlencoded()` and `parse_multipart()` return
  `NULL` for a request with no body, where they returned `list()`. Nothing
  on the wire is `NULL`, so an absent body is now told apart from `{}` and
  `[]`.
- A route parameter matches one path segment: `/users/:id` no longer
  matches `/users/2/3`, `/users/1/` or `/users/`. It used to match across
  `/`, which let `/orgs/:org/info` answer `/orgs/acme/teams/core/info`
  with `org = "acme"` and the rest of the path dropped. A trailing slash
  was only ever accepted after a parameter; `/users` never matched
  `/users/`. Use a regular expression, e.g. `/users/.+`, to match across
  `/` on purpose.
- A `:token` in a router's basepath is matched at any depth: a router
  mounted on `Router$new("/orgs/:org")` had its routes compiled with the
  literal text `:org`, so none of them could be reached. Exact paths are
  now tried before parameters across every router, not within each one,
  so a router's `/users/me` is matched before the app's `/users/:id`.
  `Routing$prepare()` is gone; `get_routes()` compiles the routes.
- A router's middleware runs for the routes of that router and of the
  routers mounted on it, and for nothing else. It was matched against the
  request path as a regular expression, so a router at `/api` also ran its
  middleware for an app route at `/x/api/y` or `/apiary`, and a router
  with a `:token` in its basepath, e.g. `Router$new("/orgs/:org")`, never
  ran its middleware at all.
- A parameter middleware, `param()`, is scoped the same way: it runs for
  the routes of its router and of the routers mounted on it, so
  `app$param("id", ...)` answers every `:id`. It used to run for the
  routes of its own router only, as in Express. It also runs after the
  middleware and the request validation, right before the handler: an
  authentication middleware sees the request first, and `value` is the
  validated value on a documented route. It used to run before both, and
  received the raw string.

**New Features**

- Add support for OpenAPI (Swagger) documentation,
  [pull/163](https://github.com/ambiorix-web/ambiorix/pull/163):
  - `app$openapi()` enables it. Routes registered with a `docs` argument are
    collected into an OpenAPI 3.1 document served at `/openapi.json`, with
    the Swagger UI at `/docs`. `title`, `version`, `description`, `info`,
    `servers`, `tags`, `security_schemes`, and `security` fill the
    document's top level. The Swagger UI assets are bundled with the
    package, so the pages work without an internet connection.
  - `openapi_docs()` documents a route, with `openapi_param()`,
    `openapi_request_body()`, and `openapi_response()` for its parameters,
    body, and responses. Path parameters are documented automatically from
    the route's `:param` tokens.
  - Schemas are built with `openapi_schema_string()`,
    `openapi_schema_integer()`, `openapi_schema_number()`,
    `openapi_schema_boolean()`, `openapi_schema_array()`,
    `openapi_schema_object()`, and `openapi_schema()`, and named with
    `openapi_schema_ref()`, which places a schema in the document's
    `components` and references it with `$ref` wherever it is used.
  - Documented routes are validated. Query and path parameters and JSON,
    form-urlencoded, and multipart bodies are checked against the
    documented schemas before the handler runs, `allOf`, `anyOf`, `oneOf`,
    and `not` included; parameters are converted to their documented type,
    and the parsed body is stored on `req$payload`.
    A request that does not match is answered with a `400` listing what is
    wrong. `app$openapi(on_invalid =)` replaces that response,
    `app$openapi(validate = FALSE)` turns validation off app-wide, and
    `openapi_docs(validate =)` overrides it per route.
- Add `req$parse_form_urlencoded()`, next to `req$parse_json()` and
  `req$parse_multipart()`.

**Bug Fixes**

- Allow overriding of default error handler, regardless of the order
  it's registered in, [pull/161](https://github.com/ambiorix-web/ambiorix/pull/161).
  The same holds for a router: its `error` handler answers its routes,
  and a router without one falls back to the router it is mounted on,
  then to the app. `set_error()` is available on routers too.
- A router mounted in more than one place answers at each of them; only
  the last mount used to.
- An error in a middleware or a parameter middleware is answered by the
  route's error handler, or `app$error`, the way an error in the handler
  is. It used to reach httpuv, which answered `ERROR: <the R message>`,
  handing the message to the client, and logged nothing.

# ambiorix 3.0.0

**Breaking Changes**

- Drop all deprecated function parameters,
  [pull 152](https://github.com/ambiorix-web/ambiorix/pull/152).

**Bug Fixes**

- `send_file()` can now send arbitrary files,
  [pull 151](https://github.com/ambiorix-web/ambiorix/pull/151).

**Changes**

- Remove dependency on the `{fs}` pkg,
  [pull 155](https://github.com/ambiorix-web/ambiorix/pull/155).
- Refactor HTTP methods in routing,
  [pull 144](https://github.com/ambiorix-web/ambiorix/pull/144).

# ambiorix 2.2.2

**New Features**

- Add support for parameter middlewares, [pull/131](https://github.com/ambiorix-web/ambiorix/pull/131).
- Add support for {mirai} promises, [pull/134](https://github.com/ambiorix-web/ambiorix/pull/134).

**Changes**

- Update browser URL to log "127.0.0.1" when the provided app host is "0.0.0.0", [pull/137](https://github.com/ambiorix-web/ambiorix/pull/137).
- Set default response status for redirection to 302, [pull/145](https://github.com/ambiorix-web/ambiorix/pull/145).

**Bug Fixes**

- Rewind {Rook} input post-read on all request parsers, [pull/135](https://github.com/ambiorix-web/ambiorix/pull/135).
- Add static routes to total number of routes, [pull/147](https://github.com/ambiorix-web/ambiorix/pull/147).

# ambiorix 2.2.1

**Changes**

- When rendering via {htmltools}, add character encoding as first item in the
  HTML document, as per the [html standard](https://html.spec.whatwg.org/multipage/semantics.html#charset)

**Bug Fixes**

- Fix bug causing app to crash at startup when no explicit routes are defined on the
  Ambiorix instance, [pull/123](https://github.com/ambiorix-web/ambiorix/pull/123)

# ambiorix 2.2.0

**New Features**

- Enable nesting of Routers, [pull/73](https://github.com/ambiorix-web/ambiorix/pull/73).
- Enable full support for htmltools tags, no need for html templates, [pull/78](https://github.com/ambiorix-web/ambiorix/pull/78), [pull/92](https://github.com/ambiorix-web/ambiorix/pull/92).
- Add `engine()` method set custom renderers, [pull/66](https://github.com/ambiorix-web/ambiorix/pull/66).
- Add `set_error()` method to set a global error handler, [pull/64](https://github.com/ambiorix-web/ambiorix/pull/64).
- Add a default error handler, [pull/88](https://github.com/ambiorix-web/ambiorix/pull/88).

**Changes**

- Resolve port to bind server on in a specific order, [pull/75](https://github.com/ambiorix-web/ambiorix/pull/75).
- Switch to {yyjsonr} for faster serialization & de-serialization, [pull/100](https://github.com/ambiorix-web/ambiorix/pull/100).
- Switch to {webutils} for faster parsing of multipart & urlencoded request bodies, [pull/100](https://github.com/ambiorix-web/ambiorix/pull/100).
- Continually process requests using `httpuv::service()` instead of a while loop, [pull/98](https://github.com/ambiorix-web/ambiorix/pull/98).
- Remove syntactic sugar to improve backwards compatibility with R <= 4.1.0, [pull/113](https://github.com/ambiorix-web/ambiorix/pull/113).
- Deprecate `create_dockerfile()`, [pull/116](https://github.com/ambiorix-web/ambiorix/pull/116)

**Bug Fixes**

- Fix bug hindering change of the max body size of a request,
  [pull/69](https://github.com/ambiorix-web/ambiorix/pull/69).
- Fix issue causing pattern matching in routes to throw error, [pull/82](https://github.com/ambiorix-web/ambiorix/pull/82), [pull/110](https://github.com/ambiorix-web/ambiorix/pull/110).
- Fix bug on error condition messaging when promise evaluation fails, [pull/88](https://github.com/ambiorix-web/ambiorix/pull/88).

# ambiorix 2.1.1

- Added `cache_templates` method to cache templates
  in memory.
- Added `use_html_template` to use `htmltools::htmlTemplate` as
  rendering.
- Custom renderers (`as_renderer`) are now more robust.
- Add `limit` field to protect against large uploads.
- Fix issue with setting custom websocket handler [#62](https://github.com/ambiorix-web/ambiorix/issues/62).
- Add `engine` method on router to set custom renderers (`use` deprecated for custom renderers).

# ambiorix 2.1.0

**Breaking**

- `Response` method `status` renamed to `set_status`
  (this is to allow having `status` as a field).

**Changes**

- Improve rendering of templates.
  No longer force render data as JSON if using an HTML template.
- Allow nested partials, their path must be relative.
- Added `jobj` function to serialise objects to JSON in `render`.
- Allow passing `host` and `port` to `start()` method.
- Added `host` and `port` active bindings.
- Move internal `is_running` field to private.
- Added `status` active binding on `Response`.
- Added `get_header`, `set_header`, and `set_headers` to `Response`.
- Allow adding multiple cookies to the request.
- Add `parseCookie` JavaScript helper function to JavaScript file.
- Allow customising the cookie parser with `as_cookie_parser`.
- Allow adding cookie value preprocessors with `as_cookie_preprocessor`.
- Arguments to `cookie` method on `Response` take options.
- Added `clear_cookie` method to `Response`
- Cookies of the same name overwrite rather than duplicate.
- Added `content_*` family of convenience function to set content type headers.
- Export `serialise`
- Fixed issue where wrong path pattern was matched.
- Catch error if no route is specified.
- Do not force body to character fixes [#44](https://github.com/ambiorix-web/ambiorix/issues/44)
- Do no force content type on response fixes [#45](https://github.com/ambiorix-web/ambiorix/issues/45)
- Deprecate passing headers to `response` or `send`-like functions, use
  `header` method.
- Deprecate `set_header` in favour of `header` method.
- Added family of `header_content*` methods to easily set `Content-Type`.
- Request `HEADERS` is always a `list`.
- Deprecate `set` and `get` on Response and Request, this is no longer
  needed the environments are no longer locked; `res$myVar <- 2L`.
- Deprecate `status` argument of responses, the active binding should
  be used instead; `res$status <- 404L`.
- Partially improved route matching.
- Allow customising the path to pattern converter.
- Added `get_header` method to the retrieve a specific method.
- Fix htmlwidget response.
- Added `image`, `png` and `jpeg` methods to `Response` to serve images.
- Added `ggplot2` method to `Response`.
- Allow `use` method on `Router` these will only be applied to paths
  of said router.
- Added `parse_*` methods to the `Request`.
- Cookie `path` defaults to `/`.
- Fix default serialiser
- More robust `parser_*` methods and fuctions.
- Empty cookie is empty list instead of empty string.
- Added `mockRequest` to for testing purposes.
- Fixed `port`, `host`, and `websocket` active bindings.
- Add ability to create custom renderer, see
  [jader](https://github.com/ambiorix-web/jader), and
  [pugger](https://github.com/ambiorix-web/pugger).

# ambiorix 2.0.0

**Breaking change**

The `render` and `send_file` methods of the `Response` object now
expect the full path to the template, with the file extension.
Where one would before `res$render("home")`, now one
`res$render("templates/home.html")`.
Similarly, in said templates, to import partials,
use full path relative to the template in which the partial is used
e.g.: from `[! header.html !]` to `[! partials/header.html !]`.

**Changes**

- Middleware no longer uses global environment that may cause side effect
  across sessions.
- `set` and `get` methods on request store in environment to allow
  locking variables when using `set`.
- Better instructions for deploying as a service.
- Remove the deprecated `Logger` class, see [log](https://github.com/devOpifex/log) package.
- Pass `host` to free port fetch function.
- Add hidden option to force change port for upcoming related service.
- Internals of calls reworked to share response object.
  This is how it should always have worked, it allows middlewares to
  updatre request and response to be used/passed to subsequent calls.
- Middleware check for run has been fixed.
- Document and export the `Response` class.
- Added pre-hook to response.
- Document and export `Request` class.
- `set` and `get` methods on `Request` and `Response` accept character
  strings as `name`.
- Use R 4.1.0 + add `Depends`
- Allow passing a list of functions to `use` to easily se multiple
  middlewares at once.
- `render` method correctly sets the `Content-type` header.
- `headers` method more robust to avoid duplicated headers.
- Add `token_create` function.
- Add `cookie` method to the `Response` class to easily set cookies.
- Add `cookie` field to `Request` to hold _parsed_ `HTTP_COOKIE`.
- Properly URL decode query string values.
- Add `sendf` method to `Response` class to pre-process request
  with `sprintf`.
- Silently read templates, no more EOF warnings.
- More informative print messages for classes.
- Add post render hooks to response.
- Added `get_headers` method to `Response` to retrieve currently set headers.
- Unlock objects to allow adding new elements to `Response` and `Request`.
- Upgrade websocket protocol if on HTTPS protocol.
- Improved the default log.
- `log` argument of ambiorix constructor now defaults to `TRUE`.
- Added `md` method to `Response` to render `.md` files.
- Added `set_log*` functions to allow using custom logs.

# ambiorix 1.0.2

- Reaches CRAN
- Removed `create_ambiorix`, see [ambiorix.generator](https://github.com/ambiorix-web/ambiorix.generator).
- Removed `add_template`, see [ambiorix.generator](https://github.com/ambiorix-web/ambiorix.generator).
- Deprecate the `Logger` class in favour of the [log](https://github.com/devOpifex/log) package.
- Fixed `parse_json` [#36](https://github.com/ambiorix-web/ambiorix/issues/36)

# ambiorix 1.0.1

- Deprecate `create_ambiorix`: moving to [ambiorix.generator](https://github.com/ambiorix-web/ambiorix.generator) package.
- Deprecate `add_template`: moving to [ambiorix.generator](https://github.com/ambiorix-web/ambiorix.generator) package.
- Added `all` method to define route and handler for all methods `GET`, `POST`, `PUT`, `DELETE`, and `PATCH`.
- The `use` method now accepts a function which is run every time the server receives a request.
- Add `set` and `get` to request to add and retrieve params (namely with the middleware)
- Fix `check_installed`, see [#33](https://github.com/ambiorix-web/ambiorix/issues/33)

# ambiorix 1.0.0

Initial version.
