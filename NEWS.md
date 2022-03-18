# ambiorix 2.0.0.9000

__Breaking__

- `Response` method `status` renamed to `set_status`
(this is to allow having `status` as a field).
- Dumped support for `.R` templates. These were processed
with bare evals and were thus very insecure.

__Changes__

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
- Do not force body to character fixes [#44](https://github.com/devOpifex/ambiorix/issues/44)
- Do no force content type on response fixes [#45](https://github.com/devOpifex/ambiorix/issues/45)
- Deprecate passing headers to `response` or `send`-like functions, use
`header` method.
- Deprecate `set_header` in favour of `header` method.
- Added family of `header_content*` methods to easily set `Content-Type`.
- Request `HEADERS` is always a `list`.
- Deprecate `set` and `get` on Response and Request.
- Deprecate `status` argument of responses, the active binding should be used instead.
- Partially improved route matching

# ambiorix 2.0.0

__Breaking change__

The `render` and `send_file` methods of the `Response` object now
expect the full path to the template, with the file extension.
Where one would before `res$render("home")`, now one
`res$render("templates/home.html")`.
Similarly, in said templates, to import partials, 
use full path relative to the template in which the partial is used
e.g.: from `[! header.html !]` to `[! partials/header.html !]`.

__Changes__

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
- Removed `create_ambiorix`, see [ambiorix.generator](https://github.com/devOpifex/ambiorix.generator).
- Removed `add_template`, see [ambiorix.generator](https://github.com/devOpifex/ambiorix.generator).
- Deprecate the `Logger` class in favour of the [log](https://github.com/devOpifex/log) package.
- Fixed `parse_json` [#36](https://github.com/devOpifex/ambiorix/issues/36)

# ambiorix 1.0.1

- Deprecate `create_ambiorix`: moving to [ambiorix.generator](https://github.com/devOpifex/ambiorix.generator) package.
- Deprecate `add_template`: moving to [ambiorix.generator](https://github.com/devOpifex/ambiorix.generator) package.
- Added `all` method to define route and handler for all methods `GET`, `POST`, `PUT`, `DELETE`, and `PATCH`.
- The `use` method now accepts a function which is run every time the server receives a request.
- Add `set` and `get` to request to add and retrieve params (namely with the middleware)
- Fix `check_installed`, see [#33](https://github.com/devOpifex/ambiorix/issues/33)

# ambiorix 1.0.0

Initial version.
