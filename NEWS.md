# ambiorix 1.0.2.9000

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

# ambiorix 1.0.2

- Reaches CRAN
- Removed `create_ambiorix`, see [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator).
- Removed `add_template`, see [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator).
- Deprecate the `Logger` class in favour of the [log](https://github.com/devOpifex/log) package.
- Fixed `parse_json` [#36](https://github.com/JohnCoene/ambiorix/issues/36)

# ambiorix 1.0.1

- Deprecate `create_ambiorix`: moving to [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator) package.
- Deprecate `add_template`: moving to [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator) package.
- Added `all` method to define route and handler for all methods `GET`, `POST`, `PUT`, `DELETE`, and `PATCH`.
- The `use` method now accepts a function which is run every time the server receives a request.
- Add `set` and `get` to request to add and retrieve params (namely with the middleware)
- Fix `check_installed`, see [#33](https://github.com/JohnCoene/ambiorix/issues/33)

# ambiorix 1.0.0

Initial version.
