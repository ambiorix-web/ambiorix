# ambiorix 1.0.2

- Reaches CRAN
- Removed `create_ambiorix`, see [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator).
- Removed `add_template`, see [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator).
- Deprecate the `Logger` class in favour of the [log](https://github/devOpifex/log) package.

# ambiorix 1.0.1

- Deprecate `create_ambiorix`: moving to [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator) package.
- Deprecate `add_template`: moving to [ambiorix.generator](https://github.com/JohnCoene/ambiorix.generator) package.
- Added `all` method to define route and handler for all methods `GET`, `POST`, `PUT`, `DELETE`, and `PATCH`.
- The `use` method now accepts a function which is run every time the server receives a request.
- Add `set` and `get` to request to add and retrieve params (namely with the middleware)
- Fix `check_installed`, see [#33](https://github.com/JohnCoene/ambiorix/issues/33)

# ambiorix 1.0.0

Initial version.
