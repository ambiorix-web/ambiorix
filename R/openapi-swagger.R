#' Build the Swagger UI HTML page
#'
#' The Swagger UI assets (CSS & JavaScript) are bundled with ambiorix
#' (`inst/swagger-ui/`), so the page works without an internet connection.
#'
#' Both URLs are written into the page as given, and are meant to be relative
#' to it: behind a proxy that serves the app under a path the app never sees,
#' an absolute `/openapi.json` is fetched from the proxy's root instead.
#'
#' The title is escaped, the two URLs are not: both are derived from paths the
#' developer sets through `app$openapi()`, never from a request.
#'
#' @param spec_url String /// Required. \cr
#'                 URL of the OpenAPI JSON document.
#'
#' @param title String /// Optional. \cr
#'              Title of the page. \cr
#'              Defaults to "API Documentation".
#'
#' @param assets_url String /// Optional. \cr
#'                   URL of the directory the Swagger UI assets are served
#'                   from. \cr
#'                   Defaults to "__swagger__".
#'
#' @return A single character string of HTML.
#'
#' @examples
#' # the page served at `/docs`
#' html <- swagger_ui_html("openapi.json", title = "My API")
#'
#' cat(substr(html, 1, 120))
#'
#' # the page served at `/api/docs`, the assets at `/__swagger__`
#' html <- swagger_ui_html("../openapi.json", assets_url = "../__swagger__")
#'
#' # a trailing slash on `assets_url` is dropped, so the asset URLs never
#' # end up with a doubled `//`
#' grepl('"assets/swagger-ui.css"', swagger_ui_html("spec", assets_url = "assets/"))
#'
#' @keywords internal
#' @noRd
swagger_ui_html <- function(
  spec_url,
  title = "API Documentation",
  assets_url = "__swagger__"
) {
  assets_url <- sub("/+$", "", assets_url)

  sprintf(
    '
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8" />
        <title>%s</title>
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link
          rel="stylesheet"
          href="%s/swagger-ui.css"
        />
      </head>
      <body>
        <div id="swagger-ui"></div>
        <script src="%s/swagger-ui-bundle.js"></script>
        <script>
          window.onload = function () {
            window.ui = SwaggerUIBundle({
              url: "%s",
              dom_id: "#swagger-ui"
            });
          };
        </script>
      </body>
    </html>
    ',
    html_escape(title),
    assets_url,
    assets_url,
    spec_url
  )
}
