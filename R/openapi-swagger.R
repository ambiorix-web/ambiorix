#' Build the Swagger UI HTML page
#'
#' The Swagger UI assets (CSS & JavaScript) are bundled with ambiorix
#' (`inst/swagger-ui/`) and referenced relative to `assets_path`, so the
#' page works without an internet connection.
#'
#' @param spec_url String /// Required. \cr
#'                 URL of the OpenAPI JSON document.
#'
#' @param title String /// Optional. \cr
#'              Title of the page. \cr
#'              Defaults to "API Documentation".
#'
#' @param assets_path String /// Optional. \cr
#'                    Path at which the Swagger UI assets are served. \cr
#'                    Defaults to "/__swagger__".
#'
#' The title is escaped, the two paths are not: both are set by the developer
#' through `app$openapi()`, never by a request.
#'
#' @return A single character string of HTML.
#'
#' @examples
#' html <- swagger_ui_html("/openapi.json", title = "My API")
#'
#' cat(substr(html, 1, 120))
#'
#' # a trailing slash on `assets_path` is dropped, so the asset URLs never
#' # end up with a doubled `//`
#' grepl("/assets/swagger-ui.css", swagger_ui_html("/spec", assets_path = "/assets/"))
#'
#' @keywords internal
#' @noRd
swagger_ui_html <- function(
  spec_url,
  title = "API Documentation",
  assets_path = "/__swagger__"
) {
  assets_path <- sub("/+$", "", assets_path)

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
    assets_path,
    assets_path,
    spec_url
  )
}
