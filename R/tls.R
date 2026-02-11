#' Generate Self-Signed Certificate
#'
#' Generate a self-signed TLS certificate for development use.
#'
#' @param cn Common name, typically the hostname (default "localhost").
#'
#' @return A list with `server` and `client` components for use with
#' the `tls` parameter of [Ambiorix] `start()` method.
#'
#' @examples
#' if (interactive()) {
#'   library(ambiorix)
#'
#'   cert <- generate_cert(cn = "localhost")
#'
#'   app <- Ambiorix$new()
#'   app$get("/", function(req, res) {
#'     res$send("Secure connection!")
#'   })
#'
#'   app$start(port = 8443, tls = cert)
#' }
#'
#' @export
generate_cert <- function(cn = "localhost") {
  nanonext::write_cert(cn = cn)
}
