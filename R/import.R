#' Import Files
#'
#' Import all R-files in a directory.
#'
#' @param ... String /// Required. \cr
#'            Directory from which to import `.R` or `.r` files. Passed to
#'            [list.files()].
#'
#' @examples
#' if (interactive()) {
#'   import("views")
#' }
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
import <- function(...) {
  files <- list.files(..., pattern = "\\.R$|\\.r$", full.names = TRUE)
  sapply(files, source)
  invisible()
}
