#' Stop
#'
#' Stop all servers.
#'
#' @return `NULL` (invisibly)
#' @examples
#' if (interactive()) {
#'   stop_all()
#' }
#' @export
stop_all <- function() {
  for (server in .globals$servers) {
    tryCatch(
      server$close(),
      error = function(e) NULL
    )
  }
  .globals$servers <- list()
  invisible(NULL)
}
