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
stop_all <- function(){
  httpuv::stopAllServers()
}
