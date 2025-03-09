#' Stop
#' 
#' Stop all servers.
#' 
#' @return `NULL` (invisibly)
#' @export
stop_all <- function(){
  httpuv::stopAllServers()
}
