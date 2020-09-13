#' Stop
#' 
#' Stop all servers.
#' 
#' @export
stop_all <- function(){
  httpuv::stopAllServers()
}