#' Browse App
#'
#' Browses the application, if RStudio available uses pane.
#'
#' @param open Whether to open the app.
#' @param url URL to browse.
#'
#' @noRd
#' @keywords internal
browse_ambiorix <- function(open, url){
  if(!open) return()

  viewer <- getOption("viewer", browseURL)

  viewer(url)

  invisible()
}

`%response%` <- function(lhs, rhs){
  if(is.null(lhs)) return(rhs)
  if(!inherits(lhs, "ambiorixResponse")) return(rhs)
  return(lhs)
}

`%error%` <- function(lhs, rhs){
  if(is.null(lhs)) return(rhs)
  return(lhs)
}

#' Remove Extensions
#'
#' Remove extensions from files.
#'
#' @noRd
#' @keywords internal
remove_extensions <- function(files){
  tools::file_path_sans_ext(files)
}

#' Checks if Package is Installed
#'
#' Checks if a package is installed, stops if not.
#'
#' @param pkg Package to check.
#'
#' @noRd
#' @keywords internal
check_installed <- function(pkg){
  has_it <- base::requireNamespace(pkg, quietly = TRUE)

  if(!has_it)
    stop(sprintf("This function requires the package {%s}", pkg), call. = FALSE)
}

#' Retrieve Port
#'
#' Retrieve the port to use.
#'
#' @param port Input port, optional.
#'
#' @return A port number.
#'
#' @noRd
#' @keywords internal
get_port <- function(host, port = NULL){

  # we need to override the port if the load balancer
  # is running. This should NOT be set by a dev
  # this ensures we can overwrite
  forced <- getOption("ambiorix.port.force")
  if(!is.null(forced))
    return(forced)

  if(!is.null(port))
    return(as.integer(port))

  httpuv::randomPort(host = host)
}

#' Make label
#' 
#' Cheap replacement for rlang::as_label to avoid dependency.
#' Must fix.
#' 
#' @noRd
#' @keywords internal
as_label <- function(x) {
  name <- tryCatch(
    is.character(x),
    error = function(e) e
  )

  if(!inherits(name, "error"))
    return(x)

  deparse(substitute(x, parent.frame()))
}

#' Silent readLines
#' 
#' Avoids EOF warnings.
#' 
#' @param ... Passed to [readLines()]:
#' 
#' @keywords internal
read_lines <- function(...) {
  suppressWarnings(
    readLines(...)
  )
}

#' Read file from disk or cache
#' 
#' @param path Path to file.
#' 
#' @keywords internal
read_lines_cached <- function(path) {
  if(!.globals$cache_tmpls)
    return(read_lines(path))

  if(length(.cache_tmpls[[path]]) > 0L)
    return(.cache_tmpls[[path]])

  content <- read_lines(path)
  .cache_tmpls[[path]] <- content
  return(content)
}
