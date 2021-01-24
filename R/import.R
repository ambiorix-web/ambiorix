#' Import Files
#' 
#' Import all R-files in a directory.
#' 
#' @param ... Directory from which to import `.R` or `.r` files.
#' 
#' @examples 
#' \dontrun{import("views")}
#' 
#' @return Invisibly returns `NULL`.
#' 
#' @export
import <- function(...){{
  files <- fs::dir_ls(here::here(...), regexp = "\\.R$|\\.r$") 
  sapply(files, source)
  invisible()
}}