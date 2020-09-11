#' Create Ambiorix Project
#' 
#' Create an Ambiorix project.
#' 
#' @param path Path where to create ambiorix application.
#' 
#' @export
create_ambiorix <- function(path){
  assert_that(not_missing(path))

  if(dir.exists(path))
    stop("Path already exists", call. = FALSE)

  project <- system.file("project", package = "ambiorix")
  fs::dir_copy(project, path)
  cli::cli_alert_success("Created ambiorix template: {.val {path}}")
}

#' Import Files
#' 
#' Import all R-files in a directory.
#' 
#' @param dir Directory from which to import `.R` or `.r` files.
#' 
#' @export
import <- function(dir){{
  assert_that(not_missing(dir))
  assert_that(has_dir(dir))
  files <- fs::dir_ls(dir, pattern = ".R|.r") 
  sapply(files, source)
  invisible()
}}