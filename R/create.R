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
  cli::cli_alert_success("Created ambiorix - {.val {path}}")
}
