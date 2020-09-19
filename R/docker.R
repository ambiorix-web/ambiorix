#' Dockerfile
#' 
#' Create the dockerfile required to run the application.
#' 
#' @details Reads the `DESCRIPTION` file of the project to produce the `Dockerfile` using [dockerfiler::dock_from_desc()].
#' 
#' @export
create_dockerfile <- function(){
  assert_that(has_file("DESRIPTION"))
  check_installed("dockerfiler")

  cli::cli_alert_warning("Ensure your {.file DESCRIPTION} file is up to date with {.fun devtools::check}")

  dockerfiler::dock_from_desc("DESCRIPTION")
  invisible()
}