#' Dockerfile
#' 
#' Create the dockerfile required to run the application.
#' 
#' @details Reads the `DESCRIPTION` file of the project to produce the `Dockerfile` using [dockerfiler::dock_from_desc()].
#' 
#' @param port,host Port and host to serve the application.
#' @param repos Repositories to install packages, this is passed to [install.packages()], defaults to 
#' [RStudio Public Package Manager](https://packagemanager.rstudio.com/client/#/) which comes with pre-built binaries
#' that much improve the building of Dockerfiles.
#' 
#' @export
create_dockerfile <- function(port, host = "0.0.0.0", repos = c(CRAN = "https://packagemanager.rstudio.com/all/latest")){
  assert_that(has_file("DESRIPTION"))
  check_installed("dockerfiler")
  assert_that(not_missing(port))

  port <- as.integer(port)

  cli::cli_alert_warning("Ensure your {.file DESCRIPTION} file is up to date with {.fun devtools::check}")

  docker <- dockerfiler::dock_from_desc("DESCRIPTION")

  docker$EXPOSE(port)
  
  docker$CMD(
    sprintf(
      "R -e \"options(ambiorix.host='%s', 'ambiorix.port'=%s);source('app.R')\"", 
      host, port
    )
  )

  docker$write("Dockerfile")

  cli::cli_alert_success("Created {.file Dockerfile}")

  invisible(docker)
}