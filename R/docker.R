#' Dockerfile
#' 
#' Create the dockerfile required to run the application.
#' The dockerfile created will install packages from 
#' RStudio Public Package Manager 
#' which comes with pre-built binaries
#' that much improve the speed of building of Dockerfiles.
#' 
#' @details Reads the `DESCRIPTION` file of the project to produce the `Dockerfile`.
#' 
#' @param port,host Port and host to serve the application.
#' @param file_path String. Path to file to write to.
#' 
#' @examples 
#' if (interactive()) {
#'   create_dockerfile(port = 5000L, host = "0.0.0.0", file_path = tempfile())
#'   # create_dockerfile(port = 5000L, host = "0.0.0.0", file_path = "Dockerfile") 
#' }
#' 
#' @return `NULL` (invisibly)
#' @export
create_dockerfile <- function(port, host = "0.0.0.0", file_path){
  .Deprecated(msg = "'create_dockerfile' is deprecated. Please write the Dockerfile manually.")
  assert_that(has_file("DESCRIPTION"))
  assert_that(not_missing(port))
  assert_that(not_missing(file_path))

  cli::cli_alert_warning("Ensure your {.file DESCRIPTION} file is up to date with {.fun devtools::check}")

  # ensure integer
  port <- as.integer(port)

  dockerfile <- c(
    "FROM jcoenep/ambiorix",
    "RUN echo \"options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'))\" >> /usr/local/lib/R/etc/Rprofile.site"
  )

  # CRAN packages
  desc <- read.dcf("DESCRIPTION")
  pkgs <- desc[, "Imports"]
  pkgs <- strsplit(pkgs, ",")[[1]]
  pkgs <- gsub("\\\n", "", pkgs)

  cmd <- sprintf(
    "CMD R -e \"options(ambiorix.host='%s', 'ambiorix.port'=%s);source('app.R')\"", 
    host, port
  )

  dockerfile <- c(
    dockerfile,
    "COPY . .",
    cmd
  )

  x <- writeLines(dockerfile, file_path)

  cli::cli_alert_success("Created {.file Dockerfile}")

  invisible()
}
