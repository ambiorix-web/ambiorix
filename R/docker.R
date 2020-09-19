#' Dockerfile
#' 
#' Create the dockerfile required to run the application.
#' The dockerfile created will install packages from 
#' [RStudio Public Package Manager](https://packagemanager.rstudio.com/client/#/) 
#' which comes with pre-built binaries
#' that much improve the speed of building of Dockerfiles.
#' 
#' @details Reads the `DESCRIPTION` file of the project to produce the `Dockerfile`.
#' 
#' @param port,host Port and host to serve the application.
#' 
#' @export
create_dockerfile <- function(port, host = "0.0.0.0"){
  assert_that(has_file("DESCRIPTION"))
  assert_that(not_missing(port))

  cli::cli_alert_warning("Ensure your {.file DESCRIPTION} file is up to date with {.fun devtools::check}")

  # ensure integer
  port <- as.integer(port)

  dockerfile <- c(
    "FROM rocker/r-base",
    "RUN echo \"options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'), download.file.method = 'libcurl')\" >> /usr/local/lib/R/etc/Rprofile.site",
    "RUN R -e 'install.packages(\"remotes\")'",
    "RUN R -e 'remotes::install_github(\"JohnCoene/ambiorix\")'"
  )

  # CRAN packages
  desc <- read.dcf("DESCRIPTION")
  pkgs <- desc[, "Imports"]
  pkgs <- strsplit(pkgs, ",")[[1]]
  pkgs <- gsub("\\\n", "", pkgs)
  cran <- sapply(pkgs, function(pkg){
    sprintf("RUN R -e \"install.packages('%s')\"", pkg)
  })

  # remotes
  rmts <- tryCatch(desc[, 'Remotes'], error = function(e) "")
  if(rmts != ""){
    rmts <- strsplit(rmts, ",")[[1]]
    rmts <- gsub("\\\n", "", rmts)
    rmts <- sapply(rmts, function(pkg){
      sprintf("RUN R -e \"remotes::install_github('%s')\"", pkg)
    })
  }

  cmd <- sprintf(
    "RUN R -e \"options(ambiorix.host='%s', 'ambiorix.port'=%s);source('app.R')\"", 
    host, port
  )

  dockerfile <- c(
    dockerfile,
    cran,
    rmts,
    "COPY . .",
    cmd
  )

  writeLines(dockerfile, "Dockerfile")

  cli::cli_alert_success("Created {.file Dockerfile}")

  invisible()
}