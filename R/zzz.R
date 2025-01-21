.globals <- new.env(hash = TRUE)
.cache_tmpls <- new.env(hash = TRUE)

.onLoad <- function(libname, pkgname) {
  .globals$infoLog <- new_log(info())
  .globals$errorLog <- new_log(error())
  .globals$successLog <- new_log(success())
  .globals$cookieParser <- default_cookie_parser
  .globals$pathToPattern <- NULL
  .globals$cookiePreprocessors <- list()
  .globals$renderer <- NULL
  .globals$cache_tmpls <- FALSE
  .globals$wsc <- list()
}
