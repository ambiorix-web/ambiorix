.onLoad <- function(libname, pkgname) {
  .globals$infoLog <- new_log(info())
  .globals$errorLog <- new_log(error())
  .globals$successLog <- new_log(success())
}
