#' Add a template file
#' 
#' Convenience function to create new templates in the `templates` directory.
#' 
#' @param name Name of the file, without extension.
#' @param ext File extension of template, `html` or `R`.
#' 
#' @export
add_template <- function(name, ext = c("html", "R")){
  assert_that(not_missing(name))
  assert_that(fs::dir_exists("templates"), msg = "Missing templates directory")

  .Deprecated("add_template_basic", package = "ambiorix.generator")

  # template
  ext <- match.arg(ext)
  template_path <- sprintf("templates/template.%s", ext)
  template <- system.file(template_path, package = "ambiorix")

  # destination
  ext_pat <- sprintf("\\.%s$", ext)
  dest <- gsub(ext_pat, "", name) # remove extension if passed
  dest_path <- here::here("templates", sprintf("%s.%s", dest, ext))
  assert_that(!fs::file_exists(dest_path), msg = "Template already exists")

  fs::file_copy(template, dest_path)

  cli::cli_alert_success("Created {.file {sprintf('templates/%s.%s', name, ext)}}")

  invisible()
}
