Route <- R6::R6Class(
  "Route",
  public = list(
    path = NULL,
    components = list(),
    pattern = NULL,
    dynamic = FALSE,
    params = NULL,
    basepath = NULL,
    initialize = function(path) {
      assert_that(not_missing(path))

      # remove query
      self$path <- gsub(
        pattern = "\\?.*$",
        replacement = "",
        x = path
      )
    },
    # split the full path, `parent` included, into components and build the
    # pattern from them: a `:token` matches one segment
    compile = function(parent = "") {
      path <- paste0(parent, self$path)
      self$dynamic <- grepl(pattern = ":", x = path, fixed = TRUE)

      components <- strsplit(
        x = path,
        split = "(?<=.)(?=[:/])",
        perl = TRUE
      )[[1]]
      components <- components[components != "/"]
      components <- gsub(pattern = "/", replacement = "", x = components)

      if (!length(components)) {
        components <- ""
      }

      self$components <- lapply(
        X = components,
        FUN = function(component) {
          list(
            dynamic = grepl(pattern = ":", x = component, fixed = TRUE),
            name = gsub(pattern = ":|$", replacement = "", x = component)
          )
        }
      )

      dynamic <- vapply(
        X = self$components,
        FUN = function(comp) comp$dynamic,
        FUN.VALUE = logical(1)
      )
      pattern <- vapply(
        X = self$components,
        FUN = function(comp) comp$name,
        FUN.VALUE = character(1)
      )
      self$params <- pattern[dynamic]

      if (!is.null(.globals$pathToPattern)) {
        self$pattern <- .globals$pathToPattern(path)
        return(invisible(self))
      }

      pattern[dynamic] <- "[^/]+"
      self$pattern <- paste0("^/", paste0(pattern, collapse = "/"), "$")
      invisible(self)
    },
    print = function() {
      cli::cli_rule("Ambiorix", right = "route")
      message("Only used internally")
    }
  )
)

#' Path to pattern
#'
#' Identify a function as a path to pattern function;
#' a function that accepts a path and returns a matching pattern.
#'
#' @param path Function /// Required. \cr
#'             A function that accepts a character vector of length 1 and
#'             returns another character vector of length 1.
#'
#' @examples
#' fn <- function(path) {
#'   pattern <- gsub(":([^/]+)", "(\\\\w+)", path)
#'   paste0("^", pattern, "$")
#' }
#'
#' path_to_pattern <- as_path_to_pattern(fn)
#'
#' path <- "/dashboard/profile/:user_id"
#' pattern <- path_to_pattern(path) # "^/dashboard/profile/(\\w+)$"
#'
#' @return Object of class "pathToPattern".
#' @export
as_path_to_pattern <- function(path) {
  assert_that(is_function(path))

  structure(
    path,
    class = c(
      "pathToPattern",
      class(path)
    )
  )
}


#' @export
print.pathToPattern <- function(x, ...) {
  cli::cli_alert_info("A path to pattern converter")
}

#' @keywords internal
#' @noRd
is_path_to_pattern <- function(obj) {
  inherits(obj, "pathToPattern")
}
