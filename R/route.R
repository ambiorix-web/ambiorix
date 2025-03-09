Route <- R6::R6Class(
  "Route",
  public = list(
    path = NULL,
    components = list(),
    pattern = NULL,
    dynamic = FALSE,
    initialize = function(path){
      assert_that(not_missing(path))
      self$path <- gsub("\\?.*$", "", path) # remove query
      self$dynamic <- grepl(":", path)
    },
    as_pattern = function(parent = ""){
      if(!is.null(.globals$pathToPattern)) {
        self$pattern <- .globals$pathToPattern(self$path)
        return(
          invisible(self)
        )
      }

      pattern <- sapply(self$components, function(comp){
        if(comp$dynamic)
          return("[[:alnum:][:space:][:punct:]]*")

        return(comp$name)
      })

      pattern <- paste0(pattern, collapse = "/")
      self$pattern <- paste0("^", parent, "/", pattern, "$")
      invisible(self)
    },
    decompose = function(parent = ""){
      path <- paste0(parent, self$path)
      # split
      components <- strsplit(path, "(?<=.)(?=[:/])", perl = TRUE)[[1]]

      # remove lonely /
      components <- components[components != "/"]

      if(length(components) == 0){
        self$components <- list(
          list(
            index = 1L, 
            dynamic = FALSE,
            name = ""
          )
        )
        return()
      }

      # cleanup
      components <- gsub("/", "", components)

      components <- as.list(components)
      comp <- list()
      for(i in seq_along(components)){
        c <- list(
          index = i, 
          dynamic = grepl(":", components[[i]]),
          name = gsub(":|$", "", components[[i]])
        )
        comp <- append(comp, list(c))
      }

      self$components <- comp
      invisible(self)
    },
    print = function(){
      cli::cli_rule("Ambiorix", right = "route")
      cat("Only used internally\n")
    }
  )
)

#' Path to pattern
#' 
#' identify a function as a path to pattern function;
#' a function that accepts a path and returns a matching pattern.
#' 
#' @param path A function that accepts a character vector of length 1
#' and returns another character vector of length 1.
#' 
#' @export 
#' @return Object of class "pathToPattern".
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
