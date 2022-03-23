#' Parsers
#' 
#' Collection of parsers to translate request data.
#' 
#' @param req The request object.
#' @param ... Additional arguments passed to the internal parsers.
#' 
#' @section Functions: 
#' - [parse_multipart()]: Parse `multipart/form-data` using [mime::parse_multipart()].
#' - [parse_json()]: Parse `multipart/form-data` using [jsonlite::fromJSON()].
#' 
#' @return Returns the parsed value as a `list` or `NULL`
#' if it failed to parse.
#' 
#' @name parsers
#' @export 
parse_multipart <- function(req){
  check_installed("mime")  
  tryCatch(
    mime::parse_multipart(req$body),
    error = function(e) NULL
  )
}

#' @export 
#' @rdname parsers
parse_json <- function(req, ...){
  data <- req$body[["rook.input"]]
  data <- data$read_lines()
  tryCatch(
    jsonlite::fromJSON(data, ...),
    error = function(e) NULL
  )
}
