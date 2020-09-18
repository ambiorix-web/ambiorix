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
#' @name parsers
#' @export 
parse_multipart <- function(req, ...){
  check_installed("mime")  

  mime::parse_multipart(req$body, ...)
}

#' @rdname parsers
parse_json <- function(req, ...){
  jsonlite::fromJSON(req$body()$read(), ...)
}
