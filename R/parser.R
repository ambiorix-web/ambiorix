#' Parsers
#' 
#' Collection of parsers to translate request data.
#' 
#' @param req The request object.
#' 
#' @section Functions: 
#' - [parse_multipart()]: Parse `multipart/form-data`.
#' 
#' @export 
parse_multipart <- function(req){
  check_installed("mime")  

  mime::parse_multipart(req$body)
}