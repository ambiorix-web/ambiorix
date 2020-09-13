#' Serialise
#' 
#' Serialise data to JSON.
#' 
#' @param data Data to serialise.
#' @param ... Options to pass to [jsonlite::toJSON()].
#' 
#' @noRd 
#' @keywords internal
serialise <- function(data, ...){
  
  # don't serialise scalar
  if(length(data) == 1) return(data)
  
  jsonlite::toJSON(data, auto_unbox = TRUE, dataframe = "rows", ...)
}

#' Retrieve Serialiser
#' 
#' Retrieve the serialiser to use, either the default or that defined by user.
#' 
#' @noRd 
#' @keywords internal
get_serialise <- function(){
  getOption("AMBIORIX_SERIALISER", serialise)
}