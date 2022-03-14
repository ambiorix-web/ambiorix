#' Serialise
#' 
#' Serialise data to JSON.
#' 
#' @param data Data to serialise.
#' @param ... Options to pass to [jsonlite::toJSON()].
#' 
#' @noRd 
#' @keywords internal
default_serialiser <- function(data, ...){
  
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
  getOption("AMBIORIX_SERIALISER", default_serialiser)
}

#' Serialise to JSON
#' 
#' Serialise an object to JSON.
#' Default serialiser can be change by setting the
#' `AMBIORIX_SERIALISER` option to the desired function.
#' 
#' @param data Data to serialise.
#' @param ... Passed to serialiser.
#' 
#' @examples 
#' \dontrun{serialise(cars)}
#' 
#' @export
serialise <- function(data, ...){
  get_serialise()(data, ...)
}
