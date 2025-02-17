#' Serialise
#' 
#' Serialise data to JSON.
#' 
#' @param data Data to serialise.
#' @param ... Options to pass to [yyjsonr::write_json_str].
#' 
#' @noRd 
#' @keywords internal
default_serialiser <- function(data, ...){
  yyjsonr::write_json_str(data, ...)
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
