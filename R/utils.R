alphanum <- c(1:9, letters)

#' Generate Random UUID
#' @noRd 
#' @keywords internal
uuid <- function(){
  x <- sample(alphanum, 20)
  paste0(x, collapse = "")
}

#' Serialise
#' 
#' Serialise data to JSON.
#' 
#' @param data Data to serialise.
#' @param ... Options to pass to [jsonlite::toJSON()].
#' 
#' @noRd 
#' @keywords internal
seralise <- function(data, ...){
  
  # don't serialise scalar
  if(length(data) == 1) return(data)
  
  jsonlite::toJSON(data, auto_unbox = TRUE, dataframe = "rows", ...)
}

#' Parse Query Value
#' 
#' Attemps to parse the query value to return a valid R object.
#' 
#' @noRd 
#' @keywords internal
parse_query_value <- function(x){
  if(x %in% c("true", "false", "TRUE", "FALSE"))
    return(as.logical(as.factor(x)))

  if(!grepl("[[:alpha:]]", x))
    return(as.numeric(x))

  return(x)
}
