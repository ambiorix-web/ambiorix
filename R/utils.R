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
serialise <- function(data, ...){
  
  # don't serialise scalar
  if(length(data) == 1) return(data)
  
  jsonlite::toJSON(data, auto_unbox = TRUE, dataframe = "rows", ...)
}

#' Render HTML
#' 
#' Evaluates a string to collect [htmltools::tags], evaluates,
#' and returns the render HTML as a collapsed string.
#' 
#' @param expr Expression to evaluate.
#' 
#' @noRd 
#' @keywords internal
render_html <- function(expr){

  tags <- eval(parse(text = expr))

  tmp <- tempfile(fileext = ".html")
  on.exit({
    fs::file_delete(tmp)
  })

  htmltools::save_html(tags, file = tmp, background = "none")

  paste0(readLines(tmp), collapse = "")
}
