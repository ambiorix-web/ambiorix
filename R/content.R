#' Content Headers
#' 
#' Convenient functions for more readable content type headers.
#' 
#' @name content
#' 
#' @export 
content_html <- function() {
  list('Content-Type' = 'text/html')
}

#' @rdname content
#' @export 
content_plain <- function() {
  list('Content-Type' = 'text/plain')
}

#' @rdname content
#' @export 
content_json <- function() {
  list("Content-Type" = "application/json")
}

#' @rdname content
#' @export 
content_csv <- function() {
  list("Content-Type" = "text/csv")
}

#' @rdname content
#' @export 
content_tsv <- function() {
  list("Content-Type" = "tab-separated-values")
}

#' @rdname content
#' @export 
content_protobuf <- function() {
  list("Content-Type" = "application/x-protobuf")
}
