#' Content Headers
#' 
#' Convenient functions for more readable content type headers.
#' 
#' @examples 
#' list(
#'  "Content-Type",
#'  content_json()
#' )
#' 
#' @name content
#' 
#' @export 
content_html <- function() {
  "text/html"
}

#' @rdname content
#' @export 
content_plain <- function() {
  "text/plain"
}

#' @rdname content
#' @export 
content_json <- function() {
  "application/json"
}

#' @rdname content
#' @export 
content_csv <- function() {
  "text/csv"
}

#' @rdname content
#' @export 
content_tsv <- function() {
  "tab-separated-values"
}

#' @rdname content
#' @export 
content_protobuf <- function() {
  "application/x-protobuf"
}
