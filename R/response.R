#' Responses
#' 
#' HTTP Responses.
#' 
#' @param body Body of response.
#' @param headers HTTP headers.
#' @param status Response status
#' 
#' @name responses
#' 
#' @export
response <- function(body, headers = list('Content-Type' = 'text/html'), status = 200L){
  list(status = status, headers = headers, body = body)
}

#' @rdname responses
#' @export
response_404 <- function(body = "Not found", headers = list('Content-Type' = 'text/html'), status = 404L){
  list(status = status, headers = headers, body = body)
}