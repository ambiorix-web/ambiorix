#' Token
#' 
#' Create a token
#' 
#' @param n Number of bytes.
#' 
#' @export 
token_create <- function(n = 16L) {
  paste(
    as.hexmode(
      sample(
        256,
        n,
        replace = TRUE
      )
    ),
    collapse = ""
  )
}
