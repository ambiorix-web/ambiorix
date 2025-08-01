#' Token
#'
#' Create a token
#'
#' @param n Number of bytes.
#' @examples
#' token_create()
#' token_create(n = 32L)
#' @return Length 1 character vector.
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
