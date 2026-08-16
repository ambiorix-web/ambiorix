#' Token
#'
#' Create a token
#'
#' @param n Integer /// Optional. \cr
#'          Number of bytes. \cr
#'          Defaults to `16L`.
#'
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
