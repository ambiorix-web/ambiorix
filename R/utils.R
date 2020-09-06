alphanum <- c(1:9, letters)

uuid <- function(){
  x <- sample(alphanum, 20)
  paste0(x, collapse = "")
}