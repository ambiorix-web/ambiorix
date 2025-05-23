% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cookie.R
\name{default_cookie_parser}
\alias{default_cookie_parser}
\title{Cookie Parser}
\usage{
default_cookie_parser(req)
}
\arguments{
\item{req}{A \link{Request}.}
}
\value{
A \code{list} of key value pairs or cookie values.
}
\description{
Parses the cookie string.
}
\examples{
if (interactive()) {
  library(ambiorix)

  #' Handle GET at '/greet'
  #'
  #' @export
  say_hello <- function(req, res) {
    cookies <- default_cookie_parser(req)
    print(cookies)

    res$send("hello there!")
  }

  app <- Ambiorix$new()
  app$get("/greet", say_hello)
  app$start()
}

}
