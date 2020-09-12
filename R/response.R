#' Plain Responses
#' 
#' Plain HTTP Responses.
#' 
#' @param body Body of response.
#' @param headers HTTP headers.
#' @param status Response status
#' 
#' @name responses
#' 
#' @export
response <- function(body, headers = list('Content-Type' = 'text/html'), status = 200L){
  list(status = status, headers = headers, body = as.character(body))
}

#' @rdname responses
#' @export
response_404 <- function(body = "Not found", headers = list('Content-Type' = 'text/html'), status = 404L){
  list(status = status, headers = headers, body = as.character(body))
}

#' Template Responses
#' 
#' Use HTML templates to render HTTP responses.
#' 
#' @param file Name of HTML template file to use (without extension), 
#' looks up the name of the file in `templates` directory.
#' @param data Named list of data objects to place in `[% tags %]` found in template.
#' @param status Status of response.
#' 
#' @examples
#' \dontrun{response_render("homepage", list(title = "Home!"))}
#' 
#' @export
response_render <- function(file, data = list(), status = 200L){
  assert_that(not_missing(file))

  file_path <- make_template_path(file)
  file_content <- readLines(file_path)

  for(i in 1:length(data)){
    pattern <- sprintf("\\[%% ?%s ?%%\\]", names(data)[i]) # [% mustache %]
    file_content <- gsub(pattern, seralise(data[[i]]), file_content)
  }

  file_content <- paste0(file_content, collapse = "")

  response(file_content)
}

#' File Response
#' 
#' Use an HTML file as response, this is similar to [response_render()]: 
#' it does not support `[% tags %]` but is more efficient.
#' 
#' @inheritParams response_render
#' 
#' @examples
#' \dontrun{response_file("homepage")}
#' 
#' @export
response_file <- function(file){
  assert_that(not_missing(file))

  file_path <- make_template_path(file)
  file_content <- readLines(file_path)

  response(file_content)
}

#' Make Template Path
#' 
#' Make a full template path from a template name.
#' 
#' @param template Name of template file.
#' 
#' @noRd 
#' @keywords internal
make_template_path <- function(template){
  template <- gsub("\\.html$", "", template)
  template <- sprintf("./templates/%s.html", template)
  normalizePath(template)
}
