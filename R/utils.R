alphanum <- c(1:9, letters)

#' Generate Random UUID
#' @noRd 
#' @keywords internal
uuid <- function(){
  x <- sample(alphanum, 20)
  paste0(x, collapse = "")
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

#' Browse App
#' 
#' Browses the application, if RStudio available uses pane.
#' 
#' @param open Whether to open the app.
#' @param url URL to browse.
#' 
#' @noRd 
#' @keywords internal
browse_ambiorix <- function(open, url){
  if(!open) return()

  viewer <- getOption("viewer", browseURL)

  viewer(url)

  invisible()
}

`%response%` <- function(lhs, rhs){
  if(is.null(lhs)) return(rhs)
  if(!inherits(lhs, "ambiorixResponse")) return(rhs)
  return(lhs)
}

`%error%` <- function(lhs, rhs){
  if(is.null(lhs)) return(rhs)
  return(lhs)
}

#' Remove Extensions
#' 
#' Remove extensions from files.
#' 
#' @noRd 
#' @keywords internal
remove_extensions <- function(files){
  tools::file_path_sans_ext(files)
}

#' Replaces Partials Tags
#' 
#' Replaces partials tags `[! partial.html !]` so it can be intrepreted by [glue::glue_data()]
#' 
#' @param file_content Content of the template file containing tags, output of [readLines()]:
#' a character vector.
#' @param ext Extension of template file.
#' 
#' @noRd 
#' @keywords internal
replace_partials <- function(file_content, ext = c("html", "R")){

  assert_that(not_missing(file_content))

  ext <- match.arg(ext)

  if(ext == "html"){
    # here only need read and collapse
    file_content <- gsub("\\[\\! ?", "[% paste0(readLines(here::here('templates', 'partials', '", file_content)
    file_content <- gsub(" ?\\!\\]", "')), collapse='') %]", file_content)
  } else {
    # here needs read collapse and wrap in `HTML`
    file_content <- gsub("\\[\\! ?", "[% HTML(paste0(readLines(here::here('templates', 'partials', '", file_content)
    file_content <- gsub(" ?\\!\\]", "')), collapse='')) %]", file_content)
  }

  return(file_content)
}

check_installed <- function(pkg){
  has_it <- base::requireNamespace(pkg, quietly = TRUE)

  if(has_it)
    stop(sprintf("This function requires the package {%s}", pkg), call. = FALSE)
}
