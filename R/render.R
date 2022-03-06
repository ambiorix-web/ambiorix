#' Replace Partials
#' 
#' Replaces partial tags `[! ... !]` with their content.
#' 
#' @param content File content where partials should be
#' replaced.
#' @param dir Directory of file from which `content` originates.
#' 
#' @keywords internal
replace_partials <- \(content, dir) {
  content <- apply_replace_partial(content, dir)

  if(any(grepl("\\[! .* !\\]", content)))
    content <- replace_partials(content, dir)

  return(content)
}

#' Replace Partial
#' 
#' Replaces partial on a single line.
#' Assumes we have a single partial on a single line.
#' 
#' @param line Single content line.
#' @param dir Base directory.
#' 
#' @keywords internal
replace_partial <- \(line, dir) {
  if(length(line) > 1)
    line <- apply_replace_partial(line, dir)

  if(!grepl("\\[! .* !\\]", line))
    return(line)

  path <- gsub("\\[!|!\\]", "", line) |> 
    trimws()

  # construct new base directory
  new_dir <- dirname(path)
  new_dir <- file.path(dir, new_dir)

  # build new path based on new directory
  path <- basename(path)
  path <- file.path(new_dir, path)

  # read lines
  lines <- read_lines(path)

  # add new directory
  pat <- sprintf("[! %s/", new_dir)
  gsub("\\[! ", pat, lines)
}

#' Get Directory
#' 
#' Retrieve directory from a file.
#' 
#' @param file File to retrieve directory from.
#' 
#' @keywords internal
get_dir <- \(file) {
  normalizePath(file) |> 
    dirname()
}

#' Replace Partial Vectorised
#' 
#' Vectorised version of [replace_partial()].
#' 
#' @inheritParams replace_partials
#' 
#' @keywords internal
apply_replace_partial <- \(content, dir) {
  sapply(content, replace_partial, dir) |> 
    unname() |> 
    unlist()
}

#' R Object
#'
#' Treats a data element rendered in a response (`res$render`) as
#' a data object and ultimately uses [dput()].
#'
#' For instance in a template, `x <- [% var %]` will not work with
#' `res$render(data=list(var = "hello"))` because this will be replace
#' like `x <- hello` (missing quote): breaking the template. Using `robj` one would
#' obtain `x <- "hello"`.
#'
#' @param obj R object to treat.
#'
#' @export
robj <- function(obj){
  assert_that(not_missing(obj))

  # Supress warnings otherwise
  # NULL, NA, and the likes
  # raise messages
  suppressWarnings(
    structure(obj, class = c("robj", class(obj)))
  )
}

#' @export
print.robj <- function(x, ...){
  cli::cli_alert_info("R object")
  x |> 
    dput(x) |> 
    print()
}

#' JSON Object
#' 
#' Serialises an object to JSON in `res$render`.
#' 
#' @param obj Object to serialise.
#' 
#' @export 
jobj <- function(obj) {
  suppressWarnings(
    structure(obj, class = c("jobj", class(obj)))
  )
}

#' @export
print.jobj <- function(x, ...){
  cli::cli_alert_info("JSON object")
  get_serialise(...)(x) |> 
    print()
}

#' Pre Hook Response
#' 
#' @param content File content, a character vector.
#' @param data A list of data passed to `glue::glue_data`.
#' 
#' @export 
pre_hook <- function(
  content,
  data
) {
  structure(
    list(
      content = content,
      data = data
    ),
    class = c("list", "responsePreHook")
  )
}
