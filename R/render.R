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
