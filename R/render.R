#' Replace Partials
#'
#' Replaces partial tags `[! ... !]` with their content.
#'
#' @param content File content where partials should be
#' replaced.
#' @param dir Directory of file from which `content` originates.
#'
#' @keywords internal
#' @noRd
replace_partials <- function(content, dir) {
  content <- apply_replace_partial(content, dir)

  if (any(grepl("\\[! .* !\\]", content))) {
    content <- replace_partials(content, dir)
  }

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
#' @noRd
replace_partial <- function(line, dir) {
  if (length(line) > 1) {
    line <- apply_replace_partial(line, dir)
  }

  if (!grepl("\\[! .* !\\]", line)) {
    return(line)
  }

  # every line is enclosed in <p> tag coming from markdown
  # we remove this, it's safe to assume that is not wanted.
  line <- gsub(
    "<p>\\[\\! ?",
    "[!",
    line
  )
  line <- gsub(
    " ?\\!\\]</p>",
    "!]",
    line
  )

  path <- trimws(gsub("\\[!|!\\]", "", line))

  # construct new base directory
  new_dir <- dirname(path)
  new_dir <- file.path(dir, new_dir)

  # build new path based on new directory
  path <- basename(path)
  path <- file.path(new_dir, path)

  # read lines
  lines <- read_lines_cached(path)

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
#' @noRd
get_dir <- function(file) {
  dirname(normalizePath(file))
}

#' Replace Partial Vectorised
#'
#' Vectorised version of `replace_partial()`.
#'
#' @inheritParams replace_partials
#'
#' @keywords internal
#' @noRd
apply_replace_partial <- function(content, dir) {
  unlist(
    unname(
      sapply(content, replace_partial, dir)
    )
  )
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
#' @return Object of class "robj".
#' @examples
#' robj(1:10)
#' @export
robj <- function(obj) {
  assert_that(not_missing(obj))

  # Supress warnings otherwise
  # NULL, NA, and the likes
  # raise messages
  suppressWarnings(
    structure(obj, class = c("robj", class(obj)))
  )
}

#' @export
print.robj <- function(x, ...) {
  cli::cli_alert_info("R object")
  class(x) <- class(x)[!class(x) %in% "robj"]
  dput(x)
}

#' JSON Object
#'
#' Serialises an object to JSON in `res$render`.
#'
#' @param obj Object to serialise.
#' @examples
#' if (interactive()) {
#'   l <- list(a = "hello", b = 2L, c = 3)
#'   jobj(l)
#' }
#' @return Object of class "jobj".
#' @export
jobj <- function(obj) {
  suppressWarnings(
    structure(obj, class = c("jobj", class(obj)))
  )
}

#' @export
print.jobj <- function(x, ...) {
  cli::cli_alert_info("JSON object")
  serialise(x, ...)
}

#' Pre Hook Response
#'
#' @param content File content, a character vector.
#' @param data A list of data passed to `glue::glue_data`.
#'
#' @examples
#' my_prh <- function(self, content, data, ext, ...) {
#'   data$title <- "Mansion"
#'   pre_hook(content, data)
#' }
#'
#' #' Handler for GET at '/'
#' #'
#' #' @details Renders the homepage
#' #' @export
#' home_get <- function(req, res) {
#'   res$pre_render_hook(my_prh)
#'   res$render(
#'     file = "page.html",
#'     data = list(
#'       title = "Home"
#'     )
#'   )
#' }
#' @return A response pre-hook.
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

#' @keywords internal
is_pre_hook <- function(obj) {
  inherits(obj, "responsePreHook")
}

#' @export
print.responsePreHook <- function(x, ...) {
  cli::cli_alert_info("A response pre hook")
}

#' HTML Template
#'
#' Use [htmltools::htmlTemplate()] as renderer.
#' Passed to `use` method.
#'
#' @return A renderer function.
#' @examples
#' use_html_template()
#' @export
use_html_template <- function() {
  as_renderer(function(file, data) {
    if (tools::file_ext(file) != "html") {
      return()
    }

    data$filename <- file
    x <- do.call(
      htmltools::htmlTemplate,
      data
    )
    as.character(x)
  })
}

#' Render Tags
#'
#' @param lines Output of [read_lines()]
#' @param data Data to render, a `list`.
#'
#' @keywords internal
#' @noRd
render_tags <- function(lines, data) {
  new_lines <- c()
  n <- 0L
  str <- ""

  for (i in seq_along(lines)) {
    line <- lines[i]
    if (!grepl("\\[%|%\\]", line) && n == 0L) {
      new_lines <- c(new_lines, line)
      next()
    }

    if (!grepl("\\[%|%\\]", line) && n > 0L) {
      str <- paste(str, line)
      next()
    }

    opened <- lengths(regmatches(line, gregexpr("\\[%", line)))
    closed <- lengths(regmatches(line, gregexpr("%\\]", line)))

    n <- n + opened - closed

    str <- paste(str, line)

    # all in one line we render and continue
    if (n == 0L) {
      new_line <- glue::glue_data(data, str, .open = "[%", .close = "%]")
      new_lines <- c(new_lines, new_line)
      str <- ""
      next
    }
  }

  if (str != "") {
    message("error")
  }

  new_lines
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
render_html <- function(expr) {
  tags <- eval(parse(text = expr))

  tmp <- tempfile(fileext = ".html")
  on.exit({
    fs::file_delete(tmp)
  })

  htmltools::save_html(tags, file = tmp, background = "none")

  paste0(read_lines(tmp), collapse = "")
}

#' Create a Renderer
#'
#' Create a custom renderer.
#'
#' @param fn A function that accepts two arguments,
#' the full path to the `file` to render, and the
#' `data` to render.
#'
#' @return A renderer function.
#' @examples
#' if (interactive()) {
#'   fn <- function(path, data) {
#'     # ...
#'   }
#'
#'   as_renderer(fn)
#' }
#' @export
as_renderer <- function(fn) {
  assert_that(is_function(fn))
  assert_that(is_renderer(fn))

  structure(
    fn,
    class = c(
      "renderer",
      class(fn)
    )
  )
}

#' @export
print.renderer <- function(x, ...) {
  cli::cli_alert_info("A renderer")
}

#' Is Renderer
#'
#' Check whether an object is a renderer.
#'
#' @param obj Object to check.
#'
#' @return Boolean
#' @keywords internal
#' @noRd
is_renderer_obj <- function(obj) {
  inherits(obj, "renderer")
}
