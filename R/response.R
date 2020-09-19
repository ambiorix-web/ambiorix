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
  assert_that(not_missing(body))
  res <- list(status = as.integer(status), headers = headers, body = as.character(body))
  construct_response(res)
}

#' @rdname responses
#' @export
response_404 <- function(body = "404: Not found", headers = list('Content-Type' = 'text/html'), status = 404L){
  res <- list(status = as.integer(status), headers = headers, body = as.character(body))
  construct_response(res)
}

#' @rdname responses
#' @export
response_500 <- function(body = "500: Server Error", headers = list('Content-Type' = 'text/html'), status = 500L){
  res <- list(status = as.integer(status), headers = headers, body = as.character(body))
  construct_response(res)
}

#' Construct Response
#' 
#' @noRd 
#' @keywords internal
construct_response <- function(res){
  structure(res, class = c(class(res), "ambiorixResponse"))
}

#' @export
print.ambiorixResponse <- function(x, ...){
  cat("An ambiorix response")
}

#' Response
#' 
#' @noRd 
#' @keywords internal
Response <- R6::R6Class(
  "Response",
  public = list(
    initialize = function(){
      template_path <- here::here("templates")
      private$.has_templates <- fs::dir_exists(template_path)

      if(!private$.has_templates)
        return(self)
    },
#' @details Set the status of the response.
#' @param status An integer defining the status.
    status = function(status){
      assert_that(not_missing(status))
      private$.status <- status
      invisible(self)
    },
#' @details Send a plain response.
#' @param body Body of the response.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    send = function(body, headers = list('Content-Type' = 'text/html'), status = NULL){
      response(status = private$.get_status(status), headers = headers, body = as.character(body))
    },
#' @details Send a file.
#' @param file File to send.
#' @param headers HTTP headers to set.
    send_file = function(file, status = NULL){
      assert_that(not_missing(file))

      self$render(file, data = list(), status = private$.get_status(status))
    },
#' @details Redirect to a path or URL.
#' @param path Path or URL to redirect to.
#' @param status Status of the response, if `NULL` uses `self$status`.
    redirect = function(path, status = NULL){
      status <- private$.get_status(status)
      if(!grepl("^3", status))
        status <- 302L

      response(status = status, headers = list(Location = path), body = "")
    },
#' @details Render a template file. 
#' @param file Template file.
#' @param data List to fill `[% tags %]`.
#' @param status Status of the response, if `NULL` uses `self$status`.
    render = function(file, data = list(), status = NULL){
      assert_that(not_missing(file))

      if(!private$.has_templates)
        stop("No templates directory found", call. = FALSE)

      file_path <- private$.get_template_path(file)

      file_content <- private$.render_template(file_path, data)

      response(file_content, status = private$.get_status(status))
    },
#' @details Render an object as JSON.
#' @param body Body of the response.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    json = function(body, headers = list("Content-Type" = "application/json"), status = NULL, ...){
      to_json <- get_serialise(...)
      response(to_json(body), headers = headers, status = private$.get_status(status))
    },
#' @details Sends a comma separated value file
#' @param data Data to convert to CSV.
#' @param name Name of the file.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to [readr::format_csv()].
    csv = function(data, name = "data", status = NULL, ...){
      assert_that(not_missing(data))
      check_installed("readr")

      name <- sprintf("attachment;charset=UTF-8;filename=%s.csv", name)

      header <- list(
        "Content-Type" = "text/csv",
        "Content-Disposition" = name
      )

      data <- readr::format_csv(data, ...)
      response(data, header = header, status = private$.get_status(status))
    },
#' @details Sends a tab separated value file
#' @param data Data to convert to CSV.
#' @param name Name of the file.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to [readr::format_tsv()].
    tsv = function(data, name = "data", status = NULL, ...){
      assert_that(not_missing(data))
      check_installed("readr")

      name <- sprintf("attachment;charset=UTF-8;filename=%s.tsv", name)

      header <- list(
        "Content-Type" = "tab-separated-values",
        "Content-Disposition" = name
      )

      data <- readr::format_tsv(data, ...)
      response(data, header = header, status = private$.get_status(status))
    },
#' @details Sends an htmlwidget.
#' @param widget The widget to use.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to [htmlwidgets::saveWidget()].
    htmlwidget = function(widget, status = NULL, ...){
      check_installed("htmlwidgets")
      if(!inherits(widget, "htmlwidget"))
        stop("This is not an htmlwidget", call. = FALSE)
      
      # save and read
      tmp <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp, selfcontained = TRUE, ...)

      response(body = paste0(readLines(tmp), collapse = ""), status = private$.get_status(status))
    },
    print = function(){
      cli::cli_li("{.code send(body, headers, status)}")
      cli::cli_li("{.code send_file(file, status)}")
      cli::cli_li("{.code render(file, data, status)}")
      cli::cli_li("{.code json(body, headers, status)}")
      cli::cli_li("{.code redirect(path, status)}")
      cli::cli_li("{.code status(status)}")
      cli::cli_li("{.code csv(data, name, ...)}")
      cli::cli_li("{.code tsv(data, name, ...)}")
      cli::cli_li("{.code rds(data, name, ...)}")
      cli::cli_li("{.code htmlwidget(widget, ...)}")
    }
  ),
  private = list(
    .has_templates = FALSE,
    .templates = list(),
    .status = 200L,
    .get_template_path = function(file){
      file <- remove_extensions(file)

      # should be recursive?
      # try HTML first
      file_html <- private$.try_template_path(file, ".html")

      if(!is.null(file_html)) return(file_html)

      # try R
      file_r <- private$.try_template_path(file, ".R")

      if(is.null(file_r)){
        msg <- sprintf("Cannot find %s.html nor %s.R in templates", file, file)
        stop(msg)
      }

      return(file_r)
    },
    .try_template_path = function(file, ext = c(".html", ".R")){
      ext <- match.arg(ext)
      path <- here::here("templates", paste0(file, ext))

      if(fs::file_exists(path))
        return(path)

      return(NULL)
    },
    .render_template = function(file, data){
      # read and replace tags
      file_content <- readLines(file)

      # render
      ext <- tools::file_ext(file)

      # handle partials
      # replace brackets so glue::glue_data evals
      file_content <- replace_partials(file_content, ext = ext)

      if(ext == "html"){

        # needs serialisation
        to_json <- get_serialise()

        # serialise to each object individually
        data <- lapply(data, function(x){
          to_json(x)
        })
      } else {
        data <- lapply(data, function(x){

        # If not AsIs can use object
        if(!inherits(x, "robj"))
          return(x)

        paste0(
          capture.output(
            dput(x)
          ), 
          collapse = ""
        )
        })
      }

      file_content <- lapply(file_content, function(x, data){
        glue::glue_data(data, x, .open = "[%", .close = "%]")
      }, data = data)

      # collapse html
      if(ext == "html")
        return(paste0(file_content, collapse = ""))

      # parse R
      render_html(file_content)

    },
    .make_template_path = function(file){
      # clean input
      file <- remove_extensions(file)

      # list possible files
      files <- remove_extensions(private$.templates)

      template <- sprintf("./templates/%s.html", template)
      normalizePath(template)
    },
    .get_status = function(status){
      if(is.null(status))
        return(private$.status)
      
      status
    }
  )
)

#' Data Object
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
  class(x) <- class(x)[class(x) != "robj"]
  print(x)
}
