#' Plain Responses
#'
#' Plain HTTP Responses.
#'
#' @param body Body of response.
#' @param headers HTTP headers.
#' @param status Response status
#' 
#' @examples
#' app <- Ambiorix$new()
#' 
#' # html
#' app$get("/", function(req, res){
#'  res$send("hello!")
#' })
#' 
#' # text
#' app$get("/text", function(req, res){
#'  res$text("hello!")
#' })
#' 
#' if(interactive())
#'  app$start()
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
#' Response class to generate responses sent from the server.
#'
#' @export 
Response <- R6::R6Class(
  "Response",
  lock_objects = FALSE,
  public = list(
#' @details Set the status of the response.
#' @param status An integer defining the status.
    status = function(status){
      assert_that(not_missing(status))
      private$.status <- status
      invisible(self)
    },
#' @details Send a plain HTML response.
#' @param body Body of the response.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    send = function(body, headers = list('Content-Type' = 'text/html'), status = NULL){
      headers <- private$.get_headers(headers)
      response(status = private$.get_status(status), headers = headers, body = as.character(body))
    },
#' @details Send a plain HTML response, pre-processed with sprintf.
#' @param body Body of the response.
#' @param ... Passed to `...` of `sprintf`.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    sendf = function(body, ..., headers = list('Content-Type' = 'text/html'), status = NULL){
      body <- sprintf(body, ...)
      headers <- private$.get_headers(headers)
      response(status = private$.get_status(status), headers = headers, body = as.character(body))
    },
#' @details Send a plain text response.
#' @param body Body of the response.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    text = function(body, headers = list('Content-Type' = 'text/plain'), status = NULL){
      headers <- private$.get_headers(headers)
      response(status = private$.get_status(status), headers = headers, body = as.character(body))
    },
#' @details Send a file.
#' @param file File to send.
#' @param headers HTTP headers to set.
#' @param status Status of the response.
    send_file = function(file, headers = list('Content-Type' = 'text/html'), status = NULL){
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
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    render = function(file, data = list(), headers = list('Content-Type' = 'text/html'), status = NULL){
      assert_that(not_missing(file))
      assert_that(has_file(file))

      file_content <- private$.render_template(file, data)
      headers <- private$.get_headers(headers)

      response(file_content, status = private$.get_status(status), headers = headers)
    },
#' @details Render an object as JSON.
#' @param body Body of the response.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to the serialiser.
    json = function(body, headers = list("Content-Type" = "application/json"), status = NULL, ...){
      to_json <- get_serialise(...)
      headers <- private$.get_headers(headers)
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

      headers <- list(
        "Content-Type" = "text/csv",
        "Content-Disposition" = name
      )
      headers <- private$.get_headers(headers)

      data <- readr::format_csv(data, ...)
      response(data, header = headers, status = private$.get_status(status))
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

      headers <- list(
        "Content-Type" = "tab-separated-values",
        "Content-Disposition" = name
      )
      headers <- private$.get_headers(headers)

      data <- readr::format_tsv(data, ...)
      response(data, header = headers, status = private$.get_status(status))
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
      headers <- private$.get_headers(headers)

      response(body = paste0(read_lines(tmp), "\n", collapse = ""), status = private$.get_status(status), headers = headers)
    },
#' @details Render a markdown file.
#' @param file Template file.
#' @param data List to fill `[% tags %]`.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    md = function(file, data = list(), headers = list('Content-Type' = 'text/html'), status = NULL) {
      check_installed("commonmark")
      assert_that(not_missing(file))

      file_content <- private$.render_template(file, data)
      headers <- private$.get_headers(headers)

      response(file_content, status = private$.get_status(status), headers = headers)
    },
#' @details Add headers to the response.
#' @param name,value Name and value of the header.
#' @return Invisibly returns self.
    header = function(name, value){
      name <- as_label(name)
      private$.headers[[name]] <- value
      invisible(self)
    },
#' @details Print
    print = function(){
      cli::cli_h3("A Response")

      if(!length(private$.headers))
        return(invisible())
      
      cli::cli_h3("Headers")
      cli::cli_ul()

      for(i in 1:length(private$.headers)) {
        cli::cli_li("HEADER {names(private$.headers)[i]}")
        str(private$.headers[[i]])
      }

      cli::cli_end()
    },
#' @details Set Data
#' @param name Name of the variable.
#' @param value Value of the variable.
#' @return Invisible returns self.
    set = function(name, value){
      assert_that(not_missing(name))
      assert_that(not_missing(value))

      name <- as_label(name)
      self[[name]] <- value

      invisible(self)
    },
#' @details Get data
#' @param name Name of the variable to get.
    get = function(name){
      assert_that(not_missing(name))

      name <- as_label(name)
      self[[name]]
    },
#' @details Get headers
#' Returns the list of headers currently set.
    get_headers = function() {
      return(private$.headers)
    },
#' @details Add a pre render hook.
#' Runs before the `render` and `send_file` method.
#' 
#' @param hook A function that accepts at least 4 arguments:
#' - `self`: The `Request` class instance.
#' - `content`: File content a vector of character string,
#' content of the template.
#' - `data`: `list` passed from `render` method.
#' - `ext`: File extension of the template file.
#' 
#' This function is used to add pre-render hooks to the `render`
#' method. The function should return an object of class 
#' `responsePreHook` as obtained by [pre_hook()].
#' This is meant to be used by middlewares to, if necessary,
#' pre-process rendered data.
#' 
#' Include `...` in your `hook` to ensure it will handle
#' potential updates to hooks in the future.
#' 
#' @return Invisible returns self.
    pre_render_hook = function(hook) {
      assert_that(not_missing(hook))
      assert_that(
        is.function(hook),
        msg = "`hook` must be a function"
      )

      assert_that(
        length(formalArgs(hook)) >= 4,
        msg = "`hook` must take at least 4 arguments: `self`, `content`, `data`, and `ext`"
      )

      private$.preHooks <- append(private$.preHooks, hook)
      invisible(self)
    },
#' @details Post render hook.
#' 
#' @param hook A function to run after the rendering of HTML.
#' It should accept at least 3 arguments:
#' - `self`: The `Request` class instance.
#' - `content`: File content a vector of character string,
#' content of the template.
#' - `ext`: File extension of the template file.
#' 
#' Include `...` in your `hook` to ensure it will handle
#' potential updates to hooks in the future.
#' 
#' @return Invisible returns self.
    post_render_hook = function(hook) {
      assert_that(not_missing(hook))
      assert_that(
        is.function(hook),
        msg = "`hook` must be a function"
      )

      assert_that(
        length(formalArgs(hook)) >= 3,
        msg = "`hook` must take 2 arguments: `self`, `content`, and `ext`"
      )
      
      private$.postHooks <- append(private$.postHooks, hook)
      invisible(self)
    },
#' @details Set a cookie
#' @param name Name of the cookie.
#' @param value value of the cookie.
#' @param expires Expiry, if an integer assumes it's the number of seconds
#' from now. Otherwise accepts an object of class `POSIXct` or `Date`.
#' If unspecified, the cookie becomes a session cookie. A session finishes 
#' when the client shuts down, after which the session cookie is removed. 
#' @param max_age Indicates the number of seconds until the cookie expires. 
#' A zero or negative number will expire the cookie immediately. 
#' If both `expires` and `max_age` are set, the latter has precedence.
#' @param domain Defines the host to which the cookie will be sent.
#' If omitted, this attribute defaults to the host of the current document URL,
#' not including subdomains.
#' @param path Indicates the path that must exist in the requested URL for the 
#' browser to send the Cookie header.
#' @param secure Indicates that the cookie is sent to the server only when a
#' request is made with the https: scheme (except on localhost), and therefore, 
#' is more resistant to man-in-the-middle attacks.
#' @param http_only Forbids JavaScript from accessing the cookie, for example,
#' through the Document.cookie property.
#' @param same_site Controls whether or not a cookie is sent with cross-origin
#' requests, providing some protection against cross-site request forgery
#' attacks (CSRF). Accepts `Strict`, `Lax`, or `None`.
    cookie = function(
      name,
      value,
      expires = NULL,
      max_age = NULL,
      domain = NULL,
      path = NULL,
      secure = TRUE,
      http_only = FALSE,
      same_site = NULL
    ) {
      assert_that(not_missing(name))
      assert_that(not_missing(value))

      name <- as_label(name)
      cookie <- sprintf("%s=%s", name, value)

      if(!is.null(expires)) {
        expires <- convert_cookie_expires(expires)
        cookie <- sprintf("%s; Expires=%s", cookie, expires)
      }

      if(!is.null(max_age)) {
        cookie <- sprintf("%s; Max-Age=%s", cookie, max_age)
      }

      if(!is.null(domain)) {
        cookie <- sprintf("%s; Domain=%s", cookie, domain)
      }

      if(!is.null(path)) {
        cookie <- sprintf("%s; Path=%s", cookie, path)
      }

      if(secure) {
        cookie <- sprintf("%s; Secure", cookie)
      }

      if(http_only) {
        cookie <- sprintf("%s; HttpOnly", cookie)
      }

      if(!is.null(same_site)) {
        cookie <- sprintf("%s; SameSite=%s", cookie, same_site)
      }

      private$.headers[["Set-Cookie"]] <- cookie

      invisible(self)
    }
  ),
  private = list(
    .templates = list(),
    .status = 200L,
    .headers = list(), 
    .preHooks = list(),
    .postHooks = list(),
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
    .render_template = function(file, data){
      # read and replace tags
      file_content <- read_lines(file)

      # render
      ext <- tools::file_ext(file)

      # handle partials
      # replace brackets so glue::glue_data evals
      file_content <- replace_partials(file, file_content, ext = ext)

      if(ext == "md")
        file_content <- commonmark::markdown_html(file_content)

      if(ext == "html"){

        # needs serialisation
        to_json <- get_serialise()

        # serialise to each object individually
        data <- lapply(data, function(x){
          to_json(x)
        })
      } 

      if(ext != "html")
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

      # hooks
      if(length(private$.preHooks) > 0) {
        for(i in 1:length(private$.preHooks)) {
          pre_processed <- private$.preHooks[[i]](self, file_content, data, ext)
          if(!inherits(pre_processed, "responsePreHook")){
            cat("Not a valid response from pre-hook (ignoring)\n", stdout())
            next
          }

          file_content <- pre_processed$content
          data <- pre_processed$data
        }
      }

      file_content <- lapply(file_content, function(x, data){
        glue::glue_data(data, x, .open = "[%", .close = "%]")
      }, data = data)

      # collapse html
      if(ext == "html" || ext == "md")
        return(private$.run_post_hooks(paste0(file_content, collapse = ""), ext))

      # parse R
      private$.run_post_hooks(render_html(file_content), ext)
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
    },
    .get_headers = function(headers = list()){
      modifyList(private$.headers, headers)
    },
    .run_post_hooks = function(file_content, ext) {
      if(length(private$.postHooks) == 0) {
        return(file_content)
      }

      for(i in 1:length(private$.postHooks)) {
        content <- private$.postHooks[[i]](self, file_content, ext)

        if(!is.character(content)){
          cat("Not a character response from post-hook (ignoring)\n", stdout())
          next
        }

        file_content <- content
      }

      return(file_content)
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

#' Convert Cookie Expires
#' 
#' Converts the cookie `expires` argument
#' to the expected
#' [Date format](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date).
#' 
#' @param expires Expiry, if an integer assumes it's the number of seconds
#' from now. Otherwise accepts an object of class `POSIXct` or `Date`.
#' 
#' @examples 
#' # expires in an hour
#' convert_cookie_expires(60 * 60)
#' 
#' # expires tomorrow
#' convert_cookie_expires(Sys.Date() + 1)
#' 
#' # expires in 1 minute
#' convert_cookie_expires(Sys.time() + 60)
#' 
#' @noRd 
#' @keywords internal
convert_cookie_expires <- function(expires) {
  if(is.character(expires))
    return(expires)

  if(is.numeric(expires)) {
    expires <- as.POSIXct(Sys.time(), tz = "UTC") + expires
  }

  if(inherits(expires, "Date") || inherits(expires, "POSIXct")) {
    expires <- as.POSIXct(expires, tz = "UTC")
    expires <- format(
      expires,
      "%a, %d %b %Y %H:%M:%S GMT"
    )
  }

  return(expires)
}
