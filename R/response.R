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
response <- function(body, headers = list(), status = 200L){
  assert_that(not_missing(body))
  res <- list(status = as.integer(status), headers = headers, body = convert_body(body))
  construct_response(res)
}

#' @rdname responses
#' @export
response_404 <- function(body = "404: Not found", headers = list("Content-Type" = content_html()), status = 404L){
  res <- list(status = as.integer(status), headers = headers, body = convert_body(body))
  construct_response(res)
}

#' @rdname responses
#' @export
response_500 <- function(body = "500: Server Error", headers = list("Content-Type" = content_html()), status = 500L){
  res <- list(status = as.integer(status), headers = headers, body = convert_body(body))
  construct_response(res)
}

#' Convert Body
#' 
#' Body may only be a character vector of length 1.
#' 
#' @param body Body of response.
#' 
#' @keywords internal
convert_body <- function(body) {
  if(inherits(body, "AsIs"))
    return(body)

  if(is.raw(body))
    return(body)

  if(is.character(body) && length(body) == 1L)
    return(body)

  if(is.factor(body) || inherits(body, "shiny.tag"))
    return(as.character(body))

  return(body)
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

#' @keywords internal
is_response <- function(obj) {
  inherits(obj, "ambiorixResponse")
}

#' Response
#' 
#' Response class to generate responses sent from the server.
#' 
#' @field status Status of the response, defaults to `200L`.
#' @field headers Named list of headers.
#'
#' @export 
Response <- R6::R6Class(
  "Response",
  lock_objects = FALSE,
  public = list(
#' @details Set the status of the response.
#' @param status An integer defining the status.
    set_status = function(status){
      assert_that(not_missing(status))
      private$.status <- status
      invisible(self)
    },
#' @details Send a plain HTML response.
#' @param body Body of the response.
    send = function(body, env = parent.frame()){
      headers <- private$.get_headers()
      resp <- response(status = private$.get_status(), headers = headers, body = convert_body(body))
      do.call(return, list(resp), envir = env)
    },
#' @details Send a plain HTML response, pre-processed with sprintf.
#' @param body Body of the response.
#' @param ... Passed to `...` of `sprintf`.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    sendf = function(body, ..., headers = NULL, status = NULL, env = parent.frame()){
      deprecated_headers(headers)
      deprecated_status(status)
      body <- sprintf(body, ...)
      headers <- private$.get_headers(headers)
      resp <- response(status = private$.get_status(status), headers = headers, body = convert_body(body))
      do.call(return, list(resp), envir = env)
    },
#' @details Send a plain text response.
#' @param body Body of the response.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    text = function(body, headers = NULL, status = NULL, env = parent.frame()){
      deprecated_headers(headers)
      deprecated_status(status)
      headers <- private$.get_headers(headers)
      headers[["Content-Type"]] <- content_plain()
      resp <- response(status = private$.get_status(status), headers = headers, body = convert_body(body))
      do.call(return, list(resp), envir = env)
    },
#' @details Send a file.
#' @param file File to send.
#' @param headers HTTP headers to set.
#' @param status Status of the response.
    send_file = function(file, headers = NULL, status = NULL, env = parent.frame()){
      deprecated_headers(headers)
      deprecated_status(status)
      assert_that(not_missing(file))
      self$render(file, data = list(), status = status, headers = headers, env = env)
    },
#' @details Redirect to a path or URL.
#' @param path Path or URL to redirect to.
#' @param status Status of the response, if `NULL` uses `self$status`.
    redirect = function(path, status = NULL, env = parent.frame()){
      deprecated_status(status)
      status <- private$.get_status(status)
      headers <- private$.get_headers(list(Location = path))
      resp <- response(status = status, headers = headers, body = "")
      do.call(return, list(resp), envir = env)
    },
#' @details Render a template file.
#' @param file Template file.
#' @param data List to fill `[% tags %]`.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    render = function(file, data = list(), headers = NULL, status = NULL, env = parent.frame()){
      assert_that(not_missing(file))
      assert_that(has_file(file))
      deprecated_status(status)
      deprecated_headers(headers)

      status <- private$.get_status(status)

      file_content <- private$.render_template(file, data)
      headers <- private$.get_headers(headers)

      resp <- response(file_content, status = private$.get_status(status), headers = headers)
      do.call(return, list(resp), envir = env)
    },
#' @details Render an object as JSON.
#' @param body Body of the response.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to the serialiser.
    json = function(body, headers = NULL, status = NULL, ..., env = parent.frame()){
      self$header_content_json()
      deprecated_headers(headers)
      deprecated_status(status)
      headers <- private$.get_headers(headers)
      resp <- response(serialise(body), headers = headers, status = private$.get_status(status))
      do.call(return, list(resp), envir = env)
    },
#' @details Sends a comma separated value file
#' @param data Data to convert to CSV.
#' @param name Name of the file.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to [readr::format_csv()].
    csv = function(data, name = "data", status = NULL, ..., env = parent.frame()){
      assert_that(not_missing(data))
      check_installed("readr")
      deprecated_status(status)

      name <- sprintf("attachment;charset=UTF-8;filename=%s.csv", name)

      headers <- list(
        "Content-Disposition" = name
      )
      self$header_content_csv()
      headers <- private$.get_headers(headers)

      data <- readr::format_csv(data, ...)
      resp <- response(data, header = headers, status = private$.get_status(status))
      do.call(return, list(resp), envir = env)
    },
#' @details Sends a tab separated value file
#' @param data Data to convert to CSV.
#' @param name Name of the file.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to [readr::format_tsv()].
    tsv = function(data, name = "data", status = NULL, ..., env = parent.frame()){
      assert_that(not_missing(data))
      check_installed("readr")
      deprecated_status(status)

      name <- sprintf("attachment;charset=UTF-8;filename=%s.tsv", name)

      headers <- list(
        "Content-Disposition" = name
      )
      self$header_content_tsv()
      headers <- private$.get_headers(headers)

      data <- readr::format_tsv(data, ...)
      resp <- response(data, header = headers, status = private$.get_status(status))
      do.call(return, list(resp), envir = env)
    },
#' @details Sends an htmlwidget.
#' @param widget The widget to use.
#' @param status Status of the response, if `NULL` uses `self$status`.
#' @param ... Additional arguments passed to [htmlwidgets::saveWidget()].
    htmlwidget = function(widget, status = NULL, ..., env = parent.frame()){
      check_installed("htmlwidgets")
      if(!inherits(widget, "htmlwidget"))
        stop("This is not an htmlwidget", call. = FALSE)
      deprecated_status(status)

      # save and read
      tmp <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(widget, tmp, selfcontained = TRUE, ...)
      on.exit({
        unlink(tmp)
      })
      headers <- private$.get_headers()

      resp <- response(body = paste0(read_lines(tmp), "\n", collapse = ""), status = private$.get_status(status), headers = headers)
      do.call(return, list(resp), envir = env)
    },
#' @details Render a markdown file.
#' @param file Template file.
#' @param data List to fill `[% tags %]`.
#' @param headers HTTP headers to set.
#' @param status Status of the response, if `NULL` uses `self$status`.
    md = function(file, data = list(), headers = NULL, status = NULL, env = parent.frame()) {
      check_installed("commonmark")
      deprecated_headers(headers)
      deprecated_status(status)
      self$render(file, data, headers, status, env = env)
    },
#' @details Send a png file
#' @param file Path to local file.
    png = function(file, env = parent.frame()){
      private$.send_image(file, "png", env = env)
    },
#' @details Send a jpeg file
#' @param file Path to local file.
    jpeg = function(file, env = parent.frame()) {
      private$.send_image(file, "jpeg", env = env)
    },
#' @details Send an image
#' Similar to `png` and `jpeg` methods but guesses correct method 
#' based on file extension.
#' @param file Path to local file.
    image = function(file) {
      type <- tools::file_ext(file)
      if(!type %in% c("png", "jpeg"))
        stop("Only accepts .png and .jpeg files")
      private$.send_image(file, type)
    },
#' @details Ggplot2
#' @param plot Ggplot2 plot object.
#' @param type Type of image to save.
#' @param ... Passed to [ggplot2::ggsave()]
    ggplot2 = function(plot, ..., type = c("png", "jpeg"), env = parent.frame()) {
      assert_that(not_missing(plot))
      check_installed("ggplot2")

      type <- match.arg(type)
      ext <- sprintf(".%s", type)
      temp <- tempfile(fileext = ext)
      ggplot2::ggsave(
        temp,
        plot, 
        ...
      ) 
      private$.send_image(temp, type, clean = TRUE, env = env)
    },
#' @details Print
    print = function(){
      cli::cli_h3("A Response")

      if(!length(private$.headers))
        return(invisible())
      
      cli::cli_h3("Headers")
      cli::cli_ul()

      for(i in seq_along(private$.headers)) {
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
      .Deprecated(
        "",
        package = "ambiorix",
        "Deprecated. The environment is no longer locked, you may simply `res$name <- value`"
      )

      name <- as_label(name)
      self[[name]] <- value

      invisible(self)
    },
#' @details Get data
#' @param name Name of the variable to get.
    get = function(name){
      assert_that(not_missing(name))
      .Deprecated(
        "",
        package = "ambiorix",
        "Deprecated. The environment is no longer locked, you may simply `req$value"
      )

      name <- as_label(name)
      self[[name]]
    },
#' @details Add headers to the response.
#' @param name,value Name and value of the header.
#' @return Invisibly returns self.
    header = function(name, value){
      assert_that(not_missing(name))
      assert_that(not_missing(value))
      name <- as_label(name)
      private$.headers[[name]] <- value
      invisible(self)
    },
#' @details Set Content Type to JSON
#' @return Invisibly returns self.
    header_content_json = function(){
      self$header("Content-Type", content_json())
      invisible(self)
    },
  #' @details Set Content Type to HTML
  #' @return Invisibly returns self.
    header_content_html = function(){
      self$header("Content-Type", content_html())
      invisible(self)
    },
  #' @details Set Content Type to Plain Text
  #' @return Invisibly returns self.
    header_content_plain = function(){
      self$header("Content-Type", content_plain())
      invisible(self)
    },
  #' @details Set Content Type to CSV
  #' @return Invisibly returns self.
    header_content_csv = function(){
      self$header("Content-Type", content_csv())
      invisible(self)
    },
  #' @details Set Content Type to TSV
  #' @return Invisibly returns self.
    header_content_tsv = function(){
      self$header("Content-Type", content_tsv())
      invisible(self)
    },
#' @details Get headers
#' Returns the list of headers currently set.
    get_headers = function() {
      return(private$.headers)
    },
#' @details Get a header
#' Returns a single header currently, `NULL` if not set.
#' @param name Name of the header to return.
    get_header = function(name) {
      assert_that(not_missing(name))
      return(private$.headers[[name]])
    },
#' @details Set headers
#' @param headers A named list of headers to set.
    set_headers = function(headers) {
      assert_that(not_missing(headers))
      if(!is.list(headers))
        stop("`headers` must be a named list")

      private$.headers <- headers
      invisible(self)
    },
    #' @details Set a Header
    #' @param name Name of the header.
    #' @param value Value to set.
    #' @return Invisible returns self.
    set_header = function(name, value) {
      assert_that(not_missing(name))
      assert_that(not_missing(value))

      .Deprecated(
        "header",
        package = "ambiorix",
        "Deprecated. This is deprecated, use the `header()` method."
      )

      private$.headers[[name]] <- value
      invisible(self)
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
#' Overwrites existing cookie of the same `name`.
#' @param name Name of the cookie.
#' @param value value of the cookie.
#' @param expires Expiry, if an integer assumes it's the number of seconds
#' from now. Otherwise accepts an object of class `POSIXct` or `Date`.
#' If a `character` string then it is set as-is and not pre-processed.
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
#' through the document.cookie property.
#' @param same_site Controls whether or not a cookie is sent with cross-origin
#' requests, providing some protection against cross-site request forgery
#' attacks (CSRF). Accepts `Strict`, `Lax`, or `None`.
#' @return Invisibly returns self.
    cookie = function(
      name,
      value,
      expires = getOption("ambiorix.cookie.expire"),
      max_age = getOption("ambiorix.cookie.maxage"),
      domain = getOption("ambiorix.cookie.domain"),
      path = getOption("ambiorix.cookie.path", "/"),
      secure = getOption("ambiorix.cookie.secure", TRUE),
      http_only = getOption("ambiorix.cookie.httponly", TRUE),
      same_site = getOption("ambiorix.cookie.savesite")
    ) {
      assert_that(not_missing(name))
      assert_that(not_missing(value))

      if(length(.globals$cookiePreprocessors) > 0) {
        for(i in 1:length(.globals$cookiePreprocessors)) {
          value <- .globals$cookiePreprocessors[[i]](
            name,
            value,
            expires,
            max_age,
            domain,
            path,
            secure,
            http_only,
            same_site
          )
        }
      }

      name <- as_label(name)
      private$.cookies[[name]] <- cookie(
        name,
        value,
        expires,
        max_age,
        domain,
        path,
        secure,
        http_only,
        same_site
      )

      invisible(self)
    },
#' @details Clear a cookie
#' Clears the value of a cookie.
#' @param name Name of the cookie to clear.
#' @return Invisibly returns self.
    clear_cookie = function(name) {
      # cookies with date in the past are removed from the browser
      self$cookie(
        name,
        "",
        expires = Sys.Date() - 365L
      )

      invisible(self)
    }
  ),
  active = list(
    status = function(value) {
      if(missing(value))
        return(private$.status)

      private$.status <- as.integer(value)
    },
    headers = function(value) {
      if(missing(value))
        return(private$.headers)

      if(!is.list(value))
        stop("Must be a `list`")

      private$.headers <- value
    }
  ),
  private = list(
    .status = 200L,
    .cookies = list(),
    .headers = list(), 
    .preHooks = list(),
    .postHooks = list(),
    .render_template = function(file, data){
      file <- normalizePath(file)

      if(!is.null(.globals$renderer)) {
        response <- .globals$renderer(file, data)
        # if the response is NULL we keep going
        if(isTRUE(!is.null(response)))
          return(response)
      }

      # read and replace tags
      file_content <- read_lines_cached(file)

      # render
      ext <- tools::file_ext(file)

      # handle partials
      # replace brackets so glue::glue_data evals
      file_content <- replace_partials(file_content, get_dir(file))

      if(ext == "html") {
        data <- lapply(data, \(x) {
          if(!inherits(x, "jobj"))
            return(x)

          serialise(x) |> 
            as.character()
        })
      }

      # hooks
      if(length(private$.preHooks) > 0) {
        for(i in 1:length(private$.preHooks)) {
          pre_processed <- private$.preHooks[[i]](self, file_content, data, ext)
          if(!is_pre_hook(pre_processed)){
            cat(error(), "Not a valid return value from pre-hook (ignoring)\n")
            next
          }

          file_content <- pre_processed$content
          data <- pre_processed$data
        }
      }

      file_content <- render_tags(file_content, data)
      
      if(ext == "md")
        file_content <- commonmark::markdown_html(file_content)

      # collapse html
      if(ext == "html" || ext == "md")
        return(private$.run_post_hooks(paste0(file_content, collapse = ""), ext))

      # parse R
      private$.run_post_hooks(render_html(file_content), ext)
    },
    .get_status = function(status = NULL){
      if(is.null(status))
        return(private$.status)

      status
    },
    .get_headers = function(headers = NULL){
      private$.render_cookies()
      heads <- private$.headers

      if(!is.null(headers))
        heads <- modifyList(heads, headers)

      if(is.null(heads[["Content-Type"]]))
        heads <- modifyList(heads, list("Content-Type" = content_html()))

      return(heads)
    },
    .run_post_hooks = function(file_content, ext) {
      if(length(private$.postHooks) == 0) {
        return(file_content)
      }

      for(i in 1:length(private$.postHooks)) {
        content <- private$.postHooks[[i]](self, file_content, ext)

        if(!is.character(content)){
          cat(error(), "Not a character vector returned from post-hook (ignoring)\n", stdout())
          next
        }

        file_content <- content
      }

      return(file_content)
    },
    .render_cookies = function() {
      if(length(private$.cookies) == 0L)
        return()

      for(opts in private$.cookies) {
        cookie <- sprintf("%s=%s", opts$name, opts$value)

        if(!is.null(opts$expires)) {
          expires <- convert_cookie_expires(opts$expires)
          cookie <- sprintf("%s; Expires=%s", cookie, expires)
        }

        if(!is.null(opts$max_age))
          cookie <- sprintf("%s; Max-Age=%s", cookie, opts$max_age)

        if(!is.null(opts$domain))
          cookie <- sprintf("%s; Domain=%s", cookie, opts$domain)

        if(!is.null(opts$path))
          cookie <- sprintf("%s; Path=%s", cookie, opts$path)

        if(opts$secure)
          cookie <- sprintf("%s; Secure", cookie)

        if(opts$http_only)
          cookie <- sprintf("%s; HttpOnly", cookie)

        if(!is.null(opts$same_site)) 
          cookie <- sprintf("%s; SameSite=%s", cookie, opts$same_site)

        names(cookie) <- "Set-Cookie"

        private$.headers <- append(
          private$.headers,
          as.list(cookie)
        )
      }
    },
    .send_image = function(file, type = c("png", "jpeg"), clean = FALSE, env = parent.frame()) {
      assert_that(not_missing(file))

      if(grepl("http", file))
        stop("Must be a local file", call. = FALSE)

      if(clean) {
        on.exit({
          unlink(file)
        })
      }

      type <- match.arg(type)
      type <- sprintf("image/%s", type)

      size <- file.info(file)$size
      con <- file(file, "rb")
      on.exit({
        close(con)
      }, add = TRUE)

      raw <- readBin(con, raw(), size)

      self$header("Content-Length", size)
      self$header("Content-Type", type)
      self$send(raw, env = env)
    }
  )
)

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

deprecated_headers <- function(headers = NULL) {
  if(is.null(headers))
    return()

  .Deprecated(
    "header",
    package = "ambiorix",
    msg = "Deprecated. Pass headers with the `header()` method."
  )
}

deprecated_status <- function(status = NULL) {
  if(is.null(status))
    return()

  .Deprecated(
    "status",
    package = "ambiorix",
    msg = "Deprecated. Pass status to the `status` binding, e.g.: `res$status <- 404L`."
  )
}
