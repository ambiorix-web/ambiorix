#' Parse multipart form data
#'
#' Parses multipart form data, including file uploads, and returns the parsed fields as a list.
#'
#' @param req Request /// Required. \cr
#'            The [Request] whose body is parsed.
#'
#' @param ... Key=Value pairs /// Optional. \cr
#'            Additional parameters passed to the parser function.
#'
#' @details
#' If a field is a file upload it is returned as a named list with:
#' - `value`: Raw vector representing the file contents. You must
#'    process this further (eg. convert to data.frame). See the examples section.
#' - `content_disposition`: Typically "form-data", indicating how the content
#'    is meant to be handled.
#' - `content_type`: MIME type of the uploaded file (e.g., "image/png" or "application/pdf").
#' - `name`: Name of the form input field.
#' - `filename`: Original name of the uploaded file.
#'
#' If no body data, an empty list is returned.
#'
#' ### Overriding Default Parser
#'
#' By default, `parse_multipart()` uses [webutils::parse_http()] internally.
#' You can override this globally by setting the `AMBIORIX_MULTIPART_FORM_DATA_PARSER` option:
#'
#' ```r
#' options(AMBIORIX_MULTIPART_FORM_DATA_PARSER = my_custom_parser)
#' ```
#'
#' Your custom parser function must accept the following parameters:
#' 1. `body`: Raw vector containing the form data.
#' 2. `content_type`: The 'Content-Type' header of the request as defined by the client.
#' 3. `...`: Additional optional parameters.
#'
#' @examples
#' if (interactive()) {
#'   library(ambiorix)
#'   library(htmltools)
#'   library(readxl)
#'
#'   page_links <- function() {
#'     Map(
#'       f = function(href, label) {
#'         tags$a(href = href, label)
#'       },
#'       c("/", "/about", "/contact"),
#'       c("Home", "About", "Contact")
#'     )
#'   }
#'
#'   forms <- function() {
#'     form1 <- tags$form(
#'       action = "/url-form-encoded",
#'       method = "POST",
#'       enctype = "application/x-www-form-urlencoded",
#'       tags$h4("form-url-encoded:"),
#'       tags$label(`for` = "first_name", "First Name"),
#'       tags$input(id = "first_name", name = "first_name", value = "John"),
#'       tags$label(`for` = "last_name", "Last Name"),
#'       tags$input(id = "last_name", name = "last_name", value = "Coene"),
#'       tags$button(type = "submit", "Submit")
#'     )
#'
#'     form2 <- tags$form(
#'       action = "/multipart-form-data",
#'       method = "POST",
#'       enctype = "multipart/form-data",
#'       tags$h4("multipart/form-data:"),
#'       tags$label(`for` = "email", "Email"),
#'       tags$input(id = "email", name = "email", value = "john@mail.com"),
#'       tags$label(`for` = "framework", "Framework"),
#'       tags$input(id = "framework", name = "framework", value = "ambiorix"),
#'       tags$label(`for` = "js-framework", "JS Framework"),
#'       tags$select(
#'         id = "js-framework",
#'         name = "js-framework",
#'         multiple = NA,
#'         tags$option(
#'           value = "js",
#'           selected = NA,
#'           "JavaScript"
#'         ),
#'         tags$option(
#'           value = "node",
#'           selected = NA,
#'           "Node.js"
#'         )
#'       ),
#'       tags$label(`for` = "file", "Upload CSV file"),
#'       tags$input(type = "file", id = "file", name = "file", accept = ".csv"),
#'       tags$label(`for` = "file2", "Upload xlsx file"),
#'       tags$input(type = "file", id = "file2", name = "file2", accept = ".xlsx"),
#'       tags$button(type = "submit", "Submit")
#'     )
#'
#'     tagList(form1, form2)
#'   }
#'
#'   home_get <- function(req, res) {
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("hello, world!"),
#'       forms()
#'     )
#'
#'     res$send(html)
#'   }
#'
#'   home_post <- function(req, res) {
#'     body <- req$parse_json()
#'     # print(body)
#'
#'     response <- list(
#'       code = 200L,
#'       msg = "hello, world"
#'     )
#'     res$json(response)
#'   }
#'
#'   url_form_encoded_post <- function(req, res) {
#'     body <- req$parse_form_urlencoded()
#'     # print(body)
#'
#'     list_items <- lapply(
#'       X = seq_along(body),
#'       FUN = function(idx) {
#'         nm <- names(body)[[idx]]
#'
#'         tags$li(
#'           nm,
#'           ":",
#'           body[[nm]]
#'         )
#'       }
#'     )
#'     input_vals <- tags$ul(list_items)
#'
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("Request processed"),
#'       input_vals
#'     )
#'
#'     res$send(html)
#'   }
#'
#'   multipart_form_data_post <- function(req, res) {
#'     body <- req$parse_multipart()
#'
#'     list_items <- lapply(
#'       X = seq_along(body),
#'       FUN = function(idx) {
#'         nm <- names(body)[[idx]]
#'         field <- body[[idx]]
#'
#'         # if 'field' is a file, parse it & print on console:
#'         is_file <- "filename" %in% names(field)
#'         is_csv <- is_file && identical(field[["content_type"]], "text/csv")
#'         is_xlsx <- is_file &&
#'           identical(
#'             field[["content_type"]],
#'             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
#'           )
#'
#'         if (is_file) {
#'           file_path <- tempfile()
#'           writeBin(object = field$value, con = file_path)
#'           on.exit(unlink(x = file_path))
#'         }
#'
#'         if (is_csv) {
#'           # print(read.csv(file = file_path))
#'         }
#'
#'         if (is_xlsx) {
#'           # print(readxl::read_xlsx(path = file_path))
#'         }
#'
#'         out <- ""
#'
#'         if (is_file) {
#'           out <- "printed on console"
#'         }
#'
#'         if (!is_file) {
#'           out <- paste(field, collapse = ", ")
#'         }
#'
#'         tags$li(
#'           nm,
#'           ":",
#'           out
#'         )
#'       }
#'     )
#'     input_vals <- tags$ul(list_items)
#'
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("Request processed"),
#'       input_vals
#'     )
#'
#'     res$send(html)
#'   }
#'
#'   about_get <- function(req, res) {
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("About Us")
#'     )
#'     res$send(html)
#'   }
#'
#'   contact_get <- function(req, res) {
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("Get In Touch!")
#'     )
#'     res$send(html)
#'   }
#'
#'   app <- Ambiorix$new(port = 3000L)
#'   app$get("/", home_get)
#'   app$post("/", home_post)
#'   app$get("/about", about_get)
#'   app$get("/contact", contact_get)
#'   app$post("url-form-encoded", url_form_encoded_post)
#'   app$post("/multipart-form-data", multipart_form_data_post)
#'
#'   app$start()
#' }
#' @seealso [parse_form_urlencoded()], [parse_json()]
#' @return Named list, or `NULL` when the request has no body.
#' @export
parse_multipart <- function(req, ...) {
  on.exit(req$rook.input$rewind())
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(NULL)
  }

  default <- function(body, content_type, ...) {
    webutils::parse_http(
      body = body,
      content_type = content_type,
      ...
    )
  }

  parser <- getOption(
    x = "AMBIORIX_MULTIPART_FORM_DATA_PARSER",
    default = default
  )
  parsed <- parser(body = body, content_type = req$CONTENT_TYPE, ...)

  if (!identical(parser, default)) {
    return(parsed)
  }

  raw_to_char <- function(x) rawToChar(as.raw(x))

  values <- lapply(
    X = parsed,
    FUN = function(item) {
      # return files as is:
      is_file <- "filename" %in% names(item)
      if (is_file) {
        return(item)
      }

      raw_to_char(item[["value"]])
    }
  )

  values
}

#' Parse application/x-www-form-urlencoded data
#'
#' @description
#' This function parses `application/x-www-form-urlencoded` data, typically used in form submissions.
#'
#' @param req Request /// Required. \cr
#'            The [Request] whose body is parsed.
#'
#' @param ... Key=Value pairs /// Optional. \cr
#'            Additional parameters passed to the parser function.
#'
#' @details
#'
#' ### Overriding Default Parser
#'
#' By default, `parse_form_urlencoded()` uses [webutils::parse_http()].
#' You can override this globally by setting the `AMBIORIX_FORM_URLENCODED_PARSER` option:
#'
#' ```r
#' options(AMBIORIX_FORM_URLENCODED_PARSER = my_other_custom_parser)
#' ```
#'
#' Your custom parser function *MUST* accept the following parameters:
#' 1. `body`: Raw vector containing the form data.
#' 2. `...`: Additional optional parameters.
#'
#' @inherit parse_multipart examples
#' @seealso [parse_multipart()], [parse_json()]
#' @return Named list, or `NULL` when the request has no body.
#' @export
parse_form_urlencoded <- function(req, ...) {
  on.exit(req$rook.input$rewind())
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(NULL)
  }

  default <- function(body, ...) {
    webutils::parse_http(
      body = body,
      content_type = "application/x-www-form-urlencoded",
      ...
    )
  }

  parser <- getOption(x = "AMBIORIX_FORM_URLENCODED_PARSER", default = default)

  parser(body = body, ...)
}

#' Parse application/json data
#'
#' @description
#' This function parses JSON data from the request body.
#'
#' @param req Request /// Required. \cr
#'            The [Request] whose body is parsed.
#'
#' @param ... Key=Value pairs /// Optional. \cr
#'            Additional parameters passed to the parser function.
#'
#' @return The parsed body: a named list for an object, an unnamed list or
#'         an atomic vector for an array, a scalar otherwise. `NULL` when
#'         the request has no body.
#'
#' @details
#'
#' ### Parsed Shapes
#'
#' By default, `parse_json()` uses [yyjsonr::read_json_raw()] with
#' `obj_of_arrs_to_df`, `arr_of_objs_to_df`, and `arr_of_arrs_to_matrix`
#' turned off, so a body keeps the structure it was sent with: an object is
#' a named list, an array of objects is an unnamed list of named lists, and
#' neither becomes a data frame or matrix. The same JSON shape always parses
#' to the same R shape, whatever values it holds.
#'
#' `length1_array_asis` is turned on for the same reason: `["a"]` is read as
#' `I("a")`, marked `AsIs`, where `"a"` is a plain string, so an array of
#' one stays an array. The marker is invisible to `==`, `%in%`, `[[` and
#' arithmetic, and both [yyjsonr::write_json_str()] and
#' [jsonlite::toJSON()] write the value back as `["a"]`; `identical()` and
#' `inherits()` do see it.
#'
#' `int64 = "double"` reads an integer too large for R's integer type as a
#' whole double, where yyjsonr would (by default) read it as a string: a
#' 64-bit id or a millisecond timestamp stays a number, and `[3000000000, 10]`
#' stays a numeric vector rather than a list of a string and an integer. An
#' integer that fits is still read as an integer, so a handler gets an integer
#' or a double depending on the value, the way `1` and `1e3` already did. A
#' double holds a whole number exactly up to 2^53; past that it is rounded.
#' Pass `int64 = "string"` for the old reading, or `int64 = "bit64"` with
#' the bit64 package attached for exact 64-bit integers.
#'
#' A request with no body at all parses to `NULL`, before the parser is
#' called. Nothing on the wire is `NULL`, so an absent body cannot be
#' confused with `{}`, a named empty list, or `[]`, an empty list. A body
#' of `null` is the one exception: it is the same value.
#'
#' Pass any of those options per call to read the body differently:
#'
#' ```r
#' body <- req$parse_json(arr_of_objs_to_df = TRUE)
#' ```
#'
#' ### Overriding Default Parser
#'
#' You can override the parser globally by setting the `AMBIORIX_JSON_PARSER` option:
#'
#' ```r
#' my_json_parser <- function(body, ...) {
#'   txt <- rawToChar(body)
#'   jsonlite::fromJSON(txt, ...)
#' }
#' options(AMBIORIX_JSON_PARSER = my_json_parser)
#' ```
#'
#' Your custom parser *MUST* accept the following parameters:
#' 1. `body`: Raw vector containing the JSON data.
#' 2. `...`: Additional optional parameters.
#'
#' The same parser, default or override, reads websocket messages: a
#' message received by `app$receive()` has the shape `parse_json()` would
#' give it as a body. A text frame is handed over as its bytes, a binary
#' frame as is.
#'
#' ### Validated Routes
#'
#' On a route validated with `app$openapi()`, the body is parsed with this
#' same parser — default or override — and stored on `req$payload`, so a
#' handler sees exactly what `parse_json()` would return, and validation
#' checks the structure the client actually sent. An override must keep an
#' array of one apart from a scalar for that to hold: return it marked
#' `AsIs`, as the default does, or as a list, as
#' `jsonlite::fromJSON(simplifyVector = FALSE)` does.
#'
#' @inherit parse_multipart examples
#' @seealso [parse_multipart()], [parse_form_urlencoded()]
#' @export
parse_json <- function(req, ...) {
  on.exit(req$rook.input$rewind())
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(NULL)
  }

  get_json_parser()(body, ...)
}

#' Default JSON Parser
#'
#' Reads JSON with [yyjsonr::read_json_raw()] and the defaults described
#' under Parsed Shapes in [parse_json()]. Shared by request bodies and
#' websocket messages, so both read the same way.
#'
#' @param body Raw vector /// Required. \cr
#'             The JSON bytes.
#'
#' @param ... Key=Value pairs /// Optional. \cr
#'            Named options passed to [yyjsonr::read_json_raw()], either
#'            directly or as `opts = list(...)`. Both spellings are merged
#'            and filled in with the defaults.
#'
#' @return The parsed value.
#'
#' @noRd
#' @keywords internal
default_json_parser <- function(body, ...) {
  dots <- list(...)

  # `yyjsonr::read_json_raw()` accepts both `opts` & `...` but
  # `...` should override `opts`.
  # ensure that happens and use `opts` only:
  opts <- dots$opts
  if (is.null(opts)) {
    opts <- list()
  }

  dots$opts <- NULL
  opts[names(dots)] <- dots

  defaults <- list(
    obj_of_arrs_to_df = FALSE,
    arr_of_objs_to_df = FALSE,
    arr_of_arrs_to_matrix = FALSE,
    length1_array_asis = TRUE,
    int64 = "double"
  )
  for (option in setdiff(x = names(defaults), y = names(opts))) {
    opts[[option]] <- defaults[[option]]
  }

  yyjsonr::read_json_raw(body, opts = opts)
}

#' Retrieve JSON Parser
#'
#' The parser set with the `AMBIORIX_JSON_PARSER` option, or the default.
#'
#' @noRd
#' @keywords internal
get_json_parser <- function() {
  getOption(x = "AMBIORIX_JSON_PARSER", default = default_json_parser)
}
