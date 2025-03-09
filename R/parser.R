#' Parse multipart form data
#'
#' Parses multipart form data, including file uploads, and returns the parsed fields as a list.
#'
#' @param req The request object.
#' @param ... Additional parameters passed to the parser function.
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
#'     body <- parse_json(req)
#'     cat(strrep(x = "-", times = 10), "\n")
#'     cat("Parsed JSON:\n")
#'     print(body)
#'     cat(strrep(x = "-", times = 10), "\n")
#'
#'     response <- list(
#'       code = 200L,
#'       msg = "hello, world"
#'     )
#'     res$json(response)
#'   }
#'
#'   url_form_encoded_post <- function(req, res) {
#'     body <- parse_form_urlencoded(req)
#'     cat(strrep(x = "-", times = 8), "\n")
#'     cat("Parsed application/x-www-form-urlencoded:\n")
#'     print(body)
#'     cat(strrep(x = "-", times = 8), "\n")
#'
#'     list_items <- lapply(
#'       X = names(body),
#'       FUN = function(nm) {
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
#'     body <- parse_multipart(req)
#'
#'     list_items <- lapply(
#'       X = names(body),
#'       FUN = function(nm) {
#'         field <- body[[nm]]
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
#'           print(read.csv(file = file_path))
#'         }
#'
#'         if (is_xlsx) {
#'           print(readxl::read_xlsx(path = file_path))
#'         }
#'
#'         tags$li(
#'           nm,
#'           ":",
#'           if (is_file) "printed on console" else field
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
#'   app <- Ambiorix$new(port = 5000L)
#'
#'   app$
#'     get("/", home_get)$
#'     post("/", home_post)$
#'     get("/about", about_get)$
#'     get("/contact", contact_get)$
#'     post("/url-form-encoded", url_form_encoded_post)$
#'     post("/multipart-form-data", multipart_form_data_post)
#'
#'   app$start()
#' }
#' @seealso [parse_form_urlencoded()], [parse_json()]
#' @return Named list.
#' @export
parse_multipart <- function(req, ...) {
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(list())
  }

  default <- function(body, content_type, ...) {
    webutils::parse_http(
      body = body,
      content_type = content_type,
      ...
    )
  }

  parser <- getOption(x = "AMBIORIX_MULTIPART_FORM_DATA_PARSER", default = default)
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
#' @param req The request object.
#' @param ... Additional parameters passed to the parser function.
#'
#' @return A list of parsed form fields, with each key representing a form field name and each value
#' representing the form field's value.
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
#' @return Named list
#' @export
parse_form_urlencoded <- function(req, ...) {
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(list())
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
#' @param req The request object.
#' @param ... Additional parameters passed to the parser function.
#'
#' @return An R object (e.g., list or data frame) parsed from the JSON data.
#'
#' @details
#'
#' ### Overriding Default Parser
#'
#' By default, `parse_json()` uses [yyjsonr::read_json_raw()] for JSON parsing.
#' You can override this globally by setting the `AMBIORIX_JSON_PARSER` option:
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
#' @inherit parse_multipart examples
#' @seealso [parse_multipart()], [parse_form_urlencoded()]
#' @return Named list
#' @export
parse_json <- function(req, ...) {
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(list())
  }

  parser <- getOption(x = "AMBIORIX_JSON_PARSER", default = yyjsonr::read_json_raw)
  parser(body, ...)
}
