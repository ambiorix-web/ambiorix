#' Parse HTTP request
#'
#' @description
#' Parses the body of an HTTP request based on its `Content-Type` header. This
#' function simplifies working with HTTP requests by extracting specific data
#' fields from the parsed body.
#'
#' @details
#' Supported `Content-Type` values include:
#' - `multipart/form-data`
#' - `application/json`
#' - `application/x-www-form-urlencoded`
#'
#' The `fields_to_extract` & `new_field_names` parameters **only**
#' used for 'multipart/form-data' and 'application/x-www-form-urlencoded'.
#'
#' For 'multipart/form-data', if a field is a file upload it is returned as a named list with:
#' - `value`: Raw vector representing the file contents. You must
#'    process this further (eg. convert to data.frame). See the examples section.
#' - `content_disposition`: Typically "form-data", indicating how the content
#'    is meant to be handled.
#' - `content_type`: MIME type of the uploaded file (e.g., "image/png" or "application/pdf").
#' - `name`: Name of the form input field.
#' - `filename`: Original name of the uploaded file.
#'
#' @param req A request object. The request must include a `CONTENT_TYPE` header
#' and a body accessible via `req$rook.input$read()`.
#' @param content_type String. 'Content-Type' of the request. See details for
#' valid values.
#' By default, this parameter is set to `NULL` and is inferred from the `req`
#' object during run time.
#' The only time you need to provide this argument is if `req$CONTENT_TYPE`
#' is different from how you want the request body to be parsed.
#' For example, `req$CONTENT_TYPE` gives "text/plain;charset=UTF-8" but you want
#' to parse the request body as "application/json".
#' @param fields_to_extract Character vector specifying the names of fields to
#' extract from the parsed request body. If missing, returns all
#' fields found after parsing of the HTTP request.
#' @param new_field_names Character vector of same length as
#' `fields_to_extract`. Specifies new names to assign to the extracted fields
#' in the returned list. Useful for renaming the fields for clarity or
#' consistency in the output. If not provided or empty (default), the
#' original names in `fields_to_extract` are used.
#' @return Named list containing the extracted fields and their associated
#' values. If no data is found or an error occurs, an empty list is returned.
#' @examples
#' if (interactive()) {
#'   library(ambiorix)
#'   library(htmltools)
#'   library(readxl)
#'
#'   page_links <- \() {
#'     Map(
#'       f = \(href, label) {
#'         tags$a(href = href, label)
#'       },
#'       c("/", "/about", "/contact"),
#'       c("Home", "About", "Contact")
#'     )
#'   }
#'
#'   forms <- \() {
#'     form1 <- tags$form(
#'       action = "/url-form-encoded",
#'       method = "GET",
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
#'     form3 <- tags$form(
#'       action = "/multipart-form-data2",
#'       method = "POST",
#'       enctype = "multipart/form-data",
#'       tags$h4("multipart/form-data (specific fields extracted & renamed):"),
#'       tags$label(`for` = "family_name", "Family Name"),
#'       tags$input(id = "family_name", name = "family_name", value = "the johns"),
#'       tags$label(`for` = "user_book", "User Book"),
#'       tags$input(id = "user_book", name = "user_book", value = "JavaScript for R"),
#'       tags$label(`for` = "user_age", "User Age"),
#'       tags$input(id = "user_age", name = "user_age", value = "15"),
#'       tags$button(type = "submit", "Submit")
#'     )
#'
#'     tagList(form1, form2, form3)
#'   }
#'
#'   home_get <- \(req, res) {
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("hello, world!"),
#'       forms()
#'     )
#'
#'     res$send(html)
#'   }
#'
#'   url_form_encoded_get <- \(req, res) {
#'     query <- req$query
#'     list_items <- lapply(
#'       X = names(query),
#'       FUN = \(nm) {
#'         tags$li(
#'           nm,
#'           ":",
#'           query[[nm]]
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
#'   multipart_form_data_post <- \(req, res) {
#'     body <- parse_req(req)
#'
#'     list_items <- lapply(
#'       X = names(body),
#'       FUN = \(nm) {
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
#'           read.csv(file = file_path) |> print()
#'         }
#'
#'         if (is_xlsx) {
#'           readxl::read_xlsx(path = file_path) |> print()
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
#'   multipart_form_data_post2 <- \(req, res) {
#'     body <- parse_req(
#'       req,
#'       fields_to_extract = c("user_book", "user_age"),
#'       new_field_names = c("book", "age")
#'     )
#'
#'     list_items <- lapply(
#'       X = names(body),
#'       FUN = \(nm) {
#'         tags$li(nm, ":", body[[nm]])
#'       }
#'     )
#'     input_vals <- tags$ul(list_items)
#'
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("Request processed, only these fields extracted & renamed:"),
#'       input_vals
#'     )
#'
#'     res$send(html)
#'   }
#'
#'   about_get <- \(req, res) {
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("About Us")
#'     )
#'     res$send(html)
#'   }
#'
#'   contact_get <- \(req, res) {
#'     html <- tagList(
#'       page_links(),
#'       tags$h3("Get In Touch!")
#'     )
#'     res$send(html)
#'   }
#'
#'   home_post <- \(req, res) {
#'     body <- parse_req(req)
#'     response <- list(
#'       code = 200L,
#'       msg = "hello, world"
#'     )
#'     res$json(response)
#'   }
#'
#'   app <- Ambiorix$new(port = 5000L)
#'
#'   app$
#'     get("/", home_get)$
#'     post("/", home_post)$
#'     get("/about", about_get)$
#'     get("/contact", contact_get)$
#'     get("/url-form-encoded", url_form_encoded_get)$
#'     post("/multipart-form-data", multipart_form_data_post)$
#'     post("/multipart-form-data2", multipart_form_data_post2)
#'
#'   app$start()
#' }
#' @export
parse_req <- function(req, content_type = NULL, fields_to_extract = character(), new_field_names = character()) {
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(list())
  }

  content_type_choices <- c(
    "application/json",
    "multipart/form-data",
    "application/x-www-form-urlencoded"
  )
  content_type <- if (!is.null(content_type)) {
    match.arg(arg = content_type, choices = content_type_choices)
  } else {
    req$CONTENT_TYPE
  }

  # -----application/json-----
  if (identical(content_type, "application/json")) {
    return(
      yyjsonr::read_json_raw(raw_vec = body)
    )
  }

  parsed <- webutils::parse_http(body = body, content_type = content_type)

  # -----application/x-www-form-urlencoded-----
  if (identical(content_type, "application/x-www-form-urlencoded")) {
    return(
      extract_and_rename_req_fields(
        x = parsed,
        fields_to_extract = fields_to_extract,
        new_field_names = new_field_names
      )
    )
  }

  # -----multipart/form-data-----
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

  extract_and_rename_req_fields(
    x = values,
    fields_to_extract = fields_to_extract,
    new_field_names = new_field_names
  )
}

#' Extract & rename parsed request fields
#'
#' @param x Named list. The parsed request.
#' @param fields_to_extract Character vector.
#' @param new_field_names Character vector.
#' @keywords internal
#' @noRd
extract_and_rename_req_fields <- function(x, fields_to_extract = character(), new_field_names = character()) {
  if (identical(fields_to_extract, character())) {
    return(x)
  }

  required <- x[fields_to_extract]

  if (identical(new_field_names, character())) {
    return(required)
  }

  stopifnot(
    "'fields_to_extract' must have same length as 'new_field_names'" =
      identical(
        length(fields_to_extract),
        length(new_field_names)
      )
  )

  names(required) <- new_field_names
  required
}

#' Parsers
#'
#' Collection of parsers to translate request data.
#'
#' @param req The request object.
#' @param ... Additional arguments passed to the internal parsers.
#'
#' @section Functions:
#' - [parse_multipart()]: Parse `multipart/form-data` using [webutils::parse_multipart()].
#' - [parse_json()]: Parse `application/json` using [yyjsonr::read_json_raw()].
#'
#' @return Returns the parsed value as a `list` or `NULL`
#' if it failed to parse.
#'
#' @seealso [parse_req()] for a more robust request parser.
#'
#' @name parsers
#' @export
parse_multipart <- function(req, ...) {
  parse_req(req)
}

#' @export
#' @rdname parsers
parse_json <- function(req, ...) {
  parse_req(req, ...)
}
