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
#'
#' The `fields_to_extract` & `new_field_names` parameters are currently only
#' used for 'multipart/form-data'.
#'
#' **Limitations**:
#' - File uploads are not yet supported but could be added in future updates if
#' required.
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
#'       tags$input(name = "first_name", value = "John"),
#'       tags$input(name = "last_name", value = "Coene"),
#'       tags$button(type = "submit", "Submit")
#'     )
#'
#'     form2 <- tags$form(
#'       action = "/multipart-form-data",
#'       method = "POST",
#'       enctype = "multipart/form-data",
#'       tags$h4("multipart/form-data:"),
#'       tags$input(name = "email", value = "john@mail.com"),
#'       tags$input(name = "framework", value = "ambiorix"),
#'       tags$button(type = "submit", "Submit")
#'     )
#'
#'     form3 <- tags$form(
#'       action = "/multipart-form-data2",
#'       method = "POST",
#'       enctype = "multipart/form-data",
#'       tags$h4("multipart/form-data (specific fields extracted & renamed):"),
#'       tags$input(name = "family_name", value = "the johns"),
#'       tags$input(name = "user_book", value = "JavaScript for R"),
#'       tags$input(name = "user_age", value = "15"),
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
#'         tags$li(nm, ":", query[[nm]])
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
#'         tags$li(nm, ":", body[[nm]])
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
    "multipart/form-data",
    "application/json"
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

  # -----multipart/form-data-----
  raw_to_char <- function(x) rawToChar(as.raw(x))

  values <- lapply(X = parsed, FUN = `[[`, "value") |>
    lapply(FUN = raw_to_char)

  if (identical(fields_to_extract, character())) {
    return(values)
  }

  required <- values[fields_to_extract]

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
#' - [parse_multipart()]: Parse `multipart/form-data` using [mime::parse_multipart()].
#' - [parse_json()]: Parse `multipart/form-data` using [jsonlite::fromJSON()].
#'
#' @return Returns the parsed value as a `list` or `NULL`
#' if it failed to parse.
#'
#' @name parsers
#' @export
parse_multipart <- function(req) {
  check_installed("mime")
  tryCatch(
    mime::parse_multipart(req$body),
    error = function(e) NULL
  )
}

#' @export
#' @rdname parsers
parse_json <- function(req, ...) {
  data <- req$body[["rook.input"]]
  data <- data$read_lines()
  tryCatch(
    jsonlite::fromJSON(data, ...),
    error = function(e) NULL
  )
}
