#' Request
#'
#' A request object wrapping the incoming HTTP request.
#'
#' @field headers Named list of request headers.
#' @field method HTTP method (GET, POST, etc.).
#' @field path Request path.
#' @field uri Full request URI.
#' @field query Parsed query string as a list.
#' @field params URL parameters from dynamic routes.
#' @field body Request body (raw vector).
#' @field cookie Parsed cookies.
#' @field CONTENT_TYPE Content type of the request.
#' @field CONTENT_LENGTH Content length of the request.
#' @field PATH_INFO Alias for path (compatibility).
#' @field REQUEST_METHOD Alias for method (compatibility).
#' @field QUERY_STRING Raw query string.
#' @field REMOTE_ADDR Remote address (from X-Forwarded-For header if present).
#' @field SERVER_NAME Server name (from Host header).
#'
#' @return A Request object.
#' @examples
#' if (interactive()) {
#'   library(ambiorix)
#'
#'   app <- Ambiorix$new()
#'
#'   app$get("/", function(req, res) {
#'     print(req)
#'     res$send("Using {ambiorix}!")
#'   })
#'
#'   app$start()
#' }
#' @export
Request <- R6::R6Class(
  "Request",
  lock_objects = FALSE,
  public = list(
    headers = NULL,
    method = NULL,
    path = NULL,
    uri = NULL,
    query = list(),
    params = list(),
    body = NULL,
    cookie = list(),
    CONTENT_TYPE = NULL,
    CONTENT_LENGTH = NULL,
    PATH_INFO = NULL,
    REQUEST_METHOD = NULL,
    QUERY_STRING = NULL,
    REMOTE_ADDR = NULL,
    SERVER_NAME = NULL,
    HTTP_HOST = NULL,
    HTTP_USER_AGENT = NULL,
    HTTP_ACCEPT = NULL,
    HTTP_COOKIE = NULL,
    HTTP_REFERER = NULL,
    #' @details Constructor
    #' @param req Original request from nanonext.
    initialize = function(req) {
      # nanonext provides: method, uri, headers (named char vector), body (raw)
      self$method <- req$method
      self$REQUEST_METHOD <- req$method
      self$uri <- req$uri

      # parse path and query string from uri
      uri_parts <- strsplit(req$uri, "?", fixed = TRUE)[[1]]
      self$path <- uri_parts[1]
      self$PATH_INFO <- uri_parts[1]
      self$QUERY_STRING <- if (length(uri_parts) > 1) uri_parts[2] else ""

      # headers - convert to list first, then access from list (returns NULL if missing)
      self$headers <- as.list(req$headers)
      self$CONTENT_TYPE <- self$headers[["Content-Type"]]
      self$CONTENT_LENGTH <- self$headers[["Content-Length"]]
      self$HTTP_HOST <- self$headers[["Host"]]
      self$HTTP_USER_AGENT <- self$headers[["User-Agent"]]
      self$HTTP_ACCEPT <- self$headers[["Accept"]]
      self$HTTP_COOKIE <- self$headers[["Cookie"]]
      self$HTTP_REFERER <- self$headers[["Referer"]]
      self$SERVER_NAME <- self$headers[["Host"]]
      self$REMOTE_ADDR <- self$headers[["X-Forwarded-For"]] %||% ""

      # body
      self$body <- req$body

      # parse query string
      private$.parse_query_string(self$QUERY_STRING)

      # parse cookies
      self$cookie <- .globals$cookieParser(self)
    },
    #' @details Print
    print = function() {
      cli::cli_h3("A Request")
      cli::cli_ul()
      cli::cli_li("method: {.val {self$method}}")
      cli::cli_li("path: {.val {self$path}}")
      cli::cli_li("uri: {.val {self$uri}}")
      cli::cli_li("QUERY_STRING: {.val {self$QUERY_STRING}}")
      cli::cli_li("CONTENT_TYPE: {.val {self$CONTENT_TYPE}}")
      cli::cli_li("CONTENT_LENGTH: {.val {self$CONTENT_LENGTH}}")
      cli::cli_li("HTTP_HOST: {.val {self$HTTP_HOST}}")
      cli::cli_li("HTTP_USER_AGENT: {.val {self$HTTP_USER_AGENT}}")
      cli::cli_li("REMOTE_ADDR: {.val {self$REMOTE_ADDR}}")

      if (length(self$headers)) {
        cli::cli_li("headers:")
        str(self$headers)
      }

      if (length(self$params)) {
        cli::cli_li("params:")
        str(self$params)
      }

      if (length(self$query)) {
        cli::cli_li("query:")
        str(self$query)
      }

      cli::cli_end()
    },
    #' @details Get Header
    #' @param name Name of the header
    get_header = function(name) {
      assert_that(not_missing(name))
      self$headers[[name]]
    },
    #' @details Parse Multipart encoded data
    parse_multipart = function() {
      parse_multipart(self)
    },
    #' @details Parse JSON encoded data
    #' @param ... Arguments passed to [parse_json()].
    parse_json = function(...) {
      parse_json(self, ...)
    }
  ),
  private = list(
    .parse_query_string = function(query) {
      if (is.null(query) || identical(nchar(query), 0L)) {
        return()
      }
      self$query <- webutils::parse_query(query)
      invisible()
    }
  )
)

#' Set Parameters
#'
#' Set the query's parameters.
#'
#' @param path Corresponds the requests' `PATH_INFO`
#' @param route See `Route`
#'
#' @return Parameter list
#' @keywords internal
#' @noRd
set_params <- function(path, route = NULL) {
  if (is.null(route)) {
    return(list())
  }

  if (!route$dynamic) {
    return(list())
  }

  path_split <- strsplit(path, "/")[[1]]
  path_split <- path_split[path_split != ""]

  nms <- c()
  pms <- list()
  for (i in seq_along(path_split)) {
    if (i > length(route$components)) {
      break
    }

    if (route$components[[i]]$dynamic) {
      nms <- c(nms, route$components[[i]]$name)
      pms <- append(pms, utils::URLdecode(path_split[i]))
    }
  }

  names(pms) <- nms
  return(pms)
}

#' Mock Request
#'
#' Mock a request, used for tests.
#'
#' @param cookie Cookie string.
#' @param query Query string.
#' @param path Path string.
#' @param method HTTP method.
#'
#' @examples
#' mockRequest()
#'
#' @return A `Request` object.
#' @export
mockRequest <- function(
  cookie = "",
  query = "",
  path = "/",
  method = "GET"
) {
  uri <- if (nzchar(query)) paste0(path, "?", query) else path

  req <- list(
    method = method,
    uri = uri,
    headers = c(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Encoding" = "gzip, deflate, br",
      "Accept-Language" = "en-US,en;q=0.9",
      "Connection" = "keep-alive",
      "Host" = "localhost:8080",
      "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36",
      "Cookie" = cookie
    ),
    body = raw()
  )

  Request$new(req)
}
