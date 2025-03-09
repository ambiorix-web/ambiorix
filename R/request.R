#' Request
#' 
#' A request.
#' 
#' @field HEADERS Headers from the request.
#' @field HTTP_ACCEPT Content types to accept.
#' @field HTTP_ACCEPT_ENCODING Encoding of the request.
#' @field HTTP_ACCEPT_LANGUAGE Language of the request.
#' @field HTTP_CACHE_CONTROL Directorives for the cache (case-insensitive).
#' @field HTTP_CONNECTION Controls whether the network connection stays open after the current transaction finishes.
#' @field HTTP_COOKIE Cookie data.
#' @field HTTP_HOST Host making the request.
#' @field HTTP_SEC_FETCH_DEST Indicates the request's destination. That is the initiator of the original fetch request, which is where (and how) the fetched data will be used.
#' @field HTTP_SEC_FETCH_MODE Indicates mode of the request.
#' @field HTTP_SEC_FETCH_SITE Indicates the relationship between a request initiator's origin and the origin of the requested resource. 
#' @field HTTP_SEC_FETCH_USER Only sent for requests initiated by user activation, and its value will always be `?1`.
#' @field HTTP_UPGRADE_INSECURE_REQUESTS Signals that server supports upgrade.
#' @field HTTP_USER_AGENT User agent.
#' @field SERVER_NAME Name of the server.
#' @field httpuv.version Version of httpuv.
#' @field PATH_INFO Path of the request.
#' @field QUERY_STRING Query string of the request.
#' @field REMOTE_ADDR Remote address.
#' @field REMOTE_PORT Remote port.
#' @field REQUEST_METHOD Method of the request, e.g.: `GET`.
#' @field rook.errors Errors from rook.
#' @field rook.input Rook inputs.
#' @field rook.url_scheme Rook url scheme.
#' @field rook.version Rook version.
#' @field SCRIPT_NAME The initial portion of the request URL's "path" that corresponds to the application object, so that the application knows its virtual "location".  #' @field SERVER_NAME Server name.
#' @field SERVER_PORT Server port
#' @field CONTENT_LENGTH Size of the message body.
#' @field CONTENT_TYPE Type of content of the request.
#' @field HTTP_REFERER Contains an absolute or partial address of the page that makes the request.
#' @field body Request, an environment.
#' @field query Parsed `QUERY_STRING`, `list`.
#' @field params A `list` of parameters.
#' @field cookie Parsed `HTTP_COOKIE`.
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
    HEADERS = NULL,
    HTTP_ACCEPT = NULL, 
    HTTP_ACCEPT_ENCODING = NULL,
    HTTP_ACCEPT_LANGUAGE = NULL, 
    HTTP_CACHE_CONTROL = NULL,
    HTTP_CONNECTION = NULL,
    HTTP_COOKIE = NULL,
    HTTP_HOST = NULL,
    HTTP_SEC_FETCH_DEST = NULL,
    HTTP_SEC_FETCH_MODE = NULL,
    HTTP_SEC_FETCH_SITE = NULL, 
    HTTP_SEC_FETCH_USER = NULL,
    HTTP_UPGRADE_INSECURE_REQUESTS = NULL,
    HTTP_USER_AGENT = NULL,
    httpuv.version = NULL,
    PATH_INFO = NULL,
    QUERY_STRING = NULL, 
    REMOTE_ADDR = NULL,
    REMOTE_PORT = NULL,
    REQUEST_METHOD = NULL,
    rook.errors = NULL,
    rook.input = NULL,
    rook.url_scheme = NULL,
    rook.version = NULL, 
    SCRIPT_NAME = NULL,
    SERVER_NAME = NULL,
    SERVER_PORT = NULL,
    CONTENT_LENGTH = NULL,
    CONTENT_TYPE = NULL,
    HTTP_REFERER = NULL,
    body = NULL,
    query = list(),
    params = list(),
    cookie = list(),
    #' @details Constructor
    #' @param req Original request (environment).
    initialize = function(req){
      self$HEADERS <- as.list(req$HEADERS)
      self$HTTP_ACCEPT <- req$HTTP_ACCEPT
      self$HTTP_ACCEPT_ENCODING <- req$HTTP_ACCEPT_ENCODING
      self$HTTP_ACCEPT_LANGUAGE <- req$HTTP_ACCEPT_LANGUAGE
      self$HTTP_CACHE_CONTROL <- req$HTTP_CACHE_CONTROL
      self$HTTP_CONNECTION <- req$HTTP_CONNECTION
      self$HTTP_COOKIE <- req$HTTP_COOKIE
      self$HTTP_HOST <- req$HTTP_HOST
      self$HTTP_SEC_FETCH_DEST <- req$HTTP_SEC_FETCH_DEST
      self$HTTP_SEC_FETCH_MODE <- req$HTTP_SEC_FETCH_MODE
      self$HTTP_SEC_FETCH_SITE <- req$HTTP_SEC_FETCH_SITE
      self$HTTP_SEC_FETCH_USER <- req$HTTP_SEC_FETCH_USER
      self$HTTP_UPGRADE_INSECURE_REQUESTS <- req$HTTP_UPGRADE_INSECURE_REQUESTS
      self$HTTP_USER_AGENT <- req$HTTP_USER_AGENT
      self$httpuv.version <- req$httpuv.version
      self$PATH_INFO <- req$PATH_INFO
      self$QUERY_STRING <- req$QUERY_STRING
      self$REMOTE_ADDR <- req$REMOTE_ADDR
      self$REMOTE_PORT <- req$REMOTE_PORT
      self$REQUEST_METHOD <- req$REQUEST_METHOD
      self$rook.errors <- req$rook.errors
      self$rook.input <- req$rook.input
      self$rook.url_scheme <- req$rook.url_scheme
      self$rook.version <- req$rook.version
      self$SCRIPT_NAME <- req$SCRIPT_NAME
      self$SERVER_NAME <- req$SERVER_NAME
      self$SERVER_PORT <- req$SERVER_NAME
      self$CONTENT_LENGTH <- req$CONTENT_LENGTH
      self$CONTENT_TYPE <- req$CONTENT_TYPE
      self$HTTP_REFERER <- req$HTTP_REFERER
      self$body <- req

      private$.parse_query_string(req$QUERY_STRING)
      self$cookie <- .globals$cookieParser(req)

    },
    #' @details Print
    print = function(){
      cli::cli_h3("A Request")
      cli::cli_ul()
      cli::cli_li("HEADERS: {.val {self$HEADERS}}")
      cli::cli_li("HTTP_ACCEPT: {.val {self$HTTP_ACCEPT}}")
      cli::cli_li("HTTP_ACCEPT_ENCODING: {.val {self$HTTP_ACCEPT_ENCODING}}")
      cli::cli_li("HTTP_ACCEPT_LANGUAGE: {.val {self$HTTP_ACCEPT_LANGUAGE}}")
      cli::cli_li("HTTP_CACHE_CONTROL: {.val {self$HTTP_CACHE_CONTROL}}")
      cli::cli_li("HTTP_CONNECTION: {.val {self$HTTP_CONNECTION}}")
      cli::cli_li("HTTP_COOKIE: {.val {self$HTTP_COOKIE}}")
      cli::cli_li("HTTP_HOST: {.val {self$HTTP_HOST}}")
      cli::cli_li("HTTP_SEC_FETCH_DEST: {.val {self$HTTP_SEC_FETCH_DEST}}")
      cli::cli_li("HTTP_SEC_FETCH_MODE: {.val {self$HTTP_SEC_FETCH_MODE}}")
      cli::cli_li("HTTP_SEC_FETCH_SITE: {.val {self$HTTP_SEC_FETCH_SITE}}")
      cli::cli_li("HTTP_SEC_FETCH_USER: {.val {self$HTTP_SEC_FETCH_USER}}")
      cli::cli_li("HTTP_UPGRADE_INSECURE_REQUESTS: {.val {self$HTTP_UPGRADE_INSECURE_REQUESTS}}")
      cli::cli_li("HTTP_USER_AGENT: {.val {self$HTTP_USER_AGENT}}")
      cli::cli_li("httpuv.version {.val {self$httpuv.version}}")
      cli::cli_li("PATH_INFO: {.val {self$PATH_INFO}}")
      cli::cli_li("QUERY_STRING: {.val {self$QUERY_STRING}}")
      cli::cli_li("REMOTE_ADDR: {.val {self$REMOTE_ADDR}}")
      cli::cli_li("REMOTE_PORT: {.val {self$REMOTE_PORT}}")
      cli::cli_li("REQUEST_METHOD: {.val {self$REQUEST_METHOD}}")
      cli::cli_li("SCRIPT_NAME: {.val {self$SCRIPT_NAME}}")
      cli::cli_li("SERVER_NAME: {.val {self$SERVER_NAME}}")
      cli::cli_li("SERVER_PORT: {.val {self$SERVER_PORT}}")
      cli::cli_li("CONTENT_LENGTH: {.val {self$CONTENT_LENGTH}}")
      cli::cli_li("CONTENT_TYPE: {.val {self$CONTENT_TYPE}}")
      cli::cli_li("HTTP_REFERER: {.val {self$HTTP_REFERER}}")
      cli::cli_li("rook.version: {.val {self$rook.version}}")
      cli::cli_li("rook.url_scheme: {.val {self$rook.url_scheme}}")

      if(length(self$params)){
        cli::cli_li("params: {.val params}")
        str(self$params)
      }

      if(length(self$query)){
        cli::cli_li("query: {.val query}")
        str(self$query)
      }

      cli::cli_end()
    },
    #' @details Get Header
    #' @param name Name of the header
    get_header = function(name){
      assert_that(not_missing(name))
      req$HEADERS[[name]]
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
    .parse_query_string = function(query){
      if(identical(length(query), 0L))
        return()

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
set_params <- function(path, route = NULL){

  if(is.null(route))
    return(list())

  if(!route$dynamic)
    return(list())

  path_split <- strsplit(path, "/")[[1]]
  path_split <- path_split[path_split != ""]

  nms <- c()
  pms <- list()
  for(i in seq_along(path_split)){
    if(i > length(route$components))
      break

    if(route$components[[i]]$dynamic){
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
#' 
#' @examples 
#' mockRequest()
#' 
#' @return A `Request` object.
#' @export 
mockRequest <- function(
  cookie = "",
  query = "",
  path = "/"
){
  req <- list(
    HEADERS = list(
      accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9", 
      `accept-encoding` = "gzip, deflate, br", 
      `accept-language` = "en-US,en;q=0.9", 
      connection = "keep-alive", 
      host = "localhost:13698", 
      `sec-ch-ua` = "\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"99\"", 
      `sec-ch-ua-mobile` = "?0", 
      `sec-ch-ua-platform` = "\"Linux\"", 
      `sec-fetch-dest` = "document", 
      `sec-fetch-mode` = "navigate", 
      `sec-fetch-site` = "none", 
      `sec-fetch-user` = "?1", 
      `upgrade-insecure-requests` = "1", 
      `user-agent` = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.82 Safari/537.36"
    ), 
    HTTP_COOKIE = cookie, 
    HTTP_ACCEPT = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9", 
    HTTP_ACCEPT_ENCODING = "gzip, deflate, br", 
    HTTP_ACCEPT_LANGUAGE = "en-US,en;q=0.9", 
    HTTP_CONNECTION = "keep-alive", 
    HTTP_HOST = "localhost:13698", 
    HTTP_SEC_FETCH_DEST = "document", 
    HTTP_SEC_FETCH_MODE = "navigate", 
    HTTP_SEC_FETCH_SITE = "none", 
    HTTP_SEC_FETCH_USER = "?1", 
    HTTP_UPGRADE_INSECURE_REQUESTS = "1", 
    HTTP_USER_AGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.82 Safari/537.36", 
    httpuv.version = structure(
      list(c(1L, 6L, 5L)), 
      class = c("package_version", "numeric_version")
    ), 
    PATH_INFO = path, 
    QUERY_STRING = query, 
    REMOTE_ADDR = "127.0.0.1", 
    REMOTE_PORT = "44328", 
    REQUEST_METHOD = "GET", 
    rook.errors = list(), 
    rook.input = list(), 
    rook.url_scheme = "http", 
    rook.version = "1.1-0", 
    SCRIPT_NAME = "", 
    SERVER_NAME = "127.0.0.1", 
    SERVER_PORT = "127.0.0.1"
  )

  Request$new(req)
}
