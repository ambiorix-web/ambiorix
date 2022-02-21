#' Preprocess Request
#' 
#' @noRd 
#' @keywords internal
Request <- R6::R6Class(
  "Request",
  public = list(
    HEADERS = NULL,
    HTTP_ACCEPT = NULL, 
    HTTP_ACCEPT_ENCODING = NULL,
    HTTP_ACCEPT_LANGUAGE = NULL, 
    HTTP_CACHE_CONTROL = NULL,
    HTTP_CONNECTION = NULL,
    HTTP_COOKIE = NULL,
    HTTP_DNT = NULL, 
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
    initialize = function(req){
      self$HEADERS <- req$HEADERS
      self$HTTP_ACCEPT <- req$HTTP_ACCEPT
      self$HTTP_ACCEPT_ENCODING <- req$HTTP_ACCEPT_ENCODING
      self$HTTP_ACCEPT_LANGUAGE <- req$HTTP_ACCEPT_LANGUAGE
      self$HTTP_CACHE_CONTROL <- req$HTTP_CACHE_CONTROL
      self$HTTP_CONNECTION <- req$HTTP_CONNECTION
      self$HTTP_COOKIE <- req$HTTP_COOKIE
      self$HTTP_DNT <- req$HTTP_DNT
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

    },
    print = function(){
      cli::cli_li("HEADERS: {.val {self$HEADERS}}")
      cli::cli_li("HTTP_ACCEPT: {.val {self$HTTP_ACCEPT}}")
      cli::cli_li("HTTP_ACCEPT_ENCODING: {.val {self$HTTP_ACCEPT_ENCODING}}")
      cli::cli_li("HTTP_ACCEPT_LANGUAGE: {.val {self$HTTP_ACCEPT_LANGUAGE}}")
      cli::cli_li("HTTP_CACHE_CONTROL: {.val {self$HTTP_CACHE_CONTROL}}")
      cli::cli_li("HTTP_CONNECTION: {.val {self$HTTP_CONNECTION}}")
      cli::cli_li("HTTP_COOKIE: {.val {self$HTTP_COOKIE}}")
      cli::cli_li("HTTP_DNT: {.val {self$HTTP_DNT}}")
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
    },
    set = function(name, value){
      assert_that(not_missing(name))
      assert_that(not_missing(value))

      name <- deparse(substitute(name))
      private$.data[[name]] <- value

      invisible(self)
    },
    get = function(name){
      assert_that(not_missing(name))

      name <- deparse(substitute(name))
      private$.data[[name]]
    }
  ),
  private = list(
    .data = list(),
    .parse_query_string = function(query){
      if(is.null(query)){
        self$query <- list()
        return()
      }

      if(query == ""){
        self$query <- list()
        return()
      }
      
      q <- gsub("^\\?", "", query)
      params <- strsplit(q, "&")[[1]]
      params_split <- strsplit(params, "=")

      lst <- sapply(params_split, function(x){
        if(length(x) > 1) return(x[2])

        x[1]
      })
      names(lst) <- sapply(params_split, function(x){
        if(length(x) > 1) return(x[1])
        return(NULL)
      })

      self$query <- as.list(lst)
      invisible()
    }
  )
)

#' Set Parameters
#' 
#' Set the query's parameters.
#' 
#' @param path Correspond's the the requests' `PATH_INFO`
#' @param route See `Route`
#' 
#' @return Parameter list
set_params <- function(path, route = NULL){

  if(is.null(route))
    return(list())

  if(!route$dynamic)
    return(list())

  path_split <- strsplit(path, "/")[[1]]
  path_split <- path_split[path_split != ""]

  nms <- c()
  pms <- list()
  for(i in 1:length(path_split)){
    if(route$components[[i]]$dynamic){
      nms <- c(nms, route$components[[i]]$name)
      pms <- append(pms, utils::URLdecode(path_split[i]))
    }
  }

  names(pms) <- nms
  return(pms)
}
