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
    query = list(),
    params = list(),
    initialize = function(req, route){
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

      private$.parse_query_string(req$QUERY_STRING)

      if(route$dynamic)
        private$.parse_path(route$components, req$PATH_INFO)
    }
  ),
  private = list(
    .parse_query_string = function(query){
      if(query == ""){
        self$query <- list()
        return()
      }
      
      q <- gsub("^\\?", "", query)
      params <- strsplit(q, "&")[[1]]
      params_split <- strsplit(params, "=")

      lst <- sapply(params_split, function(x) x[2])
      names(lst) <- sapply(params_split, function(x) x[1])

      self$query <- as.list(lst)
      invisible()
    },
    .parse_path = function(params, path){
      path_split <- strsplit(path, "/")[[1]]
      path_split <- path_split[path_split != ""]

      nms <- c()
      pms <- list()
      for(i in 1:length(path_split)){
        if(params[[i]]$dynamic){
          nms <- c(nms, params[[i]]$name)
          pms <- append(pms, path_split[i])
        }
      }

      names(pms) <- nms
      self$params <- pms
    }
  )
)
