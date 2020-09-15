#' Plain Responses
#' 
#' Plain HTTP Responses.
#' 
#' @param body Body of response.
#' @param headers HTTP headers.
#' @param status Response status
#' 
#' @name responses
#' 
#' @export
response <- function(body, headers = list('Content-Type' = 'text/html'), status = 200L){
  res <- list(status = as.integer(status), headers = headers, body = as.character(body))
  construct_response(res)
}

#' @rdname responses
#' @export
response_404 <- function(body = "404: Not found", headers = list('Content-Type' = 'text/html'), status = 404L){
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
#' @noRd 
#' @keywords internal
Response <- R6::R6Class(
  "Response",
  public = list(
    initialize = function(){
      template_path <- here::here("templates")
      private$.has_templates <- fs::dir_exists(template_path)

      if(!private$.has_templates)
        return(self)
    },
    send = function(body, headers = list('Content-Type' = 'text/html'), status = 200L){
      response(status = status, headers = headers, body = as.character(body))
    },
    send_file = function(file, status = 200L){
      assert_that(not_missing(file))

      self$render(file, data = list(), status = status)
    },
    render = function(file, data = list(), status = 200L){
      assert_that(not_missing(file))

      if(!private$.has_templates)
        stop("No templates directory found", call. = FALSE)

      file_path <- private$.get_template_path(file)

      file_content <- private$.render_template(file_path, data)

      response(file_content, status = status)
    },
    json = function(body, headers = list("Content-Type" = "application/json"), status = 200L){
      to_json <- get_serialise()
      response(to_json(body), headers = headers, status = status)
    },
    print = function(){
      cli::cli_li("{.code send(body, headers, status)}")
      cli::cli_li("{.code send_file(file, status)}")
      cli::cli_li("{.code render(file, data, status)}")
    }
  ),
  private = list(
    .has_templates = FALSE,
    .templates = list(),
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
    .try_template_path = function(file, ext = c(".html", ".R")){
      ext <- match.arg(ext)
      path <- here::here("templates", paste0(file, ext))

      if(fs::file_exists(path))
        return(path)

      return(NULL)
    },
    .render_template = function(file, data){
      # read and replace tags
      file_content <- readLines(file)

      # render
      ext <- tools::file_ext(file)
      if(ext == "html"){
        to_json <- get_serialise()
        data <- lapply(data, function(x){
          to_json(x)
        })
      } else {
        data <- lapply(data, function(x){
          if(!inherits(x, "AsIs"))
            return(x)

          # remove AsIs
          # causes warnings on NULL and NA
          # will have side effects
          class(x) <- class(x)[class(x) != "AsIs"]

          paste0(capture.output(dput(x)), collapse = "")
        })
      }

      file_content <- lapply(file_content, function(x, data){
        glue::glue_data(data, x, .open = "[%", .close = "%]")
      }, data = data)

      # if(length(data) > 0){
      #   for(i in 1:length(data)){
      #     pattern <- sprintf("\\[%% ?%s ?%%\\]", names(data)[i]) # [% mustache %]

      #     # only serialise if HTML
      #     if(ext == "html"){
      #       to_json <- get_serialise()
      #       value <- to_json(data[[i]])
      #     } else {
      #       value <- data[[i]]

      #       if (inherits(value, "character")) {
      #         value <- sprintf("'%s'", as.character(value))
      #       } else if (is.null(value)) {
      #         value <- "NULL"
      #       } else if (is.na(value)) {
      #         value <- "NA"
      #       } else if (inherits(value, "AsIs")) {
      #         value <- as.character(value)
      #       } else {
      #         value <- as.character(dput(value))
      #       } 

      #     }
          
      #     file_content <- gsub(pattern, value, file_content)
      #   }
      # }

      # collapse html
      if(ext == "html")
        return(paste0(file_content, collapse = ""))

      # parse R
      render_html(file_content)

    },
    .make_template_path = function(file){
      # clean input
      file <- remove_extensions(file)

      # list possible files
      files <- remove_extensions(private$.templates)

      template <- sprintf("./templates/%s.html", template)
      normalizePath(template)
    }
  )
)