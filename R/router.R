#' Router
#' 
#' Web server.
#' 
#' @field error 500 response when the route errors, must a handler function that accepts the request and the response, by default uses [response_500()].
#' 
#' @examples 
#' # log
#' logger <- new_log()
#' # router
#' # create router
#' router <- Router$new("/users")
#' 
#' router$get("/", function(req, res){
#'  res$send("List of users")
#' })
#' 
#' router$get("/:id", function(req, res){
#'  logger$log("Return user id:", req$params$id)
#'  res$send(req$params$id)
#' })
#' 
#' router$get("/:id/profile", function(req, res){
#'  msg <- sprintf("This is the profile of user #%s", req$params$id)
#'  res$send(msg)
#' })
#' 
#' # core app
#' app <- Ambiorix$new()
#' 
#' app$get("/", function(req, res){
#'  res$send("Home!")
#' })
#' 
#' # mount the router
#' app$use(router)
#' 
#' if(interactive())
#'  app$start()
#' 
#' @importFrom assertthat assert_that
#' @importFrom utils browseURL
#' 
#' @export
Router <- R6::R6Class(
  "Router",
  inherit = Web,
  public = list(
    error = NULL,
#' @details Define the base route.
#' @param path The base path of the router.
    initialize = function(path){
      assert_that(not_missing(path))
      super$initialize(path)
    },
#' @details Print
    print = function(){
      cli::cli_rule("Ambiorix", right = "router")
      cli::cli_li("routes: {.val {super$.n_routes()}}")
    }
  )
)
