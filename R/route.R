Route <- R6::R6Class(
  "Route",
  public = list(
    path = NULL,
    components = list(),
    pattern = NULL,
    dynamic = FALSE,
    initialize = function(path){
      assert_that(not_missing(path))
      self$path <- path
      self$dynamic <- grepl(":", path)
      self$decompose()
      self$as_pattern()
    },
    as_pattern = function(){
      pattern <- sapply(self$components, function(comp){
        if(comp$dynamic)
          return("[[:alpha:]]*")

        return(comp$name)
      })

      pattern <- paste0(pattern, collapse = "/")
      self$pattern <- paste0("^/", pattern, "$")
      invisible(self)
    },
    decompose = function(){
      # split
      components <- strsplit(self$path, "(?<=.)(?=[:/])", perl = TRUE)[[1]]

      # remove lonely /
      components <- components[components != "/"]

      if(length(components) == 0){
        self$components <- list(
          list(
            index = 1, 
            dynamic = FALSE,
            name = ""
          )
        )

        return()
      }

      # cleanup
      components <- gsub("/", "", components)

      components <- as.list(components)
      comp <- list()
      for(i in 1:length(components)){
        c <- list(
          index = i, 
          dynamic = grepl(":", components[[i]]),
          name = gsub(":|$", "", components[[i]])
        )
        comp <- append(comp, list(c))
      }

      self$components <- comp
      invisible(self)
    }
  )
)