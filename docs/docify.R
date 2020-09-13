library(purrr)

fs::file_copy("NEWS.md", "docs/changelog.md", overwrite = TRUE)

if(fs::dir_exists("./docs/reference"))
  stop("Reference exists")
fs::dir_create("./docs/reference")

# ------------------------------------------- REFERENCE
# functions
get_id <- function(x){
  gsub("\\.md", "", x)
}

get_name <- function(x){
  x <- gsub("\\_", " ", x)
  gsub("\\.md", "", x)
}

# directory of .Rd
dir <- "./man"

# read .Rd
files <- list.files(dir)
files <- files[grepl("\\.Rd", files)]

docs <- purrr::map(files, function(x){
  input <- paste0(dir, "/", x)
  nm <- gsub("\\.Rd", ".md", x)
  output <- paste0("./docs/reference/", nm)
  Rd2md::Rd2markdown(input, output)

  list(name = nm, output = output)
})
