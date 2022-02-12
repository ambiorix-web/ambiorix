library(purrr)

news <- readLines("NEWS.md")
news <- gsub("#", "##", news)
news <- c("# Changelog", "", news)
writeLines(news, con = "docs/changelog.md")

fs::dir_delete("./docs/reference")
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

md <- c()
for(i in 1:length(docs)) {
  md <- c(
    md,
    sprintf(
      "- [%s](%s)", 
      gsub("\\.md", "", docs[[i]]$name),
      gsub("\\./docs/", "", docs[[i]]$output)
    )
  )
}

cat(paste0(md, collapse = "\n"), "\n")
