# ambiorix

<!-- badges: start -->
<!-- badges: end -->

## Usage

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req){
  response("Homepage!")
})

app$get("/about", function(req){
  response("About!")
})

app$start()
```

