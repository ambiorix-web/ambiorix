# Hello World

First, make sure you have the package installed. It is available from CRAN.

```r
install.packages("ambiorix")
```

Alternatively, the development version can be installed from Github with the [remotes](https://remotes.r-lib.org/) package.

```r
# install.packages("remotes")
remotes::install_github("JohnCoene/ambiorix")
```

<!-- panels:start -->

<!-- div:left-panel -->

By default ambiorix will serve the application on a random port, this can be changed, along with other things, when instantiating the class. 

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Using {ambiorix}!")
})

app$get("/about", function(req, res){
  res$text("About")
})

app$start()
```

<!-- div:right-panel -->

![](../_assets/rstudio.png)

<!-- panels:end -->

