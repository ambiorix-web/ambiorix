# Hello World

Make sure you have the package installed.

```r
# install.packages("ambiorix")
remotes::install_github("JohnCoene/ambiorix")
```

By default ambiorix will serve the application on port `3000`, this can be changed, along with other things, when instantiating the class.

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello!")
})

app$start()
```

![](../_assets/rstudio.png)

Kill the server with `app$stop()`.
