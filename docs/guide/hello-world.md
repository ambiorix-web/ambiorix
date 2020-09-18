# Hello World

Make sure you have the package installed.

```r
# install.packages("ambiorix")
remotes::install_github("JohnCoene/ambiorix")
```

By default ambiorix will serve the application on a random port, this can be changed, along with other things, when instantiating the class.

```r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Using {ambiorix}!")
})

app$start()
```

![](../_assets/rstudio.png)

The `get` method will add a `GET` method on the path `/` (the homepage).
