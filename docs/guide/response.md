# Response

Every get handler should accept the request (`req`) and the response (`res`).

## Plain

One can send a plain HTTP response with `send`.

```r
app$get("/", function(req, res){
  res$send("hello!")
})
```

## File

An `.html` or `.R` file can also be used as response.

```r
# sends templates/home.html
app$get("/", function(req, res){
  res$send_file("home")
})
```

## Render

An `.html` or `.R` file can also be rendered. The difference with `send_file` is that it will use `data` to replace `[% tags %]`. You can read more it in the [templates](/guide/project?id=templates) documentation.

```r
# renders templates/home.html
# replaces [% title %]
app$get("/:book", function(req, res){
  res$render("home", data = list(title = req$params$book))
})
```

## JSON

You can also send JSON responses with `json`.

```r
app$get("/:book", function(req, res){
  res$json(cars)
})
```

## Status

The status of the response can be specified in the response method (e.g.: `render('home', status = 200L)`), or with the `status` method.

```r
app$get("/error", function(req, res){
  res$status(500)
  res$send("Error!")
})
```

## Redirect

Redirect to a different url.

```r
app$get("/redirect", function(req, res){
  res$redirect("/", status = 302L)
})
```

## CSV

Serialises to CSV.

```r
app$get("/csv", function(req, res){
  res$csv(cars, "cars-data")
})
```

## TSV

Serialises to tab separated file.

```r
app$get("/tsv", function(req, res){
  res$tsv(mtcars, "more-cars")
})
```

## htmlwidget

Serialises an htmlwidget

```r
library(echarts4r)

app$get("/tsv", function(req, res){
  plot <- e_charts(cars, speed) %>% 
    e_scatter(dist)
  res$htmlwidget(plot)
})
```

