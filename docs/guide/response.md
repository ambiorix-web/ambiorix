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
