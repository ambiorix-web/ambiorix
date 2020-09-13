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

An `.html` or `.R` file can also be rendered. The difference with `send_file` is that it will use `data` to replace `[% tags %]`. When an HTML file is used then the data is serialised to JSON, when using an `.R` file the data is `dput` in stead of the tag, unless the data object is wrapped with `I` in which case it is used as-is.

```r
# renders templates/home.html
# replaces [% title %]
app$get("/:book", function(req, res){
  res$render("home", data = list(title = req$params$book))
})
```

