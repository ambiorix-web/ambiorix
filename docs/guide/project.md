
# Project & Templates

The easiest way to get setup is by creating an ambiorix project with `create_ambiorix`. 

```r
ambiorix::create_ambiorix("myapp")
```

This allows using templates and rendering them with `res$render`. These templates can make use of `[% tags %]` which are replaced with item found in data.

The following template

```r
# templates/home.R
library(htmltools)

tags$html(
  lang = "en",
  tags$head(
    tags$meta(charset= "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "static/style.css"),
    tags$title("Ambiorix")
  ),
  tags$body(
    tags$h1("[% title %]") # tag
  )
)

```

The `[% title %]` can then be replaced with.

```r
res$render("home", data = list(title = "Hello from R"))
```

Lists and dataframes are `dput` in the template.

One can also use HTML templates (`.html` files) in which case the data is serialised to JSON.
