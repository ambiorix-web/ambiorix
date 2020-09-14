
# Project & Templates

The easiest way to get setup by creating an ambiorix project. This will setup a static directory, 404 page, websockets, etc.

## Project 

Create the project with `create_ambiorix`. 

```r
ambiorix::create_ambiorix("myapp")
```

This creates a directory with the following file structure.

```
.
├── DESCRIPTION
├── app.R
├── assets
│   ├── ambiorix.js
│   └── style.css
├── templates
│   ├── 404.html
│   ├── about.R
│   └── home.html
└── views
    └── base.R
```

## Templates

A project allows using templates and rendering them with `res$render`. These templates can make use of `[% tags %]` which are replaced with item found in data.

### R

The following template file is written with the [htmltools](https://CRAN.R-project.org/package=htmltools) package and contains a `[% tag %]`.

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
res$render("home", data = list(title = I("Hello from R")))
```

Lists and dataframes are `dput` in the template so you can use them to dynamically create content. Note that since we use `dput` to place the data in the template above we use `I` to specify that the title should be used as-is, i.e.: without quotes. Otherwise the title would appear as `"Hello form R"` rather than `Hello from R`. 

### HTML

One can also use HTML templates (`.html` files) in which case the data is serialised to JSON.

```html
<!-- templates/home.html -->
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="static/style.css">
  <script src="static/ambiorix.js"></script>
  <script>
    var wss = new Ambiorix();
    wss.receive("hello", function(msg){
      alert(msg);
    });
    wss.start();
  </script>
  <title>Ambiorix</title>
</head>
<body>
  <h1 class="brand">[% title %]</h1> <!-- tag -->
  <button onclick="Ambiorix.send('hello', 'Hi from the client')">Send a message</button>
</body>
</html>
```

This is rendered with the same method.

```r
res$render("home", data = list(title = "Hello from R"))
```
