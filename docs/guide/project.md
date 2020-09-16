
# Project & Templates

The easiest way to get setup by creating an ambiorix project. This will setup a static directory, 404 page, websockets, etc.

## Project 

Create the project with `create_ambiorix` or with the [ambiorix-cli](https://github.com/JohnCoene/ambiorix-cli).

<!-- tabs:start -->

#### ** R **

```r
ambiorix::create_ambiorix("myapp")
```

#### ** CLI **

```bash
ambiorix-cli create myapp
```

<!-- tabs:end -->

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
│   ├── home.html
│   └── partials
│       └── header.html
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
    tags$h1("[% title %]")
  )
)
```

The `[% title %]` can then be replaced with.

```r
res$render("home", data = list(title = "Hello from R"))
```

R objects can also be passed by placing it in `robj()` which indicates the object should be used as-is: internally ambiorix will use `dput`.

```r
# templates/home.R
library(htmltools)

dataset <- [% df %]

tags$html(
  lang = "en",
  tags$head(
    tags$meta(charset= "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "static/style.css"),
    tags$title("Ambiorix")
  ),
  tags$body(
    tags$pre(
      tags$code(
        jsonlite::toJSON(dataset)
      )
    )
  )
)
```

```r
res$render("home", data = list(df = robj(cars)))
```

Note that since the `[% tags %]` are passed to `glue::glue_data` internally they can therefore include R code.

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
    tags$h1("[% if(x) 'Hello' else 'Bye!' %]")
  )
)
```

```r
res$render("home", data = list(x = TRUE))
```

### HTML

One can also use HTML templates (`.html` files) in which case the data is serialised to JSON. This also uses `glue::glue_data` internally.

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

### Partials

You can also use partials (inspired by [gohugo](https://gohugo.io)), blocks of reusable HTML content. These are used with a different tag: `[! partial_name.html !]`.

Therefore the template below (`templates/home.html`).

```html
<!-- templates/home.html -->
<!DOCTYPE html>
<html lang="en">
<head>
  [! header.html !]
  <title>Ambiorix</title>
</head>
<body>
  <h1 class="brand">Hello</h1>
</body>
</html>
```

Imports the HTML at: `templates/partials/header.html `

```html
<!-- templates/partials/header.html -->
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link rel="stylesheet" href="static/style.css">
<script src="static/ambiorix.js"></script>
```

To produce the following output.

```html
<!-- templates/home.html -->
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="static/style.css">
  <script src="static/ambiorix.js"></script>
  <title>Ambiorix</title>
</head>
<body>
  <h1 class="brand">Hello</h1>
</body>
</html>
```
