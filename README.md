<div align="center">

# ambiorix

<img src="man/figures/ambiorix.png" height = "200px"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/devOpifex/ambiorix/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/devOpifex/ambiorix/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/devOpifex/ambiorix/branch/master/graph/badge.svg)](https://app.codecov.io/gh/devOpifex/ambiorix?branch=master)
<!-- badges: end -->

[Website](https://ambiorix.dev) | [CLI](https://github.com/devOpifex/ambiorix-cli) | [Generator](https://github.com/devOpifex/ambiorix.generator) | [Docker](https://hub.docker.com/r/jcoenep/ambiorix) | [Load Balancer](https://github.com/devOpifex/belgic)

Web framework for R based on [httpuv](https://github.com/rstudio/httpuv) and inspired by [express.js](https://github.com/expressjs/express).

</div>


## Example

``` r
library(ambiorix)

app <- Ambiorix$new()

app$get("/", function(req, res){
  res$send("Hello!")
})

app$get("/about", function(req, res){
  res$send("About page")
})

app$start()
```

## Middlewares

- [druid](https://github.com/devOpifex/druid) Logger
- [alesia](https://github.com/devOpifex/alesia) Minifier
- [eburones](https://github.com/devOpifex/eburones) Sessions
- [agris](https://github.com/devOpifex/druid) Security
- [scilis](https://github.com/devOpifex/scilis) Cookies
- [titan](https://github.com/devOpifex/titan) Prometheus metrics
- [surf](https://github.com/devOpifex/surf) CSRF protection
- [signaculum](https://github.com/devOpifex/signaculum) favicon
- [pugger](https://github.com/devOpifex/pugger) Pug engine
- [jader](https://github.com/devOpifex/jader) Jade engine

## Tools & Extensions

- [belgic](https://github.com/devOpifex/belgic) Load balancer
- [packer](https://github.com/JohnCoene/packer) JavaScript
- [CLI](https://github.com/devOpifex/ambiorix-cli) for generator
- [Generator](https://github.com/devOpifex/ambiorix.generator) Project generator
- [Docker](https://hub.docker.com/r/jcoenep/ambiorix) Docker image

## Install

The stable version is available on CRAN with:

```r
install.packages("ambiorix")
```

You can also install the development version from Github:

```r
# install.packages("ambiorix")
remotes::install_github("devOpifex/ambiorix")
```

## Contributing

Please note that the ambiorix project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
