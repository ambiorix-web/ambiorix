# Treat R Objects

## Description

Treats a data element rendered in a response (`res$render`) as a data object and ultimately uses [dput()].

For instance in a template, `x <- [% var %]` will not work with `res$render(data=list(var = "hello"))` because this will be replace like `x <- hello` (missing quote): breaking the template. Using `robj` one would obtain `x <- "hello"`.

## Usage

```r
res$render("home", data = list(x = robj("A string")))
```

## Arguments

- `obj`: The R object to treat

