# `robj`

Data Object


## Description

Treats a data element rendered in a response ( `res$render` ) as
 a data object and ultimately uses [`dput()`](#dput()) .


## Usage

```r
robj(obj)
```


## Arguments

Argument      |Description
------------- |----------------
`obj`     |     R object to treat.


## Details

For instance in a template, x <- [% var %] will not work with
 `res$render(data=list(var = "hello"))` because this will be replace
 like `x <- hello` (missing quote): breaking the template. Using `robj` one would
 obtain `x <- "hello"` .


