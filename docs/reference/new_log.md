# `new_log`

Logger


## Description

Returns a new logger using the `log` package.


## Usage

```r
new_log(prefix = ">", write = FALSE, file = "ambiorix.log", sep = "")
```


## Arguments

Argument      |Description
------------- |----------------
`prefix`     |     String to prefix all log messages.
`write`     |     Whether to write the log to the `file` .
`file`     |     Name of the file to dump the logs to, only used if `write` is `TRUE` .
`sep`     |     Separator between `prefix` and other flags and messages.


## Value

An R& of class `log::Logger` .


## Examples

```r
log <- new_log()
log$log("Hello world")
```


