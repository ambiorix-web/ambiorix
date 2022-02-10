# `Logger`

Logger


## Description

Log events to `ambiorix.log` .


## Details

The logger prepends every `write` with the current timestamp obtained with [`Sys.time()`](#sys.time()) .
 Every `write` is a single line in the log.


## Value

An object of class `Logger` that can be used to log
 events in an application.


