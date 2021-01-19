# Logger

__The logger has been deprecated in favour of `Logger` from the [log](https://log.opifex.org) package.__

## Description

Log class to write to `ambiorix.log`

## Fields

- `log`: Whether to actually log events to the file, if `FALSE` the `write` method has no effect.

## Methods

### Constructor

Instantiate a logger.

- `log`: Whether to actually log events to the file, if `FALSE` the `write` method has no effect.

```r
Logger$new()
```

### Log

Write a line to the logger.

- `label`: The event label.
- `...`:  Any other text to log on the line.

```r
Logger$new()$log("Homepage", "was visited!")
```

