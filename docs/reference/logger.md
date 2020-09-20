# Logger

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

### Write

Write a line to the logger.

- `label`: The event label.
- `...`:  Any other text to write on the line.

```r
Logger$new()$write("Homepage", "was visited!")
```

