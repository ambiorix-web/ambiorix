# Other web frameworks

These are other web frameworks for the R programming language that you might want to check out. All are, like ambiorix, built on top of [httpuv](https://github.com/rstudio/httpuv).

## Shiny

The most popular framework to produce web applications in R is by far [shiny](https://github.com/rstudio/shiny) created and maintained by RStudio. It allows easily creating single page web applications, crucially it works very differently internally; namely thanks to reactivity.

If you can from Python, shiny is more like plotly dash while ambiorix is closer to django.

## Prairie

[Prairie](https://github.com/nteetor/prairie) is another web framework by Nate Teetor which works in a rather similar way as ambiorix.

## Fiery

[Fiery](https://github.com/thomasp85/fiery) by Thomas Lin Pedersen, though also built on top of httpuv, provides a very different interface:

> Fiery is designed around a clear server life-cycle with events being triggered at specific points during the life-cycle that will call the handlers attached to these events.
