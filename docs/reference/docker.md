# Dockerfile

## Description

Create the dockerfile required to run the application.
The dockerfile created will install packages from 
[RStudio Public Package Manager](https://packagemanager.rstudio.com/client/#/) 
which comes with pre-built binaries
that much improve the speed of building of Dockerfiles.

## Usage

```r
dockerfile(port, host = "0.0.0.0")
```

## Arguments

- `port`, `host`: Port and host to serve the application.
