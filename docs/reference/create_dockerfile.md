# `create_dockerfile`

Dockerfile


## Description

Create the dockerfile required to run the application.
 The dockerfile created will install packages from
 RStudio Public Package Manager
 which comes with pre-built binaries
 that much improve the speed of building of Dockerfiles.


## Usage

```r
create_dockerfile(port, host = "0.0.0.0")
```


## Arguments

Argument      |Description
------------- |----------------
`port, host`     |     Port and host to serve the application.


## Details

Reads the `DESCRIPTION` file of the project to produce the `Dockerfile` .


## Examples

```r
create_dockerfile()
```


