# Deploying

## Docker

The easiest way to deploy an ambiorix app is using docker. 

### Existing Image

This project comes with an image you can pull and use.

```bash
docker pull jcoenep/ambiorix
```

By default the image will use an internal very basic application and binds it to port `3000`.

```bash
docker run -p 3000:3000 jcoenep/ambiorix
```

To use your own application mount your app in the images `/app` directory, so from the root of the app this would look like this:

```bash
docker run -v "$(pwd)"/.:/app -p 3000:3000 jcoenep/ambiorix
```

### Generate

Or you can generate your own dockerfile.

```r
create_dockerfile(port = 3000L)
```

The function `create_dockerfile` will parse the `DESCRIPTION` file to create a `Dockerfile`, below is an example of the output.

```dockerfile
FROM jcoenep/ambiorix
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e "remotes::install_github('JohnCoene/echarts4r')"
COPY . .
EXPOSE 3000
RUN R -e "options(ambiorix.host='0.0.0.0', 'ambiorix.port'=3000);source('app.R')"
```

You can then build the image.

```docker
docker build -t ambiorix .
```

Finally, run it.

```docker
docker run ambiorix
```

With docker installed this will work just as well on your machine as on a remote server.
