FROM rocker/r-ver:latest
RUN apt-get update && apt-get install -y \
  git-core \
  libssl-dev
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'))" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("htmltools", "ambiorix"))'
ARG app=/usr/local/lib/R/site-library/ambiorix/app.R
WORKDIR /app
RUN cp ${app} /app/app.R
EXPOSE 3000
CMD R -e "options(ambiorix.host='0.0.0.0', 'ambiorix.port'=3000);source('app.R')"
