FROM rocker/r-ver:latest
RUN apt-get update && apt-get install -y \
  git-core \
  libssl-dev \
  r-cran-httpuv
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'))" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("mime")'
RUN R -e 'install.packages("readr")'
RUN R -e 'install.packages("htmltools")'
RUN R -e 'install.packages("htmlwidgets")'
RUN R -e 'remotes::install_github("ambiorix-web/ambiorix")'
RUN R -e "install.packages('htmltools')"
ARG app=/usr/local/lib/R/site-library/ambiorix/app.R
WORKDIR /app
RUN cp ${app} /app/app.R
EXPOSE 3000
CMD R -e "options(ambiorix.host='0.0.0.0', 'ambiorix.port'=3000);source('app.R')"
