FROM rocker/r-base:latest
RUN apt-get update && apt-get install -y git-core
LABEL maintainer="jcoenep@gmail.com"
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'))" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("JohnCoene/ambiorix")'
RUN R -e 'install.packages("mime")'
RUN R -e 'install.packages("readr")'
RUN R -e 'install.packages("htmltools")'
RUN R -e 'install.packages("htmlwidgets")'
ARG app=/usr/local/lib/R/site-library/ambiorix/app.R
RUN cp ${app} ~/.
EXPOSE 3000
RUN R -e "options(ambiorix.host='0.0.0.0', 'ambiorix.port'=3000);source('app.R')"
