FROM rocker/r-base
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("JohnCoene/ambiorix")'
RUN R -e "install.packages('promises')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "remotes::install_github('JohnCoene/echarts4r')"
COPY . .
EXPOSE 3000
RUN R -e "options(ambiorix.host='0.0.0.0', 'ambiorix.port'=${port});source('app.R')"
