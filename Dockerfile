FROM rocker/shiny-verse:3.5.3 

# Add libproj-dev and libgdal-dev if using the rgdal library
RUN apt-get update && apt-get install libcurl4-openssl-dev libv8-3.14-dev -y &&\
    mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install library
RUN R -e "install.packages(c('shinydashboard', 'shinyjs', 'lubridate', 'DT', 'remotes'), repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('GuillaumePressiat/nomensland')"

# copy the app to the image
COPY shinyapps /srv/shiny-server/
COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /srv/shiny-server/
RUN chmod -R 755 /usr/local/lib/R/etc/

EXPOSE 3838

CMD R -e "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=21010)"
