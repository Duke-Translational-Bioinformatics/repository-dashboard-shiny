FROM rocker/shiny:latest

MAINTAINER Ben Neely "nigelneely@gmail.com"

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev

# Load app-specific packages not loaded in rocker/shiny
RUN R -e "install.packages(c('httr', 'ggplot2', 'chron'), repos='https://cran.rstudio.com/')"

EXPOSE 3838

ADD dashboard /srv/shiny-server/dashboard

CMD ["/usr/bin/shiny-server.sh"]
