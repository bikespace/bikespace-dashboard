FROM rocker/shiny

LABEL Sonal Ranjit (sonal.ranjit3@gmail.com)

## install R package dependencies (and clean up)
RUN apt-get update && apt-get install -y gnupg2 \
    libssl-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## install pacakges form CRAN (and clean up)
RUN Rscript -e "install.packages(c('shiny', 'shinydashboard', 'RCurl', 'jsonlite', 'highcharter','rvest', 'purrr', 'tidyr', 'dplyr', 'stringr', 'leaflet', 'htmltools','RColorBrewer', 'rmarkdown', 'webshot', 'shinyBS', 'shinyjs', 'shinyWidgets','ggplot2', 'Hmisc'), repos='https://cran.rstudio.com/')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## assume shiny app is in folder /bikespace-dashboard
COPY ./bikespace-dashboard /srv/shiny-server/bikespace-dashboard/

COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf