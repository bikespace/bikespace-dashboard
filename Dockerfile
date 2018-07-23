# To build, cd to the shiny server directory, then:
#   docker build -t ss-devel docker/ubuntu16.04/
#
# To run:
#   docker run --rm -ti -p 3838:3838 -v $(pwd):/shiny-server --name ss ss-devel

FROM ubuntu:16.04

LABEL maintainer="sonal.ranjit3@gmail.com"

# =====================================================================
# R
# =====================================================================

# Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTED=noninteractive

# Need this to add R repo
RUN apt-get update && apt-get install -y software-properties-common

# Add R apt repository
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $(lsb_release -cs)/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Install basic stuff and R
RUN apt-get update && apt-get install -y \
    sudo \
    git \
    vim-tiny \
    less \
    wget \
    r-base \
    r-base-dev \
    r-recommended \
    fonts-texgyre

RUN echo 'options(\n\
  repos = c(CRAN = "https://cran.r-project.org/"),\n\
  download.file.method = "libcurl",\n\
  # Detect number of physical cores\n\
  Ncpus = parallel::detectCores(logical=FALSE)\n\
)' >> /etc/R/Rprofile.site

# Create docker user with empty password (will have uid and gid 1000)
RUN useradd --create-home --shell /bin/bash docker \
    && passwd docker -d \
    && adduser docker sudo

# Don't require a password for sudo
RUN sed -i 's/^\(%sudo.*\)ALL$/\1NOPASSWD:ALL/' /etc/sudoers

# =====================================================================
# Shiny Server dev stuff + Shiny
# =====================================================================

RUN apt-get update && apt-get install -y \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    cmake \
    # Pro-specific
    libpam0g-dev \
    openjdk-8-jre \
    libcurl3 \
    apt-transport-https

# install miktex
RUN sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys D6BC243565B2087BC3F897C9277A7293F59E4889 && \
    echo "deb http://miktex.org/download/ubuntu xenial universe" | sudo tee /etc/apt/sources.list.d/miktex.list && \
    sudo apt-get update && \
    sudo apt-get install -y miktex

RUN apt-get install -y texlive-latex-base \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

EXPOSE 3838

RUN R -e "install.packages(c('devtools', 'shiny', 'shinydashboard', 'RCurl', 'jsonlite', 'highcharter', \
                             'rvest', 'purrr', 'tidyr', 'dplyr', 'stringr', 'leaflet', 'htmltools', \
                             'RColorBrewer', 'webshot', 'shinyBS', 'shinyjs', 'shinyWidgets', 'ggplot2', 'Hmisc'), repos='http://cran.rstudio.com/')"

RUN R -e "devtools::install_version('rmarkdown', version='1.8')" 

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY /bikespace-dashboard /srv/shiny-server/
COPY shiny-server.sh /usr/bin/shiny-server.sh


EXPOSE 80

## install packages from github (and clean up)
RUN Rscript -e "webshot::install_phantomjs()"
RUN mv /root/bin/phantomjs /usr/bin
RUN phantomjs --version

CMD ["/usr/bin/shiny-server.sh"]