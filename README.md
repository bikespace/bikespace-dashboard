# BikeSpace Dashboard

Dashboard application to visualize user-submited bike parking issues in the City
of Toronto from [app.bikespace.ca](https://app.bikespace.ca/). Built using R Studio's 
[Shiny](https://shiny.rstudio.com/) platform, [Leaflet](https://leafletjs.com/), [Highcharts](https://www.highcharts.com/), and [Mapbox](https://www.mapbox.com/).

## Structure

The application  is located in `bikespace-dashboard` directory and consists of the [global.R](https://gitlab.com/bikespace/shiny-dashboard/blob/master/bikespace-dashboard/global.R), [ui.R](https://gitlab.com/bikespace/shiny-dashboard/blob/master/bikespace-dashboard/ui.R), [server.R](https://gitlab.com/bikespace/shiny-dashboard/blob/master/bikespace-dashboard/server.R), [report.Rmd](https://gitlab.com/bikespace/shiny-dashboard/blob/master/bikespace-dashboard/report.Rmd) files and the [www/](https://gitlab.com/bikespace/shiny-dashboard/tree/master/bikespace-dashboard/www) directory, which contains the [BikeSpace logo](https://gitlab.com/bikespace/shiny-dashboard/blob/master/bikespace-dashboard/www/header_logo.png) and the [custom.css](https://gitlab.com/bikespace/shiny-dashboard/blob/master/bikespace-dashboard/www/custom.css) file

## Installation

To run the application locally, download [R](https://www.r-project.org/) and [R Studio](https://www.rstudio.com/), an IDE for the R programming language (not necessary but highly reccomended).

Once downloaded, the following code will install the necessary packages to run the application:

```r
# For easy package install in local environment:
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg))
     install.packages(new.pkg, dependencies = TRUE)
   sapply(pkg, require, character.only = TRUE)
 }

packages <- c("shiny", "shinydashboard", "RCurl", "jsonlite", "igraph","highcharter",
               "rvest", "purrr", "tidyr", "dplyr", "stringr", "leaflet", "htmltools",
               "RColorBrewer", "rmarkdown", "webshot", "shinyBS", "shinyjs", "shinyWidgets",
               "ggplot2", "Hmisc")
 
check.packages(packages)

```

### PDF export

The PDF export functionality of the application requires a LaTeX distribution and [PhantomJS](http://phantomjs.org/) to be installed. 

[MikTeX](https://miktex.org/) is our reccommended LaTeX distribution, while you can install PhantomJS through R by running ```webshot::install_phantomjs()```

## Launch

Launch the application by running the ```runApp()``` command within the working directory where the application files are located.

## Contact

For more information about BikeSpace, check out our [website](http://www.bikespace.ca/)


