##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: A script to download all required packages to run the application and set up the backend database
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

##adapted from: https://stackoverflow.com/questions/38928326/is-there-something-like-requirements-txt-for-r?noredirect=1&lq=1
pkgLoad <- function() {
  
  req.packages <- c("data.table", "plyr", "dplyr", "shiny", "shinyjs", "parallel", "ggplot2", "utils", "plotly", "DT", "googlesheets4", "knitr",
                    'shinydashboard', "shinyalert", "ggthemes", "waiter", "shinycssloaders", "reshape2", "tidyr", "stringr", "lubridate",
                    "scales", "maps", "timevis", "sodium", "V8", "shinyBS", "htmltools", "reticulate", "leaflet", "sf", "leaflet.extras",
                    "Hmisc", "usmap", "hash", "readxl", "pracma", "deSolve", "RColorBrewer", "viridis", "car", "mapproj", "ggstance",
                    "assertthat", "parallel", "pbapply", "doParallel", "argparser", "Rcpp")
  
  packagecheck <- match( req.packages, utils::installed.packages()[,1] )
  
  packagestoinstall <- req.packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall)
  } else {
    print( "All requested packages already installed" )
  }
  
}

pkgLoad()

## set up the database
source(file = "code/build_database.R")