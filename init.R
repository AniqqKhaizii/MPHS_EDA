# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("DT", "dplyr", "fpp2", "forecast", "ggplot2", "httr", "jsonlite", "lubridate", "openxlsx", "plotly", "readr", "readxl", "shiny", "shinydashboard",
                "shinydashboardPlus", "shinyWidgets","TTR", "tidyr", "tibble", "tidyverse", "likert", "bslib", "dashboardthemes", "highcharter", "leaflet")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))