# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("DT", "dplyr", "ggplot2", "httr", "jsonlite", "lubridate", "openxlsx", "plotly", "readr", "readxl", "shiny", "shinydashboard",
                "shinydashboardPlus", "tidyr", "dashboardthemes", "highcharter")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
