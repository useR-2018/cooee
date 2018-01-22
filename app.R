required_packages <- c("shiny", "tidyverse", "googlesheets", "DT", "jsonlite", "purrr")
missing_packages <- !(required_packages %in% rownames(installed.packages()))
if(any(missing_packages)){
  stop(paste0('Some additional packages are necessary to run this app. Please install them using this command:
       install.packages(c("', paste0(required_packages[missing_packages], collapse = '", "'), '"))'))
}

library(shiny)
runApp(port = 4642)
