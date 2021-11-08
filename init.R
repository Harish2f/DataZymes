my_packages = c("shiny","shinydashboard","dplyr","shinycssloaders","shinythemes","shinyWidgets","bsplus","htmltools",
                "giphyr","readxl","plotly","shinyalert","shinyjs","timevis","vistime","leaflet","lubridate","janitor",
                "tidyverse","ggplot2","DT","shinyBS","viridis","cluster","fpc","rpart")


install_if_missing = function(p) {
  if(p %in% rownames(installed.packages()) == FALSE) {
    
    install.packages(p)
  }
  
}


invisible(sapply(my_packages,install_if_missing))