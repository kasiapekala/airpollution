# source: https://gist.github.com/smithdanielle/9913897
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages <- c("shiny","shinydashboard","shinyWidgets","shinydashboardPlus","DT","plyr","dplyr","tidyr","lubridate","ggplot2","RColorBrewer","leaflet","httr","rlist","stringr","stringi","sp","xts","readxl","scales")
check.packages(packages)