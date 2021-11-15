options(shiny.trace = F)  

#####---base
library(shiny)
library(shiny.semantic)    
library(semantic.dashboard)  
library(shinycssloaders)
library(ggplotify)
library(plotly)

root_folder = "./"
source(paste0(root_folder,"Functions.R"))

source("modules/Dashboard_file.R",local=TRUE)  
 