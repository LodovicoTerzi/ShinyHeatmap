

######################################################################################
####### Application for creating mutational heatmap from list of mutation  ###########
#######              Coded by: Lodovico Terzi di Bergamo                   ###########
#######                    DR Lab - 14.07.2023                             ###########
######################################################################################

library(BiocManager)
options(repos = BiocManager::repositories())
options(repos = getOption("repos"))
library(shiny)
library("shinydashboard")
library("shinyWidgets")
library("shinyjs")
library("shinyBS")
library(DT)
library(ComplexHeatmap)
library(periscope)
library(readxl)

# Run the application 
shiny::runApp()

