# create User Interface -------------------------------------------------

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

# note this is both in ui and server - remember to change both
type.ns <- c("splicing", "frameshift deletion", "frameshift insertion", "nonframeshift deletion", "nonframeshift insertion", "nonsynonymous SNV", "stoploss", "stopgain", "startloss")
type.s <- c("synonymous SNV")
type.int <- c("intronic", "UTR3", "UTR", "UTR5")


## Dashboard on top -------------------------------------------------
dashboard <- dashboardHeader(title = strong("ShinyHeatmap"),
                             tags$li(a(img(src = 'logoIOR.svg',height = "50px"), style = "padding-top:00px; padding-bottom:00px; background: #F5F5F5; width: 100px; border-right:0px;"), class = "dropdown"),
                             tags$li(a(img(src = 'logoUSI.svg',height = "50px"), style = "padding-top:00px; padding-bottom:00px; background: #F5F5F5; width: 100px"), class = "dropdown"),
                             tags$li(a(img(src = 'logoETH.svg',height = "50px"), style = "padding-top:00px; padding-bottom:00px; background: #F5F5F5; width: 200px"), class = "dropdown"))


## Sidebar left side -------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    #change here for order - first one is the opening page
    menuItem("ShinyHeatmap", icon = icon("calculator", class = "duotone"), tabName = "ShinyHeatmap"),
    menuItem("Read Me", icon = icon("info"), tabName = "readme"),
    h6("Lodovico Terzi di Bergamo", style="padding-top: 10px; padding-left: 10px; font-size: 15px"),
    h6("lodovico.terzi@ior.usi.ch", style="padding-bottom: 30px; padding-left: 10px")),
  tags$li(a(onclick = "onclick =window.open('https://github.com/LodovicoTerzi')", href = NULL, icon("github"), title = "GitHub", style = "cursor: pointer; padding-left: 40px; font-size: 36px; list-style-type: none;")),
  collapsed = TRUE)



## Body of the application -------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "ShinyHeatmap",
            shinyjs::useShinyjs(),
            
            fluidRow(
              
              column(2,
                     
                     div(style="width: 100%; margin-bottom: 0px", fileInput("file", label = "Please select input file", accept = c(".xlsx", ".xls"))),
                     div(style="display: inline-block; vertical-align:top; width: 100%; margin-top: -20px", actionButton("loadData", "Load Data")),
                     h1(),
                     pickerInput(inputId = "SynonymousSelection",label = "SynonymousSelection", choices = c(type.ns, type.s, type.int), selected = type.s,multiple = TRUE),
                     pickerInput(inputId = "NonSynonymousSelection",label = "NonSynonymousSelection", choices = c(type.ns, type.s, type.int), selected = type.ns,multiple = TRUE),
                     pickerInput(inputId = "IntronicSelection",label = "IntronicSelection", choices = c(type.ns, type.s, type.int), selected = type.int,multiple = TRUE),
                     h3(),
                     selectInput(inputId = "GenePercentage", label="Genes mutatated in X% patients", choices=c(0,1,3,5,10,50), selected=0, multiple=F),
                     checkboxInput(inputId = "MutatedPatientsYesNo", label="Add unmutated patients", value = TRUE),
                     checkboxInput(inputId = "OnlyNonSynonymous", label="Only non-synonymous muts", value = FALSE),
                     numericInput(inputId = "HeatmapHeight", label="Height", value=16),
                     numericInput(inputId = "HeatmapWidth", label="Width", value=8),
                     numericInput(inputId = "LabelSize", label="Label size", value=4),
                     #div(style="color: #fff; background-color: #337ab7; border-color: #2e6da4", actionButton("plotButton", "Let's go!")),
                     div(style="display: inline-block; width: 100%; margin-top: 30px; display: flex;justify-content: center;align-items: center;", circleButton("plotButton", "Go!", style="color: #fff; background-color: #3CB371; border-color: black; top-indent: -10px; text-align:center;position: relative; line-height: 10px;", size = "default")),
                     div(style="display:inline-block;display:center-align; margin-top: 10px; width: 100%;", textInput("FilenameDownload", width='100%', label="Filename")),
                     div(style="display:inline-block;display:center-align; margin-top: 3px; width: 100%; display: flex;justify-content: center;align-items: center;", shinyjs::hidden(downloadButton("downloadAll", 'Download tables'))),
                     div(style="display:inline-block;display:center-align; margin-top: 3px; width: 100%; display: flex;justify-content: center;align-items: center;", shinyjs::hidden(downloadButton("downloadPlot", label = "Download heatmap"))),
                     div(style="display:inline-block;display:center-align; margin-top: 3px; width: 100%; display: flex;justify-content: center;align-items: center;", shinyjs::hidden(downloadButton("downloadPlotByType", label = "Download heatmap by type"))),
              ),
              
              
              column(10,
                     
                     tabsetPanel(
                       tabPanel("Table-Complete", DTOutput('PlotDataTableComplete')),
                       tabPanel("Table-All ", DTOutput('PlotDataTable')),
                       tabPanel("Table-Synonymous", DTOutput('PlotDataTableSynonymous')),
                       tabPanel("Table-NonSynonymous", DTOutput('PlotDataTableNonSynonymous')),
                       tabPanel("Table-Intronic", DTOutput('PlotDataTableIntronic')),
                       tabPanel("Heatmap", plotOutput('PlotHeatmap', height=1500),
                                downloadablePlotUI("object_id1", 
                                                   downloadtypes = c("png", "csv"), 
                                                   download_hovertext = "Download the plot and data here!",
                                                   height = "500px", 
                                                   btn_halign = "left")),
                       tabPanel("HeatmapByType", plotOutput('PlotHeatmap_byType', height=1500)),
                       tabPanel("HeatmapBy3Types", plotOutput('PlotHeatmap_byType3types', height=1500))), 
                     )
              
              
            ),
            
            #color of switch input
            tags$head(tags$style(HTML('
                                      /* switch input */
                                       .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-Yes,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-Yes {background: #3CB371; color: white;}
                                       .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-No,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-No {background: #191970; color: white;}
                                      /* logo */
                                       .skin-blue .main-header .logo {background-color: #00688B; color: #FF4500}
                                      /* logo when hovered */
                                       .skin-blue .main-header .logo:hover {background-color: #FF4500; color: #00688B;}
                                      /* header bar */
                                       .skin-blue .main-header .navbar {background-color: #191970;}
                                      /* active selected tab in the sidebarmenu */
                                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #F5F5F5; color: #FF4500;}
                                      /* background */ 
                                       .content-wrapper, .right-side {background-color: #F5F5F5;}
                                      /* panels color */ 
                                       .tabbable > .nav > li > a {color: black;}
                                      /* remove bullet point from github */ 
                                       li {list-style-type: none;}
                                      /* toggle button when hovered  */
                                       .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #191970; color: #F5F5F5;}
                                      /* active tab  */
                                      .tabbable > .nav > li[class=active]    > a {background-color: #3CB371; color:#F5F5F5}
                                      ')))),
    tabItem(tabName = "readme",
            uiOutput("pdfview"))
  ))




ui <- shinyUI(dashboardPage(dashboard, sidebar, body))
