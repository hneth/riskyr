## ui.R
## riskyr | R Shiny | spds, uni.kn | 2018 01 03
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# rm(list=ls()) # clean all.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Dependencies:

library(shiny)
library(shinyBS)
library(markdown)
library(DT)
library(diagram)
library(shape)
library(tidyr)
library(dplyr)
library(ggplot2)
library(vcd)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Initial environment:

e1 <- list("name" = "Demo",  # name (e.g., HIV, mammography, ...)
           "N" = 100,        # N in population
           "prev" = .15,     # prevalence in population = p(true positive)
           "sens" = .85,     # sensitivity = p(positive decision | true positive)
           "spec" = .75,     # specificity = p(negative decision | true negative)
           "source" = "source information" # information source (e.g., citation)
)

env <- e1 # from current environment

## Import ready-made and worked out example data:
datasets <- read.csv2("./www/datasets_riskyr.csv", stringsAsFactors = FALSE)
            # WAS: read.csv("./www/riskyR_datasets.csv", stringsAsFactors = FALSE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## JavaScript:

{
  ## Source: https://stackoverflow.com/questions/30502870/shiny-slider-on-logarithmic-scale
  
  ## logifySlider javascript function: 
  JS.logify <-
    "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
if (sci) {
// scientific style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
})
} else {
// regular number style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return (Math.pow(10, num)); }
})
}
}"

## call logifySlider for each relevant sliderInput: 
JS.onload <-
  "
// execute upon document loading: 
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
logifySlider('log_slider', sci = false)
logifySlider('log_slider2', sci = true)
}, 5)})
"

}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Define user interface logic:

shinyUI(
  
  # tags$head(tags$script(HTML(JS.logify))),
  # tags$head(tags$script(HTML(JS.onload))),
  
  navbarPage(title = "riskyr",
             theme = "bootstrap.sandstone.css", # or any other bootsstrap theme https://bootswatch.com/3/
             
             #####               
             tabPanel("1: Representations",
                      icon = icon("tree-deciduous", lib = "glyphicon"),
                      
                      #####
                      sidebarLayout(
                        
                        # Sidebar panel for inputs:
                        sidebarPanel(
                          
                          # Input: Select all input values:
                          textInput("name",
                                    label = "Condition name:",
                                    value = "Condition X"),
                          
                          br(), # add horizontal space

                          tags$b("Population size:"),
                          tags$br("(Use slider or enter number)"),
                          sliderInput("N",
                                      label = NULL,
                                      value = 100,
                                      min = 0,
                                      max = 10^6,
                                      step = 10
                                      ), # use log-scale from 1 to 10^9
                          numericInput("numN", 
                                       label = NULL, 
                                       value = 100,
                                       min = 0,
                                       max = 10^6,
                                       step = 10),
                          br(),
                          tags$b("Prevalence:"),
                          tags$br("(Use slider or enter number)"),
                          sliderInput("prev", 
                                      label = NULL, sep = "",
                                      value = 0.15, 
                                      min = 0,
                                      max = 1,
                                      step = 10^-3),
                          numericInput("numprev", 
                                       label = NULL, 
                                       value = 0.15,
                                       min = 0,
                                       max = 1,
                                       step = 10^-3),
                          br(),
                          tags$b("Sensitivity:"),
                          tags$br("(Use slider or enter number)"),
                          sliderInput("sens", 
                                      label = NULL, sep = "",
                                      value = 0.85,
                                      min = 0,
                                      max = 1,
                                      step = 10^-3),
                          numericInput("numsens", 
                                       label = NULL, 
                                       value = 0.85,
                                       min = 0,
                                       max = 1,
                                       step = 10^-3),
                          br(),
                          tags$b("Specificity:"),
                          tags$br("(Use slider or enter number)"),
                          sliderInput("spec", 
                                      label = NULL, sep = "",
                                      value = 0.75,
                                      min = 0,
                                      max = 1, 
                                      step = 10^-3),
                          numericInput("numspec", 
                                       label = NULL, 
                                       value = 0.75,
                                       min = 0,
                                       max = 1,
                                       step = 10^-3),
                          
                          br(), 
                          
                          ## Provide existing data sets as drop-down list:
                          selectInput("dataselection", label = "Or view an example:", 
                                      choices = setNames(as.list(1:nrow(datasets)), # create choices from datasets
                                                         datasets$dataset), 
                                      selected = 1),
                          
                          bsButton("inputhelp", label = "Help", 
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action"),
                          
                          ## Tooltips on inputs:
                          bsTooltip(id = "N", title = "Number of individuals making up the population",
                                    placement = "right", trigger = "hover", options = list(container = "body")), 
                          
                          bsTooltip(id = "prev", title = "Probability of being affected: p(true)",
                                    placement = "right", trigger = "hover", options = list(container = "body")),
                          
                          bsTooltip(id = "sens", title = "Probability of correctly detecting an affected individual: p(decision positive | condition true)",
                                    placement = "right", trigger = "hover", options = list(container = "body")), 
                          
                          bsTooltip(id = "spec", title = "Probability of correctly rejecting an unaffected individual: p(decision negative | condition false) = 1 - FA",
                                    placement = "right", trigger = "hover", options = list(container = "body"))
                        ),
                        #####
                        ## Main panel for displaying different aspects about risk:
                        mainPanel(
                          
                          ## Help modal:
                          bsModal(id = "modalinputhelp", 
                                  title = "So you want to know more about the inputs", 
                                  "Here, we will provide some theoretical background information.",
                                  trigger = "inputhelp", size = "medium"),
                          
                          ## Tabset with raw data table, icon array, nf tree, confusion table, and PV graphs: 
                          tabsetPanel(type = "tabs",
                                      tabPanel("Intro",
                                               br(),
                                               "This is just a quick page for displaying rendered text based on inputs.",
                                               "Spacing doesn't work yet, but that's only formatting...",
                                               textOutput("N"),
                                               textOutput("prev"),
                                               textOutput("sens"),
                                               textOutput("spec")
                                               ),
                                      tabPanel("Stats",
                                               br(),
                                               "This page will explain, define, and compute the current value some common metrics. Here is a first example: ",
                                               br(), br(), 
                                               uiOutput("PPV")
                                               ),
                                      tabPanel("Cases", 
                                               br(),
                                               "Individual cases:", 
                                               br(), br(),
                                               conditionalPanel(condition = "input.dataselection != 1",
                                                                "Source:",
                                                                verbatimTextOutput("sourceOutput")
                                               ),
                                               DT::dataTableOutput("rawdatatable"),
                                               br()
                                               ),
                                      
                                      tabPanel("Icons", 
                                               br(), 
                                               paste0("Icon array:"), 
                                               br(), br()
                                               ),

                                      tabPanel("Tree", 
                                               br(), 
                                               paste0("Tree of natural frequencies:"), 
                                               br(), br(),  
                                               plotOutput("nftree"), 
                                               br()
                                               ),
                                      
                                      tabPanel("Table", 
                                               br(), 
                                               paste0("Aggregated cases:"), 
                                               br(), br(),  
                                               tableOutput("confusiontable"),
                                               br(),
                                               paste0("The following mosaic plot shows the cell frequencies as area sizes:"), 
                                               br(),  br(), 
                                               plotOutput("mosaicplot"),
                                               br()
                                               ),

                                      tabPanel("PV curves", 
                                               br(),
                                               paste0("Predictive values (PPV/NPV) by prevalance:"), br(), br(),
                                               plotOutput("PVs"),
                                               br(),
                                               paste0("PPV = ", data()$PPV, ", NPV = ", data()$NPV), 
                                               # print(data()$PPV),
                                               # ERROR: object of type 'closure' is not subsettable ???
                                               checkboxInput("boxPVprev", label = "Show current prevalence in plot", value = TRUE), 
                                               checkboxInput("boxPVpoints1", label = "Show current PPV/NPV in plot", value = TRUE),
                                               # br(),
                                               checkboxInput("boxPVlog", label = "Show prevalence on logarithmic scale", value = FALSE), 
                                               br() 
                                               ),
                                      
                                      tabPanel("PV cubes", 
                                               br(),
                                               paste0("Predictive values (PPV/NPV) by sensitivity and specificity:"), br(), br(),
                                               plotOutput("PVplanes"), 
                                               br(),
                                               # paste0("PPV = ", data$PPV, ", NPV = ", data$NPV), 
                                               # ERROR: object of type 'closure' is not subsettable ???  
                                               # br(),
                                               checkboxInput("boxPVpoints2", label = "Show current PPV/NPV in plots", value = TRUE), 
                                               # br(), 
                                               "Change perspective by rotating plots:", 
                                               br(),
                                               sliderInput("theta", 
                                                           "Horizontal viewing angle:",
                                                           value = -45,
                                                           min   = -90,
                                                           max   = +90), 
                                               # br(), 
                                               sliderInput("phi", 
                                                           "Vertical viewing angle:",
                                                           value = 0,
                                                           min =   0,
                                                           max =  90), 
                                               # br(), 
                                               # "Perspective effects:",
                                               # br(),
                                               # sliderInput("d", 
                                               #             "D (in-/decrease perspective effect):",
                                               #             value = 1.2,
                                               #             min = 0.1,
                                               #             max = 2), 
                                               # # br(),
                                               # sliderInput("expand", 
                                               #             "Expansion (in z-direction):",
                                               #             value = 0.9,
                                               #             min = 0.1,
                                               #             max = 2), 
                                               # br(),
                                               # "Color effects:",
                                               # br(),
                                               # sliderInput("ltheta", 
                                               #             "Ltheta (...):",
                                               #             value = 200,
                                               #             min = 0,
                                               #             max = 1000), 
                                               # # br(),
                                               # sliderInput("shade", 
                                               #             "Shade (...):",
                                               #             value = 0.10,
                                               #             min = 0,
                                               #             max = 1), 
                                               # br(),
                                               br()
                                               )

                              )
                        )
                      )
             ),
             
             #####        
             tabPanel("2: Information",
                      icon = icon("education", lib = "glyphicon")
                      
             ),
             
             #####
             tabPanel("3: About", 
                      icon = icon("home", lib = "glyphicon"),
                      includeMarkdown("about_riskyr.md")
             ),
             
             #####
             navbarMenu("Dropdown-Navigation",
                        
                        # spacer
                        "----",
                        
                        # 1st screen in dropdown navigation:
                        tabPanel("Further information",
                                 icon = icon("book", lib = "glyphicon"),
                                 "Text of tab panel", br() 
                        ),
                        
                        # spacer
                        "----",
                        
                        # 2nd screen in dropdown navigation: 
                        tabPanel("B Imprint",
                                 icon = icon("info-sign", lib = "glyphicon"),
                                 "Hier Text für Panel B", br(), br(),
                                 a("SPDS@uni.kn", href = "https://www.spds.uni-konstanz.de"), br(), br(),
                                 tags$code("This text will be displayed as computer code."), br() 
                        ),
                        
                        # spacer
                        "----"
             )
  )
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## eof. #