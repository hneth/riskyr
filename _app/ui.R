# Dependencies:
library(markdown)
library(shinyBS)
library(DT)

# Define user interface logic:
shinyUI(
  
  navbarPage(title = "riskyR",
             theme = "bootstrap.sandstone.css", # or any other bootsstrap theme https://bootswatch.com/3/
             
             #####
             tabPanel("1: About", 
                      icon = icon("home", lib = "glyphicon"),
                      includeMarkdown("justamarkdownfile.md")
             ),
             
             #####               
             tabPanel("2: Representations",
                      icon = icon("tree-deciduous", lib = "glyphicon"),
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs:
                        sidebarPanel(
                          
                          # Input: Select all input values:
                          sliderInput("N",
                                      "Population size:",
                                      value = 1000,
                                      min = 1,
                                      max = 1000000),
                          
                          br(), # horizontal space
                          
                          sliderInput("prev", 
                                      "Prevalence:",
                                      value = 0.15,
                                      min = 0,
                                      max = 1),
                          sliderInput("spec", 
                                      "Specifity:",
                                      value = 0.70,
                                      min = 0,
                                      max = 1),
                          sliderInput("sens", 
                                      "Sensitivity:",
                                      value = 0.85,
                                      min = 0,
                                      max = 1),
                          
                          br(), # horizontal space
                          
                          bsButton("inputhelp", label = "Help", 
                                   icon = icon("question-sign", lib = "glyphicon"),
                                   style = "default", type = "action"),
                          
                          # Tooltips on inputs:
                          bsTooltip(id = "N", title = "Size of population",
                                    placement = "right", trigger = "hover", options = list(container = "body")), 
                          bsTooltip(id = "prev", title = "Probability of being affected",
                                    placement = "right", trigger = "hover", options = list(container = "body")),
                          bsTooltip(id = "sens", title = "Probability of correctly detecting an affected individual",
                                    placement = "right", trigger = "hover", options = list(container = "body")), 
                          bsTooltip(id = "spec", title = "Probability of correctly rejecting an unaffected individual",
                                    placement = "right", trigger = "hover", options = list(container = "body"))
                        ),
                        
                        ## Main panel for displaying different aspects about risk:
                        mainPanel(
                          
                          # help modal
                          bsModal(id = "modalinputhelp", 
                                  title = "So you want to know more about the inputs", 
                                  "Here, we could give some theoretical background...",
                                  trigger = "inputhelp", size = "large"),
                          
                          # Tabset w/ raw data, trees, and table,...
                          tabsetPanel(type = "tabs",
                                      tabPanel("Raw data", br(), DT::dataTableOutput("rawdatatable")),
                                      tabPanel("Tree", br(), plotOutput("tree")),
                                      tabPanel("2 x 2 table", br(), tableOutput("confusiontable"), plotOutput("mosaicplot")),
                                      tabPanel("...", br(), "Here be some text")
                          )
                        )
                      )
             ),
             
             #####        
             tabPanel("3: Information",
                      icon = icon("education", lib = "glyphicon")
                      
             ),
             
             #####
             navbarMenu("Dropdown-Navigation",
                        
                        # spacer
                        "----",
                        
                        # first screen in dropdown navigation
                        tabPanel("A Further readings",
                                 icon = icon("book", lib = "glyphicon"),
                                 "Hier Text in Panel A"
                        ),
                        
                        # spacer
                        "----",
                        
                        # second screen in dropdown navigation
                        tabPanel("B Imprint",
                                 icon = icon("info-sign", lib = "glyphicon"),
                                 "Hier Text für Panel B", br(),
                                 a("uni.kn", href = "http://www.uni-konstanz.de"), br(),
                                 tags$code("This text will be displayed as computer code.")
                        ),
                        
                        # spacer
                        "----"
             )
  )
)

# eof.