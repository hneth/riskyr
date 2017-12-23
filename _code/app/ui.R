# dependencies
library(markdown)
library(shinyBS)
library(DT)




# Define user interface logic

shinyUI(
    
    navbarPage(title = "riskyR",
               theme = "bootstrap.sandstone.css", # or any other bootsstrap theme https://bootswatch.com/3/
               
               #####
               tabPanel("Tab 1 About", 
                        icon = icon("home", lib = "glyphicon"),
                        includeMarkdown("justamarkdownfile.md")
                         ),
               
               #####               
               tabPanel("Tab 2 Here comes the interesting Stuff",
                        icon = icon("heart", lib = "glyphicon"),
                        

                        sidebarLayout(
                            
                            #####               
                            # Sidebar panel for inputs
                            sidebarPanel(
                                
                                # Input: Select the input values
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
                                
                                # br() element to introduce extra vertical spacing
                                br(),
                                
                                sliderInput("N",
                                            "Population size:",
                                            value = 100,
                                            min = 4,
                                            max = 100000),
                                br(),
                                bsButton("inputhelp", label = "Help", 
                                         icon = icon("question-sign", lib = "glyphicon"),
                                         style = "default", type = "action"),
                                
                                # Tooltips on inputs
                                bsTooltip(id = "prev", title = "This tooltip could explain the concept of prevalence.",
                                          placement = "right", trigger = "hover", options = list(container = "body")),
                                bsTooltip(id = "spec", title = "What is this?",
                                          placement = "right", trigger = "hover", options = list(container = "body")),
                                bsTooltip(id = "sens", title = "... and that?",
                                          placement = "right", trigger = "hover", options = list(container = "body")),
                                bsTooltip(id = "N", title = "$78910298WTF?!#",
                                          placement = "right", trigger = "hover", options = list(container = "body"))
                                
                                
                            ),
                            
                            #####               
                            # Main panel for displaying different aspects about risk
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
               tabPanel("Tab 3 Maybe a quiz here",
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