# Shiny server.R
# spds, uni.kn | 2017 12 24

# Dependencies:
library(shiny)
library(shinyBS)

# Initial environment:
e1 <- list("name" = "demo", # name (e.g., HIV, mammography, ...)
           "N" = 100,       # N in population
           "prev" = .15,    # prevalence in population = p(true positive)
           "sens" = .85,    # sensitivity = p(positive decision | true positive)
           "spec" = .75     # specificity = p(negative decision | true negative)
           )

# Functions for plots and tables:
# ...

# Define server logic:
shinyServer(function(input, output, session){
    
    ## Inputs used in all representations:
    # N <- input$N       # N in population
    # prev <- input$prev # prevalence in population = p(true positive)
    # sens <- input$sens # sensitivity = p(positive decision | true positive)
    # spec <- input$spec # specificity = p(negative decision | true negative)
    
    ## Define common data structure:
    # Update current population (as df)
    
    ## Outputs:
    
    # (a) raw data table: 
    output$rawdatatable <- DT::renderDataTable(DT::datatable({
      cars
    }))
    
    # (b) 2x2 confusion table:
    output$confusiontable <- renderTable({ matrix(data = c(25, 130, 2500, 240.892), 
                                                  nrow = 2, byrow = TRUE,
                                                  dimnames = list(c("dec pos", "dec neg"), c("true", "false"))) },  
                                         bordered = TRUE,  
                                         hover = TRUE,  
                                         align = 'c',  
                                         digits = 2, 
                                         rownames = TRUE,
                                         na = 'missing')  
    
    # (c) mosaic plot:
    output$mosaicplot <- renderPlot(mosaicplot(table(1:5, 5:1)))
    
    # (d) tree of natural frequencies:
    
    # (e) icon array:
    
  }
)

# eof. #