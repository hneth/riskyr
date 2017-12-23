# Define server logic:
library(shiny)
library(shinyBS)

shinyServer(
  function(input, output, session) {
    
    ## Inputs used in all representations:
    # N <- input$N       # N in population
    # prev <- input$prev # prevalence in population = p(true positive)
    # sens <- input$sens # sensitivity = p(positive decision | true positive)
    # spec <- input$spec # specificity = p(negative decision | true negative)
    
    ## Define common data structure:
    # population as df
    
    ## Outputs:
    
    # outputs for tab on raw data table: 
    output$rawdatatable <- DT::renderDataTable(DT::datatable({
      cars
    }))
    
    # outputs for confusion table:
    output$confusiontable <- renderTable({ matrix(data = c(25, 130, 2500, 240.892), 
                                                  nrow = 2, byrow = TRUE,
                                                  dimnames = list(c("A", "B"), c("C", "D"))) },  
                                         bordered = TRUE,  
                                         hover = TRUE,  
                                         align = 'c',  
                                         digits = 2, 
                                         rownames = TRUE,
                                         na = 'missing')  
    
    # outputs for mosaic plot:
    output$mosaicplot <- renderPlot(mosaicplot(table(1:5, 5:1)))
    
    # tree of natural frequencies:
    # icon array:
  }
)
