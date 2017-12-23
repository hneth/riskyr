
# Define server logic
library(shiny)
library(shinyBS)


shinyServer(
    function(input, output, session) {
        
        
        ######
        # Underlying data structure

       

        
        
        #####
        # Outputs
        
        # outputs for tab on raw data table
        output$rawdatatable <- DT::renderDataTable(DT::datatable({
            cars
        }))
        
        
        # outputs for tab on confusion table
        output$confusiontable <- renderTable({ matrix(data = c(25, 130, 2500, 240.892), 
                                                      nrow = 2, byrow = TRUE,
                                                      dimnames = list(c("A", "B"), c("C", "D"))) },  
                                             bordered = TRUE,  
                                             hover = TRUE,  
                                             align = 'c',  
                                             digits = 2, 
                                             rownames = TRUE,
                                             na = 'missing')  
        
        output$mosaicplot <- renderPlot(mosaicplot(table(1:5, 5:1)))

        
    }
)
