# Shiny server.R
# spds, uni.kn | 2017 12 25
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# Dependencies:
library(shiny)
library(shinyBS)
library(DT)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Initial environment:

e1 <- list("name" = "Demo",  # name (e.g., HIV, mammography, ...)
           "N" = 100,        # N in population
           "prev" = .15,     # prevalence in population = p(true positive)
           "sens" = .85,     # sensitivity = p(positive decision | true positive)
           "spec" = .75,     # specificity = p(negative decision | true negative)
           "source" = "Source info" # information source (e.g., citation)
           )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## (0) Get current parameters:
cur.env <- e1 # from current environment

name <- cur.env$name
N <- cur.env$N
prev <- cur.env$prev
sens <- cur.env$sens
spec <- cur.env$spec
source <- cur.env$source

## (1) Determine the truth:
n.true <- round((prev * N), 0)
n.false <- (N - n.true)

truth <- c(rep(TRUE, n.true), rep(FALSE, n.false))
# truth
# sum(truth)

## (2) Determine decisions:
n.hi <- round((sens * n.true), 0)  # hits
# n.hi 
n.cr <- round((spec * n.false), 0) # correct rejections
# n.cr
n.mi <- (n.true - n.hi)            # misses
# n.mi
n.fa <- (n.false - n.cr)           # false alarms
# n.fa

decision <- c(rep(TRUE, n.hi), rep(FALSE, n.mi), rep(TRUE, n.fa), rep(FALSE, n.cr))
# decision
# sum(decision)

## (3) Population as a data frame:
population <- data.frame(tru = truth,
                         dec = decision,
                         sdt = NA)
names(population) <- c("truth", "decision", "sdt")

population$sdt[population$tru & population$dec]   <- "hi"
population$sdt[population$tru & !population$dec]  <- "mi"
population$sdt[!population$tru & population$dec]  <- "fa"
population$sdt[!population$tru & !population$dec] <- "cr"

# head(population)
# dim(population)

## (2) Make sdt (status decision-truth) an ordered factor:
population$sdt <- factor(population$sdt, 
                         levels = c("hi", "mi", "fa", "cr"),
                         # labels = c("hit", "miss", "false alarm", "correct rejection"), # explicit labels
                         labels = c("hi", "mi", "fa", "cr"), # implicit labels
                         ordered = TRUE)
# is.factor(population$sdt)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Functions for plots and tables:

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
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
      population
    }))
    
    # (b) 2x2 confusion table:
    output$confusiontable <- renderTable({ matrix(data = c(n.hi, n.fa, n.mi, n.cr), 
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# eof. #