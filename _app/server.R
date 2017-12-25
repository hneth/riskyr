# Shiny server.R
# spds, uni.kn | 2017 12 25
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# Dependencies:
library(shiny)
library(shinyBS)
library(DT)
library(diagram)

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

init.population <- function(env = cur.env) {

  ## (0) Get current parameters:
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

  ## (2) Determine decisions:        # Number of 
  n.hi <- round((sens * n.true), 0)  # a. hits
  n.cr <- round((spec * n.false), 0) # b. correct rejections
  n.mi <- (n.true - n.hi)            # c. misses
  n.fa <- (n.false - n.cr)           # d. false alarms
  decision <- c(rep(TRUE, n.hi), rep(FALSE, n.mi), rep(TRUE, n.fa), rep(FALSE, n.cr))
  
  ## (3) Population:
  ## (a) Initialize new data frame:
  population <- data.frame(tru = truth,
                           dec = decision,
                           sdt = NA)
  names(population) <- c("truth", "decision", "sdt")
  
  ## (b) Classify sdt by combination of truth and decision:
  population$sdt[population$tru & population$dec]   <- "hi"
  population$sdt[population$tru & !population$dec]  <- "mi"
  population$sdt[!population$tru & population$dec]  <- "fa"
  population$sdt[!population$tru & !population$dec] <- "cr"
  
  ## (c) Make sdt (status decision-truth) an ordered factor:
  population$sdt <- factor(population$sdt, 
                           levels = c("hi", "mi", "fa", "cr"),
                           # labels = c("hit", "miss", "false alarm", "correct rejection"), # explicit labels
                           labels = c("hi", "mi", "fa", "cr"), # implicit labels
                           ordered = TRUE)
  
  return(population)
}

population <- init.population(cur.env)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Functions for plots and tables:

## Function to make tree of natural frequencies:
make.nftree <- function(env = cur.env) {
  
  ## (0) Get current parameters:
  name <- cur.env$name
  N <- cur.env$N
  prev <- cur.env$prev
  sens <- cur.env$sens
  spec <- cur.env$spec
  source <- cur.env$source
  
  ## Tree with natural frequencies: 
  names <- c(paste0("N = ", N), # Note: Using global variables (NOT population as argument)
             paste0("true:\n", n.true), 
             paste0("false:\n", n.false), 
             paste0("hits:\n", n.hi), 
             paste0("misses:\n", n.mi),
             paste0("false alarms:\n", n.fa), 
             paste0("correct rejections:\n", n.cr))
  
  M <- matrix(nrow = 7, ncol = 8, byrow = TRUE, data = 0)
  
  M[2, 1] <- "prevalence" # paste0("prevalence = ", as.character(prev)) 
  M[3, 1] <- "(N - true)"
  M[4, 2] <- "sensitivity"
  M[5, 2] <- "(true - hi)"
  M[6, 3] <- "(false - cr)"
  M[7, 3] <- "specificity"
  
  ## plot matrix M:
  pp <- plotmat(M,
                pos = c(1, 2, 4), 
                curve = 0.0,
                name = names,
                box.lwd = 1.5, # radx = 0.1, # rady = 0.05, 
                box.size = .10, 
                box.prop = 0.5,
                box.type = "square", # "circle",
                box.col = "lightyellow", # ... 
                shadow.col = "steelblue4", # "grey25" 
                shadow.size = 0.0, # .005 
                lwd = 1.2,
                cex.txt = .90,
                main = paste0(name, ":\nTree of natural frequencies\n", "(", source, ")")
                )
  
  return(pp)
  
}

# make.nftree(cur.env)

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
    
    # (a) Raw data table: 
    output$rawdatatable <- DT::renderDataTable(DT::datatable({population}))
    
    # (b) 2x2 confusion table:
    output$confusiontable <- renderTable({matrix(data = c(n.hi, n.fa, dec.pos, 
                                                          n.mi, n.cr, dec.neg, 
                                                          n.true, n.false, N), 
                                                 nrow = 3, byrow = TRUE,
                                                 dimnames = list(c("dec pos", "dec neg", "dec sum"), c("true", "false", "sum"))) },  
                                         bordered = TRUE,  
                                         hover = TRUE,  
                                         align = 'r',  
                                         digits = 0, 
                                         rownames = TRUE,
                                         na = 'missing')  
    
    # (c) Mosaic plot:
    output$mosaicplot <- renderPlot(mosaicplot(table(1:5, 5:1)))
    
    # (d) Tree of natural frequencies:
    # output$nftree <- make.nftree(cur.env)
    
    # (e) Icon array:
    
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# eof. #