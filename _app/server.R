# Shiny server.R
# spds, uni.kn | 2017 12 26
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

rm(list=ls()) # clean all.

# Dependencies:
library(shiny)
library(shinyBS)
library(markdown)
library(DT)
library(diagram)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Functions for plots and tables:

## Function to make tree of natural frequencies:
make.nftree <- function(env, data) {
  
  ## (0) Get current parameters:
  name <- env$name
  N <- env$N
  prev <- env$prev
  sens <- env$sens
  spec <- env$spec
  source <- env$source
  
  ## Tree with natural frequencies: 
  names <- c(paste0("N = ", N), # Note: Using global variables (NOT population as argument)
             paste0("true:\n", data$n.true), 
             paste0("false:\n", data$n.false), 
             paste0("hits:\n", data$n.hi), 
             paste0("misses:\n", data$n.mi),
             paste0("false alarms:\n", data$n.fa), 
             paste0("correct rejections:\n", data$n.cr))
  
  M <- matrix(nrow = 7, ncol = 8, byrow = TRUE, data = 0)
  
  M[2, 1] <- "prevalence" # paste0("prevalence = ", as.character(prev)) 
  M[3, 1] <- "(N - true)"
  M[4, 2] <- "sensitivity"
  M[5, 2] <- "(true - hi)"
  M[6, 3] <- "(false - cr)"
  M[7, 3] <- "specificity"
  
  ## plot matrix M:
  pm <- plotmat(M,
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
  
  return(pm)
  
}

# make.nftree(env)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Define server logic:
shinyServer(function(input, output, session){

  ## Define common data structure:
  # Generate data structures as lists of reactive elements:
  env <- reactiveValues(env = NULL) 
  data <- reactiveValues(data = NULL)
  # population <- reactiveValues(population = NULL)
  
  # Observe inputs and generate data used in outputs:
  observeEvent({
    # input$name   # name of current environment 
    input$N      # N in population
    input$prev   # prevalence in population = p(true positive)
    input$sens   # sensitivity = p(positive decision | true positive)
    input$spec   # specificity = p(negative decision | true negative)
    # input$source # source of environment (reference)
  }, {
    
    ## (A) Environment parameters:  
    ## Set parameters of current environment:
    env$name <- "Environment name"
    env$N <- input$N
    env$prev <- input$prev
    env$sens <- input$sens
    env$spec <- input$spec
    env$source <- "Environment source"
    
    ## (B) Derive data:
    ## (1) Determine the truth:
    data$n.true <- round((env$prev * env$N), 0) # n.true cases
    data$n.false <- (env$N - data$n.true)      # n.false cases
    
    # Vector of true states:
    data$truth <- c(rep(TRUE, data$n.true), rep(FALSE, data$n.false)) 
    
    ## (2) Determine decisions:
    data$n.hi <- round((env$sens * data$n.true), 0)  # a. hits
    data$n.mi <- (data$n.true - data$n.hi)          # b. misses
    data$n.cr <- round((env$spec * data$n.false), 0) # d. correct rejections
    data$n.fa <- (data$n.false - data$n.cr)         # c. false alarms
    
    data$dec.pos <- data$n.hi + data$n.fa # 1. positive decisions (true & false)
    data$dec.neg <- data$n.mi + data$n.cr # 2. negative decisions (true & false)
    
    # Vector of decisions (ordered by truth):
    data$decision <- c(rep(TRUE, data$n.hi), rep(FALSE, data$n.mi), 
                          rep(TRUE, data$n.fa), rep(FALSE, data$n.cr))
    
    ## (3) SDT (status decision/truth):
    data$sdt <- c(rep("hi", data$n.hi), rep("mi", data$n.mi), 
                     rep("fa", data$n.fa), rep("cr", data$n.cr))
    
    ## (4) Coerce vectors into ordered factors:
    data$truth <- factor(data$truth, 
                            levels = c(TRUE, FALSE),
                            labels = c("condition true", "condition false"), # explicit labels
                            ordered = TRUE)
    
    data$decision <- factor(data$decision, 
                            levels = c(TRUE, FALSE),
                            labels = c("positive decision", "negative decision"), # explicit labels
                            ordered = TRUE)
    
    data$sdt <- factor(data$sdt, 
                          levels = c("hi", "mi", "fa", "cr"),
                          labels = c("hit", "miss", "false alarm", "correct rejection"), # explicit labels
                          # labels = c("hi", "mi", "fa", "cr"), # implicit labels
                          ordered = TRUE)
    
    ## (5) Combine vectors of length N in population data frame:
    data$population <- data.frame(tru = data$truth,
                                  dec = data$decision,
                                  sdt = data$sdt)
    names(data$population) <- c("truth", "decision", "SDT")
    
  })
  
  ## Outputs:
    
    # (a) Raw data table: 
    output$rawdatatable <- DT::renderDataTable(DT::datatable({data$population}))
    
    # (b) 2x2 confusion table (ordered by rows/decisions):
    output$confusiontable <- renderTable({matrix(data = c(data$n.hi, data$n.fa, data$dec.pos, 
                                                          data$n.mi, data$n.cr, data$dec.neg, 
                                                          data$n.true, data$n.false, env$N),
                                                 nrow = 3, byrow = TRUE,
                                                 dimnames = list(c("Positive decision:", 
                                                                   "Negative decision:", 
                                                                   "Truth sums:"), 
                                                                 c("Condition true:", 
                                                                   "Condition false:", 
                                                                   "Decision sums:"))
                                                 )
      },  
                                         bordered = TRUE,  
                                         hover = TRUE,  
                                         align = 'r',  
                                         digits = 0, 
                                         rownames = TRUE,
                                         na = 'missing')  
    
    # (c) Mosaic plot:
    output$mosaicplot <- renderPlot(mosaicplot(table(data$population$tru,
                                                     data$population$dec),
                                                xlab = "Truth",
                                                ylab = "Decisions",
                                                main = paste0("Mosaicplot (N = ", env$N, ")")
                                               )
                                    )
    
    # (d) Tree of natural frequencies:
    output$nftree <- renderPlot(make.nftree(env, data))
    
    # (e) Icon array:
    
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# eof. #