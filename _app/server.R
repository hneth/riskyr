# Shiny server.R
# spds, uni.kn | 2017 12 26
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# Dependencies:
library(shiny)
library(shinyBS)
library(markdown)
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

# init.population <- function(env = cur.env) {
# 
#   ## (0) Get current parameters:
#   name <- cur.env$name
#   N <- cur.env$N
#   prev <- cur.env$prev
#   sens <- cur.env$sens
#   spec <- cur.env$spec
#   source <- cur.env$source
# 
#   ## (1) Determine the truth:
#   n.true <- round((prev * N), 0)
#   n.false <- (N - n.true)
#   truth <- c(rep(TRUE, n.true), rep(FALSE, n.false))
# 
#   ## (2) Determine decisions:        # Number of 
#   n.hi <- round((sens * n.true), 0)  # a. hits
#   n.mi <- (n.true - n.hi)            # b. misses
#   n.cr <- round((spec * n.false), 0) # d. correct rejections
#   n.fa <- (n.false - n.cr)           # c. false alarms
#   decision <- c(rep(TRUE, n.hi), rep(FALSE, n.mi), rep(TRUE, n.fa), rep(FALSE, n.cr))
#   
#   ## (3) Population:
#   ## (a) Initialize new data frame:
#   population <- data.frame(tru = truth,
#                            dec = decision,
#                            sdt = NA)
#   names(population) <- c("truth", "decision", "sdt")
#   
#   ## (b) Classify sdt by combination of truth and decision:
#   population$sdt[population$tru & population$dec]   <- "hi"
#   population$sdt[population$tru & !population$dec]  <- "mi"
#   population$sdt[!population$tru & population$dec]  <- "fa"
#   population$sdt[!population$tru & !population$dec] <- "cr"
#   
#   ## (c) Make sdt (status decision-truth) an ordered factor:
#   population$sdt <- factor(population$sdt, 
#                            levels = c("hi", "mi", "fa", "cr"),
#                            # labels = c("hit", "miss", "false alarm", "correct rejection"), # explicit labels
#                            labels = c("hi", "mi", "fa", "cr"), # implicit labels
#                            ordered = TRUE)
#   
#   return(population)
# }

# population <- init.population(cur.env)

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

  ## Define common data structure:
  # Generate data structures as lists of reactive elements:
  cur.env <- reactiveValues(cur.env = NULL) 
  dat.str <- reactiveValues(dat.str = NULL) 
  
  # Observe inputs and generate data used in outputs:
  observeEvent({
    # input$name   # name of current environment 
    input$N      # N in population
    input$prev   # prevalence in population = p(true positive)
    input$sens   # sensitivity = p(positive decision | true positive)
    input$spec   # specificity = p(negative decision | true negative)
    # input$source # source of environment (reference)
  }, {
    
    ## (0) Basic parameters of current environment:
    cur.env$name <- "Environment name"
    cur.env$N <- input$N
    cur.env$prev <- input$prev
    cur.env$sens <- input$sens
    cur.env$spec <- input$spec
    cur.env$source <- "Environment source"
    
    ## (1) Determine the truth:
    dat.str$n.true <- round((input$prev * input$N), 0) # n.true cases
    dat.str$n.false <- (input$N - dat.str$n.true)      # n.false cases
    
    # Vector of true states:
    dat.str$truth <- c(rep(TRUE, dat.str$n.true), rep(FALSE, dat.str$n.false)) 
    
    ## (2) Determine decisions:
    dat.str$n.hi <- round((input$sens * dat.str$n.true), 0)  # a. hits
    dat.str$n.mi <- (dat.str$n.true - dat.str$n.hi)          # b. misses
    dat.str$n.cr <- round((input$spec * dat.str$n.false), 0) # d. correct rejections
    dat.str$n.fa <- (dat.str$n.false - dat.str$n.cr)         # c. false alarms
    
    dat.str$dec.pos <- dat.str$n.hi + dat.str$n.fa # 1. positive decisions (true & false)
    dat.str$dec.neg <- dat.str$n.mi + dat.str$n.cr # 2. negative decisions (true & false)
    
    # Vector of decisions (ordered by truth):
    dat.str$decision <- c(rep(TRUE, dat.str$n.hi), rep(FALSE, dat.str$n.mi), 
                          rep(TRUE, dat.str$n.fa), rep(FALSE, dat.str$n.cr))
    
    ## (3) SDT (status decision/truth):
    dat.str$sdt <- c(rep("hi", dat.str$n.hi), rep("mi", dat.str$n.mi), 
                     rep("fa", dat.str$n.fa), rep("cr", dat.str$n.cr))
    
    ## (4) Coerce vectors into ordered factors:
    dat.str$truth <- factor(dat.str$truth, 
                            levels = c(TRUE, FALSE),
                            labels = c("condition true", "condition false"), # explicit labels
                            ordered = TRUE)
    
    dat.str$decision <- factor(dat.str$decision, 
                            levels = c(TRUE, FALSE),
                            labels = c("positive decision", "negative decision"), # explicit labels
                            ordered = TRUE)
    
    dat.str$sdt <- factor(dat.str$sdt, 
                          levels = c("hi", "mi", "fa", "cr"),
                          labels = c("hit", "miss", "false alarm", "correct rejection"), # explicit labels
                          # labels = c("hi", "mi", "fa", "cr"), # implicit labels
                          ordered = TRUE)
    
    ## (5) Combine vectors of length N in population data frame:
    dat.str$population <- data.frame(tru = dat.str$truth,
                                     dec = dat.str$decision,
                                     sdt = dat.str$sdt)
  })
  
  ## Outputs:
    
    # (a) Raw data table: 
    output$rawdatatable <- DT::renderDataTable(DT::datatable({dat.str$population}))
    
    # (b) 2x2 confusion table (ordered by rows/decisions):
    output$confusiontable <- renderTable({matrix(data = c(dat.str$n.hi, dat.str$n.fa, dat.str$dec.pos, 
                                                          dat.str$n.mi, dat.str$n.cr, dat.str$dec.neg, 
                                                          dat.str$n.true, dat.str$n.false, cur.env$N),
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
    output$mosaicplot <- renderPlot(mosaicplot(table(dat.str$population$tru,
                                                     dat.str$population$dec),
                                                xlab = "Truth",
                                                ylab = "Decisions",
                                                main = paste0("Mosaicplot (N = ", cur.env$N, ")")
                                               )
                                    )
    
    # (d) Tree of natural frequencies:
    output$nftree <- renderPlot(make.nftree(env = cur.env, data = dat.str))
    
    # (e) Icon array:
    
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# eof. #