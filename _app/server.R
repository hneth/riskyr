# Shiny server.R
# spds, uni.kn | 2017 12 29
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# rm(list=ls()) # clean all.
# setwd("~/Desktop/stuff/Dropbox/GitHub/riskyr/_app/") # set to current directory

# Dependencies:
library(shiny)
library(shinyBS)
library(markdown)
library(DT)
library(diagram)
library(shape)
library(tidyr)
library(dplyr)
library(ggplot2)

# Import ready-made and worked out example data:
datasets <- read.csv("./www/riskyR_datasets.csv", stringsAsFactors = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Graphic settings: 
{
  ## Color names:
  ## Colors from https://bootswatch.com/sandstone/ 
  col.sand.light <- rgb(248, 245, 240, max = 255)
  col.sand.mid  <- rgb(142, 140, 132, max = 255)  
  col.sand.dark <- rgb(62, 63, 58, max = 255)
  
  col.green.1 <- rgb(184, 217, 137, max = 255)
  col.green.2 <- rgb(128, 177, 57, max = 255)
  
  col.red.1 <- rgb(230, 142, 140, max = 255)
  col.red.2 <- rgb(210, 52, 48, max = 255)
  
  col.blue.1 <- rgb(115, 200, 234, max = 255)
  col.blue.2 <- rgb(121, 149, 177, max = 255)
  col.blue.3 <- rgb(29, 149, 198, max = 255)
  col.blue.4 <- rgb(40, 74, 108, max = 255)

  col.orange.1 <- rgb(247, 169, 127, max = 255)
  col.orange.2 <- rgb(242, 100, 24, max = 255)
  
  col.grey.1 <- rgb(181, 179, 174, max = 255)
  col.grey.2 <- rgb(123, 121, 113, max = 255)

  col.ppv <- col.orange.2 # "orange3" # "firebrick" "red3"
  col.npv <- col.blue.3 # "steelblue3" # "green4" "gray50" "brown4" "chartreuse4"  
  
  ## ggplot themes:
  my.theme <-  theme_bw() +
    theme(plot.title = element_text(face = "bold", size = "12", color = "black", hjust = 0.0),
          axis.title = element_text(face = "plain", size = 11, color = "black"),
          axis.text = element_text(face = "plain", size = 10, color = "gray30"),
          # axis.line = element_line(size = 0.75, color = "black", linetype = 1), 
          axis.ticks = element_line(size = 0.75, color = "gray10", linetype = 1), 
          # panel.background = element_rect(fill = "gray95", color = "gray20"),
          panel.grid.major.x = element_line(color = "gray66", linetype = 1, size = .2),
          panel.grid.major.y = element_line(color = "gray33", linetype = 1, size = .2),
          # panel.grid.minor.x = element_blank(), 
          # panel.grid.minor.y = element_blank(),
          legend.position = "none"
    )
  
  my.theme.legend <- theme_bw() +
    theme(plot.title = element_text(face = "bold", size = "12", color = col.sand.dark, hjust = 0.0),
          axis.title = element_text(face = "plain", size = 11, color = col.sand.dark),
          axis.text = element_text(face = "plain", size = 10, color = col.sand.dark),
          # axis.line = element_line(size = 0.75, color = "black", linetype = 1), 
          axis.ticks = element_line(size = 0.75, color = col.sand.mid, linetype = 1),   
          # panel.background = element_rect(fill = "gray95", color = "gray20"),
          panel.grid.major.x = element_line(color = col.sand.light, linetype = 1, size = .2),
          panel.grid.major.y = element_line(color = col.sand.light, linetype = 1, size = .2)#,
          # panel.grid.minor.x = element_blank(), 
          # panel.grid.minor.y = element_blank()#,
          # legend.position = "none"
    )
}

# Utility functions:
{
  pc <- function(dec) {
    return(round(dec * 100, 1))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Functions for plots and tables:

## Function to draw tree of natural frequencies:
plot.nftree <- function(env, data) {
  
  ## Current environment parameters:
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
                box.col = col.sand.light, # "lightyellow" 
                shadow.col = col.sand.dark, # "steelblue4", "grey25"
                shadow.size = 0, # .005 
                lwd = 1.2,
                cex.txt = .90,
                main = paste0(name, ":\nTree of natural frequencies\n", "(", source, ")")
                )
  
  return(pm)
  
}

## Functions for PPV/NPV:
# (1) Compute PPV and NPV as a function of prev, sens, and spec:
#     using Bayes' formula:
get.PPV <- function(prev, sens, spec) {
  PPV <- NA # initialize
  num <- (prev * sens)
  den1 <- num
  den2 <- (1 - prev) * (1 - spec)
  PPV <- num / (den1 + den2)
  return(PPV)
}

get.NPV <- function(prev, sens, spec) {
  NPV <- NA # initialize
  num.n <- (1 - prev) * spec
  den1.n <- num.n
  den2.n <- (prev) * (1 - sens)
  NPV <- num.n / (den1.n + den2.n)
  return(NPV)
}

# Specify a vector of prevalences:
{
  step.0 <- .10
  prev.0 <- seq(0, 10 * step.0, by = step.0)
  step.1 <- .001
  prev.1 <- seq(step.1, 10 * step.1, by = step.1)
  step.2 <- .01
  prev.2 <- seq(step.2, 10 * step.2, by = step.2)
  step.3 <- .05
  prev.3 <- seq(step.3, 20 * step.3, by = step.3)
  step.4 <- .01
  prev.4 <- seq(.90, .90 + 10 * step.4, by = step.4)
  step.5 <- .001
  prev.5 <- seq(.990, .990 + 10 * step.5, by = step.5)
  
  prev.range <- sort(unique(c(prev.0, prev.1, prev.2, prev.3, prev.4, prev.5)))
  # prev.range <- prev.range[prev.range > 0] # remove prev = 0
  # prev.range <- prev.range[prev.range < 1] # remove prev = 1
  ## Hack to prevent -Inf on log scale:
  epsilon <- 1/1000000 # some very small constant
  prev.range[prev.range == 0] <- 0 + epsilon # slightly more than 0
  prev.range[prev.range == 1] <- 1 - epsilon # slightly less than 1 
  # prev.range
  # log10(get.PPV(prev.range, .5, .5)) # => -Inf for prev = 0
  # log10(get.NPV(prev.range, .5, .5)) # => -Inf for prev = 1 
  
  prev.scale <- sort(unique(c(step.0, 5*step.0, step.1, 5*step.1, step.2, 5*step.2, 9*step.0)))
  # log10(prev.scale)
  # prev.scale
}

plot.PVs <- function(env, log.scale = FALSE) {
  
  ## Current environment parameters:
  name <- env$name
  N <- env$N
  prev <- env$prev
  sens <- env$sens
  spec <- env$spec
  source <- env$source
  
  ## Hack to prevent -Inf on log scale: 
  if (log.scale) {
    if (prev == 0) { prev <- 0 + epsilon } # slightly more than 0
    if (prev == 1) { prev <- 1 - epsilon } # slightly less than 1
  }
  
  ## Compute current PPV and NPV values for prev.range:
  PPV <- get.PPV(prev.range, sens, spec)
  NPV <- get.NPV(prev.range, sens, spec)
  
  ## As data frame df:
  df.PVs <- data.frame(prev.range, PPV, NPV)
  
  ## Reshape into long format:
  df.PVs.long <- df.PVs %>% gather(metric, value, c(PPV, NPV))
  
  df.PVs.long$metric <- factor(df.PVs.long$metric, # ensure factor and level order:  
                               levels = c("PPV", "NPV") #,
                               # labels = c("PPV = p(true | positive decision)", 
                               #            "NPV = p(false | negative decision)")
  )
  
  ## Additional plot options:
  cur.PPV <- get.PPV(prev, sens, spec)
  cur.NPV <- get.NPV(prev, sens, spec) 
  cur.PPV.label <- paste0("PPV = ", pc(cur.PPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.PPV), "%)")
  cur.NPV.label <- paste0("NPV = ", pc(cur.NPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.NPV), "%)")
  sens.spec.label <- paste0("(sens = ", pc(sens), "%, spec = ", pc(spec), "%)") # label
  
  if (!log.scale) { ## plot on linear scale: 
    p.PVs <- ggplot(data = df.PVs.long, aes(x = prev.range, y = value, group = metric)) +
      geom_line(aes(color = metric), size = 1.2) +
      geom_point(aes(color = metric, shape = metric), size = 2) +
      geom_line(aes(x = prev), color = "grey25", linetype = 3, size = .6) + # vertical line at prev
      ## Mark and label current PPV/NPV:
      geom_point(aes(x = prev, y = cur.PPV), 
                 color = col.ppv, shape = 21, size = 5) + # mark PPV
      geom_text(aes(x = prev, y = cur.PPV, label = cur.PPV.label), 
                color = col.ppv, hjust = -.15, vjust = +.50, size = 4) + # label PPV
      geom_point(aes(x = prev, y = cur.NPV), 
                 color = col.npv, shape = 21, size = 5) + # mark NPV
      geom_text(aes(x = prev, y = cur.NPV, label = cur.NPV.label), 
                color = col.npv, hjust = -.15, vjust = +.50, size = 4) + # label NPV
      ## Scales:
      ## (a) linear scale:
      scale_x_continuous(breaks = seq(0, 1, by = .10)) + 
      labs(title = paste0(name, ":\nPPV and NPV by prevalence ", sens.spec.label, "\n(", source, ")"),
           x = "Prevalence (linear scale)", y = "Probability") + 
      ## (b) log scale:
      # scale_x_log10(breaks = prev.scale) + 
      # labs(title = paste0(name, ":\nPPV and NPV by prevalence ", sens.spec, "\n(", source, ")"),
      #                    x = "Prevalence (logarithmic scale)", y = "Probability") + 
      ## Colors: 
      scale_color_manual(values = c(col.ppv, col.npv)) +
      # scale_fill_manual(values = c(col.ppv, col.npv), name = "Metric:") +
      my.theme.legend
  }
  
  if (log.scale) { ## plot on log scale: 
    p.PVs <- ggplot(data = df.PVs.long, aes(x = prev.range, y = value, group = metric)) +
      geom_line(aes(color = metric), size = 1.2) +
      geom_point(aes(color = metric, shape = metric), size = 2) +
      geom_line(aes(x = prev), color = "grey25", linetype = 3, size = .6) + # vertical line at prev
      ## Mark and label current PPV/NPV:
      geom_point(aes(x = prev, y = cur.PPV), 
                 color = col.ppv, shape = 21, size = 5) + # mark PPV
      geom_text(aes(x = prev, y = cur.PPV, label = cur.PPV.label), 
                color = col.ppv, hjust = -.15, vjust = +.50, size = 4) + # label PPV
      geom_point(aes(x = prev, y = cur.NPV), 
                 color = col.npv, shape = 21, size = 5) + # mark NPV
      geom_text(aes(x = prev, y = cur.NPV, label = cur.NPV.label), 
                color = col.npv, hjust = -.15, vjust = +.50, size = 4) + # label NPV
      ## Scales:
      ## (a) linear scale:
      # scale_x_continuous(breaks = seq(0, 1, by = .10)) + 
      # labs(title = paste0(name, ":\nPPV and NPV by prevalence ", sens.spec, "\n(", source, ")"),
      #      x = "Prevalence (linear scale)", y = "Probability") + 
      ## (b) log scale:
      scale_x_log10(breaks = prev.scale) + 
      labs(title = paste0(name, ":\nPPV and NPV by prevalence ", sens.spec.label, "\n(", source, ")"),
           x = "Prevalence (logarithmic scale)", y = "Probability") + 
      ## Colors: 
      scale_color_manual(values = c(col.ppv, col.npv)) +
      # scale_fill_manual(values = c(col.ppv, col.npv), name = "Metric:") +
      my.theme.legend
  }
  
  return(p.PVs)
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# Define server logic:
shinyServer(function(input, output, session){

  ## Define common data structure:
  # Generate data structures as lists of reactive elements:
  env <- reactiveValues(env = NULL) # list of current environment
  data <- reactiveValues(data = NULL) # list of derived parameters
  # population <- reactiveValues(population = NULL) # df of current population
  
  # Observe inputs and generate data used in outputs:
  observeEvent({
    input$name   # name of current environment 
    input$N      # N in population
    input$prev   # prevalence in population = p(true positive)
    input$sens   # sensitivity = p(positive decision | true positive)
    input$spec   # specificity = p(negative decision | true negative)
    # input$source # source of environment (reference)
  }, {
    
    ## (A) Environment parameters:  
    ## Set parameters of current environment:
    env$name <- input$name
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
    data$n.mi <- (data$n.true - data$n.hi)           # b. misses
    data$n.cr <- round((env$spec * data$n.false), 0) # d. correct rejections
    data$n.fa <- (data$n.false - data$n.cr)          # c. false alarms
    
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
    names(data$population) <- c("Truth", "Decision", "sdt")

  })
  
  ## Integrate worked out examples:
  observeEvent(input$dataselection, {
    if (input$dataselection != 1) { # if 1st option is not ("---")
      updateSliderInput(session, "N", value = datasets[input$dataselection, "N" ]) 
      updateSliderInput(session, "sens", value = datasets[input$dataselection, "sens" ])
      updateSliderInput(session, "prev", value = datasets[input$dataselection, "prev" ])
      updateSliderInput(session, "spec", value = datasets[input$dataselection, "spec" ]) 
      output$sourceOutput <- renderText(datasets[input$dataselection, "source"]) }
  }, ignoreInit = TRUE)
  
  ## PVplot panel: Checkbox for linear vs. logarithmic scale: 
  # observeEvent(input$checkboxPVlog, {
  #   PV.log <- FALSE # initialize
  #  if (!input$checkboxPVlog) {PV.log <- FALSE} # ERROR:
  #  if (input$checkboxPVlog)  {PV.log <- TRUE}  # Does NOT seem to work!
  #  }
  #  )
  
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
  output$mosaicplot <- renderPlot(mosaicplot(table(data$population$Truth,
                                                   data$population$Decision),
                                             col = c(col.sand.mid, col.sand.dark), # c(col.blue.2, col.blue.4), 
                                             xlab = "Truth",
                                             ylab = "Decision",
                                             main = paste0(env$name, "\n(N = ", env$N, ")")
                                             )
                                  )
  
  # (d) Tree of natural frequencies:
  output$nftree <- renderPlot(plot.nftree(env, data))
  
  # (e) Icon array:

  # (f) PPV and NPV as a function of prev.range:
  output$PVs <- renderPlot(plot.PVs(env, log.scale = input$checkboxPVlog))
  
}
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# eof. #