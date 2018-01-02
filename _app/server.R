## server.R
## riskyr | R Shiny | spds, uni.kn | 2018 01 02
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# rm(list=ls()) # clean all.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Dependencies:

library(shiny)
library(shinyBS)
library(markdown)
library(DT)
library(diagram)
library(shape)
library(tidyr)
library(dplyr)
library(ggplot2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Paths:

# cur.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(cur.path) # set to current directory
# setwd("~/Desktop/stuff/Dropbox/GitHub/riskyr/_app/") # set to current directory

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Graphic settings: 

{
  ## Color names:
  {
    ## from uni.kn: 
    seeblau <- rgb(0, 169, 224, max = 255) # seeblau.4 (non-transparent)
    
    ## from https://bootswatch.com/sandstone/ 
    col.sand.light <- rgb(248, 245, 240, max = 255)
    col.sand.mid   <- rgb(142, 140, 132, max = 255)  
    col.sand.dark  <- rgb(62, 63, 58, max = 255)
    
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
    col.grey.3 <- "grey25"
    col.grey.4 <- "grey10"
    
    col.ppv <- col.orange.2 # "orange3" # "firebrick" "red3"
    col.npv <- col.blue.3 # seeblau "steelblue3" # "green4" "gray50" "brown4" "chartreuse4"  
  }
  
  ## ggplot themes:
  {
    my.theme <-  theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 12, color = col.grey.4, hjust = 0.0),
            axis.title = element_text(face = "plain", size = 11, color = col.sand.dark),
            axis.text = element_text(face = "plain", size = 10, color = col.sand.dark),
            # axis.line = element_line(size = 0.75, color = "black", linetype = 1), 
            axis.ticks = element_line(size = 0.75, color = col.sand.mid, linetype = 1), 
            panel.background = element_rect(fill = "grey99", color = col.sand.dark),
            panel.grid.major.x = element_line(color = col.sand.light, linetype = 1, size = .2),
            panel.grid.major.y = element_line(color = col.sand.light, linetype = 1, size = .2),
            # panel.grid.minor.x = element_blank(), 
            # panel.grid.minor.y = element_blank(),
            legend.position = "none"
      )
    
    my.theme.legend <- theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 12, color = col.grey.4, hjust = 0.0),
            axis.title = element_text(face = "plain", size = 11, color = col.sand.dark),
            axis.text = element_text(face = "plain", size = 10, color = col.sand.dark),
            # axis.line = element_line(size = 0.75, color = "black", linetype = 1), 
            axis.ticks = element_line(size = 0.75, color = col.sand.mid, linetype = 1),   
            panel.background = element_rect(fill = "grey99", color = col.sand.dark),
            panel.grid.major.x = element_line(color = col.sand.light, linetype = 1, size = .2),
            panel.grid.major.y = element_line(color = col.sand.light, linetype = 1, size = .2)#,
            # panel.grid.minor.x = element_blank(), 
            # panel.grid.minor.y = element_blank()#,
            # legend.position = "none"
      )
  }
  
}

## Utility functions:
{
  
  ## Percentage rounded to 1 decimal digit: 
  pc <- function(num) { 
    return(round(num * 100, 1)) 
  }
  
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Functions for PPV/NPV:
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## 2D graph:

## (1) Compute PPV and NPV as a function of prev, sens, and spec:
##     using Bayes' formula:
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

## (2) Specify a range of prevalences
##     (with finer steps at both extremes):
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

## (3) Plot PPV and NPV as a function of prev.range:
plot.PV.curves <- function(env, show.PVprev = TRUE, show.PVpoints = TRUE, log.scale = FALSE) {
  
  ## Current environment parameters:
  name <- env$name
  N <- env$N
  prev <- env$prev
  sens <- env$sens
  spec <- env$spec
  source <- env$source
  
  ## Current PPV and NPV values and labels:
  ## (a) from current data:
  # cur.PPV <- data$PPV # get.PPV(prev, sens, spec)
  # cur.NPV <- data$NPV # get.NPV(prev, sens, spec) 
  # cur.PPV.label <- data$PPV.label # paste0("PPV = ", pc(cur.PPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.PPV), "%)")
  # cur.NPV.label <- data$NPV.label # paste0("NPV = ", pc(cur.NPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.NPV), "%)")
  ## (b) Compute from scratch:
  cur.PPV <- get.PPV(prev, sens, spec)
  cur.NPV <- get.NPV(prev, sens, spec) 
  cur.PPV.label <- paste0("PPV = ", pc(cur.PPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.PPV), "%)")
  cur.NPV.label <- paste0("NPV = ", pc(cur.NPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.NPV), "%)")
  
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
  
  ## Reshape df.PVs into long format:
  df.PVs.long <- df.PVs %>% gather(metric, value, c(PPV, NPV))
  # names(df.PVs.long) <- c("prevalence", "metric", "value")
  df.PVs.long$metric <- factor(df.PVs.long$metric, # ensure factor and level order:  
                               levels = c("PPV", "NPV") #,
                               # labels = c("PPV = p(true | positive decision)", 
                               #            "NPV = p(false | negative decision)")
  )
  
  ## Additional plot options:
  prev.label <- paste0("prev = ", pc(prev), "%")
  col.prev <- col.grey.2
  cur.par.label <- paste0("(", prev.label, ", sens = ", pc(sens), "%, spec = ", pc(spec), "%)") # label
  y.min <- 0
  y.max <- 1
  x.just <- -.20
  y.just <- +.50
  
  if (!log.scale) { ## plot on linear scale: 
    p.PVs <- ggplot(data = df.PVs.long, aes(x = prev.range, y = value, group = metric)) +
      geom_line(aes(color = metric), size = 1.2) +
      # geom_point(aes(color = metric, shape = metric), size = 2) + 
      ## Scales:
      scale_y_continuous(breaks = seq(y.min, y.max, by = .10), limits = c(y.min, y.max)) + 
      ## (a) linear x scale:
      scale_x_continuous(breaks = seq(0, 1, by = .10)) + 
      labs(title = paste0("PPV and NPV by prevalence:\n", 
                          cur.PPV.label, ", ", cur.NPV.label, " ", cur.par.label, 
                          "\n[", name, ", ", source, "]"), 
           x = "Prevalence (linear scale)", y = "Probability") + 
      ## (b) log x scale:
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
      # geom_point(aes(color = metric, shape = metric), size = 2) +
      ## Scales:
      ## (a) linear x scale:
      # scale_x_continuous(breaks = seq(0, 1, by = .10)) + 
      # labs(title = paste0(name, ":\nPPV and NPV by prevalence ", sens.spec, "\n(", source, ")"),
      #      x = "Prevalence (linear scale)", y = "Probability") + 
      ## (b) log x scale:
      scale_x_log10(breaks = prev.scale) + 
      labs(title = paste0("PPV and NPV by prevalence:\n", 
                          cur.PPV.label, ", ", cur.NPV.label, " ", cur.par.label, 
                          "\n[", name, ", ", source, "]"), 
           x = "Prevalence (logarithmic scale)", y = "Probability") + 
      ## Colors: 
      scale_color_manual(values = c(col.ppv, col.npv)) +
      # scale_fill_manual(values = c(col.ppv, col.npv), name = "Metric:") +
      my.theme.legend
  }
  
  if (show.PVprev) {
    p.PVs <- p.PVs +
      ## Mark and label prev:
      geom_line(aes(x = prev), color = col.prev, linetype = 3, size = .6) + # vertical line at prev
      geom_point(aes(x = prev, y = 0), color = col.grey.3, fill = col.prev, shape = 21, size = 3) + # mark (prev, 0)
      geom_text(aes(x = prev, y = 0, label = prev.label), 
                color = col.prev, hjust = x.just, vjust = y.just, size = 4) # + # label prev  
  }
  
  if (show.PVpoints) {
    p.PVs <- p.PVs +
      ## Mark and label current PPV/NPV:
      geom_point(aes(x = prev, y = cur.PPV), 
                 color = col.grey.3, fill = col.ppv, shape = 21, size = 3) + # mark (prev, PPV)
      geom_text(aes(x = prev, y = cur.PPV, label = cur.PPV.label), 
                color = col.ppv, hjust = x.just, vjust = y.just, size = 4) + # label PPV
      geom_point(aes(x = prev, y = cur.NPV), 
                 color = col.grey.3, fill = col.npv, shape = 21, size = 3) + # mark (prev, NPV)
      geom_text(aes(x = prev, y = cur.NPV, label = cur.NPV.label), 
                color = col.npv, hjust = x.just, vjust = y.just, size = 4) # + # label NPV
  }
  
  
  return(p.PVs)
  
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## 3D graph:

## (1) Define parameters:
{ # Basic graph parameters:
  my.theta  <- -45 # horizontal viewing angle (higher values: more rotation)
  my.phi    <-   0 # vertical viewing angle (higher values: higher viewpoint)
  my.expand <- 1.1 # values < 1 shrink expansion in z-direction
  my.d      <- 1.5 # values > 1 reduce perspective effect 
  my.ltheta <- 200 # surface is illuminated from the direction specified by azimuth ltheta
  my.shade  <- .25 # values towards 1 yield shading similar to a point light source model and values towards 0 produce no shading.
}

## (2) Compute PPV and NPV for an entire matrix of values:
pv.matrix <- function(prev, sens, spec, metric) {
  
  # initializing DF (as matrix to store and return results):
  n.rows <- length(sens)
  n.cols <- length(spec)
  matrix <- as.data.frame(matrix(NA, 
                                 nrow = n.rows, 
                                 ncol = n.cols)) 
  names(matrix) <- sens 
  
  # loop through all rows and columns of pc.matrix: 
  for (row in 1:n.rows) {
    for (col in 1:n.cols) {
      
      # Compute the needed model DV for the current cell value:
      cell.val <- NA 
      
      if (metric == "PPV") {cell.val <- get.PPV(prev, sens[row], spec[col])} # compute PPV
      if (metric == "NPV") {cell.val <- get.NPV(prev, sens[row], spec[col])} # compute NPV
      
      # Store results:
      matrix[row, col] <- cell.val 
      
    }
  }
  
  return(matrix)
  
}

## (3) Plot both PPV and NPV in adjacent plots:
plot.PV.planes <- function(env, show.PVpoints = TRUE, 
                           cur.theta, cur.phi, cur.d, cur.expand, cur.ltheta, cur.shade) {
  
  ## Current environment parameters:
  name <- env$name
  N    <- env$N
  prev <- env$prev
  sens <- env$sens
  spec <- env$spec
  source <- env$source
  
  ## Current PPV and NPV values and labels:
  ## (a) from current data:
  # cur.PPV <- data$PPV # get.PPV(prev, sens, spec)
  # cur.NPV <- data$NPV # get.NPV(prev, sens, spec) 
  # cur.PPV.label <- data$PPV.label # paste0("PPV = ", pc(cur.PPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.PPV), "%)")
  # cur.NPV.label <- data$NPV.label # paste0("NPV = ", pc(cur.NPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.NPV), "%)")
  ## (b) Compute from scratch:
  cur.PPV <- get.PPV(prev, sens, spec)
  cur.NPV <- get.NPV(prev, sens, spec) 
  cur.PPV.label <- paste0("PPV = ", pc(cur.PPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.PPV), "%)")
  cur.NPV.label <- paste0("NPV = ", pc(cur.NPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.NPV), "%)")
  
  ## Ranges on x- and y-axes:
  sens.range <- seq(0.0, 1.0, by = .05) # range of sensitivity values 
  spec.range <- seq(0.0, 1.0, by = .05) # range of specificity values 
  
  ## Compute PPV and NPV matrices:
  PPV.mat <- pv.matrix(prev, sens.range, spec.range, metric = "PPV")
  NPV.mat <- pv.matrix(prev, sens.range, spec.range, metric = "NPV")
  
  ## Graph parameters:
  x <- sens.range
  y <- spec.range
  z.ppv <- as.matrix(PPV.mat)
  z.npv <- as.matrix(NPV.mat)
  z.lim <- c(0, 1) # range of z-axis
  # cur.par.label <- paste0("(", 
  #                         "prev = ", pc(prev), "%, ", 
  #                         "sens = ", pc(sens), "%, ", 
  #                         "spec = ", pc(spec), "%)")
  cur.par.label <- paste0("(prev = ", pc(prev), "%)")
  
  # Plot 2 plots (adjacent to each other):
  {
    par(mfrow = c(1, 2)) # Combine 2 plots in 1 row x 2 columns.
    par(bg = "white")
    
    ## 3D plot for PPV:
    p.ppv <- persp(x, y, z.ppv, 
                   theta = cur.theta, phi = cur.phi,  d = cur.d, expand = cur.expand, 
                   col = col.ppv, border = NA, # col.ppv, col.orange.1, 
                   ltheta = cur.ltheta, shade = cur.shade, 
                   ticktype = "detailed", nticks = 6, 
                   xlab = "sens", ylab = "spec", zlab = "PPV", zlim = z.lim, 
                   main = paste0(cur.PPV.label, "\n", cur.par.label)
    )
    
    if (show.PVpoints) { # add cur.PPV to plot:
      pmat <- p.ppv
      add.PPV <- trans3d(sens, spec, cur.PPV, pmat)
      points(add.PPV, pch = 21, col = "grey88", bg = col.ppv, lwd = 1.0, cex = 1.3)
    }
    
    ## 3D plot for NPV:    
    p.npv <- persp(x, y, z.npv, 
                   theta = cur.theta, phi = cur.phi,  d = cur.d, expand = cur.expand, 
                   col = col.npv, border = NA, # col.npv, col.blue.1, 
                   ltheta = cur.ltheta, shade = cur.shade, 
                   ticktype = "detailed", nticks = 6, 
                   xlab = "sens", ylab = "spec", zlab = "NPV", zlim = z.lim, 
                   main = paste0(cur.NPV.label, "\n", cur.par.label)
    )
    
    if (show.PVpoints) { # add cur.NPV to plot:
      pmat <- p.npv
      add.NPV <- trans3d(sens, spec, cur.NPV, pmat)
      points(add.NPV, pch = 21, col = "grey88", bg = col.npv, lwd = 1.0, cex = 1.3)
    }
    
    par(mfrow = c(1, 1)) # Remove special settings.
  }
  
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## Define server logic:

shinyServer(function(input, output, session){

  ## Define a common data structure:
  ## Generate data structures as lists of reactive elements:
  env <- reactiveValues(env = NULL) # list of current environment
  data <- reactiveValues(data = NULL) # list of derived parameters
  # population <- reactiveValues(population = NULL) # df of current population
  
  ## Observe inputs and generate data used in outputs:
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
    env$source <- "source information"
    
    ## (B) Derive data:
    ## (1) Determine the truth:
    data$n.true <- round((env$prev * env$N), 0) # n.true cases
    data$n.false <- (env$N - data$n.true)      # n.false cases
    
    ## Vector of true states:
    data$truth <- c(rep(TRUE, data$n.true), rep(FALSE, data$n.false)) 
    
    ## (2) Determine decisions:
    data$n.hi <- round((env$sens * data$n.true), 0)  # a. hits
    data$n.mi <- (data$n.true - data$n.hi)           # b. misses
    data$n.cr <- round((env$spec * data$n.false), 0) # d. correct rejections
    data$n.fa <- (data$n.false - data$n.cr)          # c. false alarms
    
    data$dec.pos <- data$n.hi + data$n.fa # 1. positive decisions (true & false)
    data$dec.neg <- data$n.mi + data$n.cr # 2. negative decisions (true & false)
    
    ## Vector of decisions (ordered by truth values):
    data$decision <- c(rep(TRUE, data$n.hi), rep(FALSE, data$n.mi), 
                       rep(TRUE, data$n.fa), rep(FALSE, data$n.cr))
    
    ## (3) SDT (status decision/truth):
    data$sdt <- c(rep("hi", data$n.hi), rep("mi", data$n.mi), 
                  rep("fa", data$n.fa), rep("cr", data$n.cr))
    
    ## (4) Coerce 3 vectors into ordered factors:
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
    
    ## (6) Compute and store current PPV and NPV values (to use in graphs and labels):
    data$PPV <- get.PPV(env$prev, env$sens, env$spec)
    data$NPV <- get.NPV(env$prev, env$sens, env$spec)
    # data$PPV.label <- paste0("PPV = ", pc(cur.PPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.PPV), "%)")
    # data$NPV.label <- paste0("NPV = ", pc(cur.NPV), "%") # paste0("(", pc(prev), "%; ", pc(cur.NPV), "%)")
    
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
  
  ## (a) Raw data table: 
  output$rawdatatable <- DT::renderDataTable(DT::datatable({data$population}))
  
  ## (b) Icon array:
  ## ... 
  
  ## (c) 2x2 confusion table (ordered by rows/decisions):
  output$confusiontable <- renderTable({matrix(data = c(data$n.hi, data$n.fa, data$dec.pos, 
                                                        data$n.mi, data$n.cr, data$dec.neg, 
                                                        data$n.true, data$n.false, env$N),
                                               nrow = 3, byrow = TRUE,
                                               dimnames = list(c("Positive decision:", 
                                                                 "Negative decision:", 
                                                                 "Truth sums:"), 
                                                               c("Condition true:", 
                                                                 "Condition false:", 
                                                                 "Decision sums:")
                                                               )
                                               )
    },
    bordered = TRUE,  
    hover = TRUE,  
    align = 'r',  
    digits = 0, 
    rownames = TRUE,
    na = 'missing'
    )  
  
  ## (d) Mosaic plot:
  output$mosaicplot <- renderPlot(mosaicplot(table(data$population$Truth,
                                                   data$population$Decision),
                                             col = c(col.sand.light, col.sand.mid),
                                                   # c(col.sand.mid, col.sand.dark), 
                                                   # c(col.blue.2, col.blue.4),
                                                   # c(col.green.1, col.red.1),
                                             xlab = "Truth",
                                             ylab = "Decision",
                                             main = paste0(env$name, "\n(N = ", env$N, ")")
                                             )
                                  )
  
  ## (e) Tree with natural frequencies:
  output$nftree <- renderPlot(plot.nftree(env, # use current environment parameters     
                                          data # use current data (hi, mi, fa, cr)
                                          )
                              )
  
  ## (f) 2D plot of PPV and NPV as a function of prev.range:
  output$PVs <- renderPlot(plot.PV.curves(env, # use current environment parameters
                                          show.PVprev = input$boxPVprev, # mark current prevalence in plot                
                                          show.PVpoints = input$boxPVpoints1, # mark cur.PPV/cur.NPV in plot
                                          log.scale = input$boxPVlog # plot x on log scale 
                                          )
                           )

  ## (g) 3D plots of PPV and NPV planes as functions of sens and spec:
  output$PVplanes <- renderPlot(plot.PV.planes(env, # use current environment parameters
                                               show.PVpoints = input$boxPVpoints2, # mark cur.PPV/cur.NPV in plot
                                               cur.theta = input$theta, # horizontal rotation
                                               cur.phi = input$phi,     # vertical rotation
                                               cur.d = my.d,            # input$d,
                                               cur.expand = my.expand,  # input$expand,
                                               cur.ltheta = my.ltheta,  # input$ltheta,
                                               cur.shade = my.shade     # input$shade
                                               )
                                )
  
}
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
## eof. #