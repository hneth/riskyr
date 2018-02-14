## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example_design, eval = FALSE----------------------------------------
#  library(riskyr)  # load riskyr
#  
#  ## (1) Define your own scenario: ----------
#  hustosis <- riskyr(prev = .04, sens = .80, spec = (1 - .05),
#                     N = 1000, popu.lbl = "representative sample"
#                     scen.lbl = "Example", cond.lbl = "Screening for hustosis",
#                     dec.lbl = "screening")
#  
#  ## View parameters:
#  summary(hustosis)
#  summary(hustosis, summarize = "probs")
#  
#  ## View graphics:
#  plot(hustosis, plottype = "ftree")
#  plot(hustosis, plottype = "icons")
#  # plot(hustosis, plottype = "mosaic")
#  plot(hustosis, plottype = "curve")
#  # plot(hustosis, plottype = "plane", what = "NPV")

## ----example_use_a, eval = FALSE-----------------------------------------
#  ## (2) Inspect an existing riskyr scenario: ----------
#  
#  ## (a) PSA screening with high prevalence: ----------
#  
#  s21 <- scenarios$n21  # assign pre-defined Scenario_21 to s
#  
#  ## View parameters:
#  summary(s21) # shows all scenario information
#  summary(s21, summarize = "prob") # shows probabilities
#  summary(s21, summarize = "freq") # shows frequencies
#  
#  ## View graphics:
#  plot(s21) # plots a network diagram (by default)
#  plot(s21, plottype = "icons")
#  plot(s21, plottype = "mosaic")
#  plot(s21, plottype = "curve", what = "all")
#  plot(s21, plottype = "plane", what = "PPV")

## ----example_use_b, eval = FALSE-----------------------------------------
#  ## (b) same with low prevalence: ----------
#  
#  s22 <- scenarios$n22  # assign pre-defined Scenario_22 to s22
#  
#  # ...
#  
#  ## Contrast two versions:
#  plot(s22, plottype = "plane", what = "PPV")
#  plot(s22, plottype = "plane", what = "PPV")
#  

