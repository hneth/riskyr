## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
custom.scenario <- riskyr(scen.lbl = "Using R", scen.lng = txt$scen.lng,
                  scen.txt = txt$scen.txt, popu.lbl = txt$popu.lbl,
                  cond.lbl = txt$cond.lbl,
                  cond.true.lbl = txt$cond.true.lbl, cond.false.lbl = txt$cond.false.lbl,
                  dec.lbl = txt$dec.lbl,
                  dec.pos.lbl = txt$dec.pos.lbl, dec.neg.lbl = txt$dec.neg.lbl,
                  hi.lbl = txt$hi.lbl, mi.lbl = txt$mi.lbl,
                  fa.lbl = txt$fa.lbl, cr.lbl = txt$cr.lbl,
                  prev = .20,  # prevalence of
                  sens = num$sens,
                  spec = num$spec, fart = NA, N = freq$N,
                  scen.src = txt$scen.src, scen.apa = txt$scen.apa)

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

