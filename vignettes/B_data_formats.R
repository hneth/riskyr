## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# URLs:
url_riskyr_org <- "http://134.34.54.61:3838/spds/riskyr/" # Note: "http://riskyr.org/" redirects there.

## ----prism-plot-1, message = FALSE, fig.width = 6, fig.height = 5, fig.show = 'asis', fig.cap = "A prism plot showing frequencies as nodes and probabilities as edges linking nodes."----
library("riskyr") # load the "riskyr" package

plot_prism(prev = .01, sens = .80, spec = NA, fart = .096,  # 3 essential probabilities
           N = 1000,       # 1 frequency
           area = "no",    # same size for all boxes
           p_lbl = "abb",  # show abbreviated names of probabilities on edges
           title_lbl = "Example")

## ----prism-plot-2, fig.width = 6, fig.height = 5, fig.show = 'asis', fig.cap = "A prism plot showing how probabilities can be computed as ratios between frequencies."----
plot_prism(prev = .50, sens = .80, spec = .60,  # 3 essential probabilities
           N = 10,         # population frequency
           scale = "f",    # scale by frequency, rather than probability ("p") 
           area = "sq",    # boxes as squares, with sizes scaled by current scale  
           p_lbl = "num",  # show numeric probability values on edges
           title_lbl = "Probabilities as ratios between frequencies")

