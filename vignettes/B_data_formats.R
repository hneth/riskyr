## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----prism_plot_1, message = FALSE, fig.width = 7.2, fig.height = 6, fig.show = 'asis', fig.cap = "A network diagram showing frequencies as nodes and probabilities as edges linking nodes."----
library("riskyr") # load the "riskyr" package

plot_prism(prev = .01, sens = .80, spec = NA, fart = .096,  # 3 essential probabilities
           N = 1000,       # 1 frequency
           area = "no",    # same size for all boxes
           p_lbl = "abb",  # show abbreviated names of probabilities on edges
           title_lbl = "Example")

## ----prism_plot_2, fig.width = 7.2, fig.height = 6, fig.show = 'asis', fig.cap = "A simple network diagram that shows how probabilities can be computed as ratios of frequencies."----
plot_prism(prev = .50, sens = .80, spec = .60,  # 3 essential probabilities
           N = 10,         # 1 frequency
           area = "no",    # all boxes have the same size
           p_lbl = "num",  # show numeric probability values on edges
           title_lbl = "Probabilities as ratios of frequencies")

