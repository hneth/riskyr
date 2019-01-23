## ----setup, include = FALSE, message = FALSE-----------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("riskyr")  # load the "riskyr" package

## ----plot_tab_demo, eval = TRUE, fig.width = 7, fig.height = 5, fig.show = 'hold', fig.cap = "Example of a 2x2 confusion table in `riskyr`."----
## (1) Plot table from basic input parameters: ----- 
plot_tab(prev = .05, sens = .75, spec = .66, N = 1000,
         p_lbl = "def") # show condition probabilies (by default)

## Alternative version (with horizontal p):
# plot_tab(prev = .05, sens = .75, spec = .66, N = 1000,
#          p_lbl = "def", p_split = "h") # show decision probabilities

## (2) Plot an existing riskyr scenario: ----- 
# s1 <- scenarios$n1  # identify s1 from scenarios
# plot(s1, type = "tab", p_lbl = "def")

## ----plot_tab_cdac, eval = TRUE, fig.width = 7, fig.height = 5, fig.show = 'hold', fig.cap = "Arranging a 2x2 confusion table by condition and by accuracy."----
plot_tab(prev = .05, sens = .75, spec = .66, N = 1000,
         by = "cdac", p_split = "h", 
         p_lbl = "def", title_lbl = "Scenario 2")

