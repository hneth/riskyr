## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# URLs:
url_riskyr_org <- "https://riskyr.org/"

# Load pkg:
library("riskyr")

## ----plot-curve-1, fig.align = "center", fig.width = 6, fig.height = 4.5, fig.show = 'hold', fig.cap = "Showing PPV and NPV as a function of prevalence (for a prevalance of 1% and given values of sensitivity and specificity) in the original mammography screening scenario.", fig.alt = "A riskyr curve plot showing PPV and NPV as a function of prev."----
plot_curve(prev = .01, sens = .80, spec = (1 - .096), 
           what = c("prev", "PPV", "NPV"), 
           title_lbl = "Mammography screening", cex.lbl = .8)

## ----plot-curve-2, eval = FALSE, fig.align = "center", fig.width = 6, fig.height = 4.5, fig.show = 'hold', fig.cap = "Showing PPV and NPV as a function of prevalence (for an increased prevalence of 60% and given values of sensitivity and specificity).", fig.alt = "A riskyr curve plot showing PPV and NPV for prev = 60%"----
# high.prev <- .60   # assume increased prevalence due to BRCA1 mutation
# 
# plot_curve(prev = high.prev, sens = .80, spec = (1 - .096),
#            what = c("prev", "PPV", "NPV"),
#            title_lbl = "Mammography screening (BRCA1 mutation)", cex.lbl = .80)

## ----plot-curve-3, fig.align = "center", fig.width = 6, fig.height = 4.5, fig.show = 'hold', fig.cap = "Curves that show PPV/NPV, ppod, and acc as a function of an prevalence (for given values of sensitivity and specificity) when assuming an increased prevalence of 60% and an uncertainty range of 5%.", fig.alt = "A riskyr curve plot showing PPV, NPV, acc, and ppod as a function of prev."----
high.prev <- .60   # assume increased prevalence due to BRCA1 mutation

plot_curve(prev = high.prev, sens = .80, spec = (1 - .096), 
           what = c("prev", "PPV", "NPV", "ppod", "acc"), 
           title_lbl = "Mammography screening (BRCA1 mutation)", uc = .05, cex.lbl = .80)

## ----plot-plane-PPV, eval = TRUE, fig.align = "center", fig.width = 5.0, fig.height = 3.5, fig.show = 'hold', fig.cap = "Plane showing the positive predictive value (PPV) as a function of sensitivity and specificity for a given prevalence.", fig.alt = "A riskyr plane plot showing PPV as a function of sens and spec"----
plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "PPV",  
           title_lbl = "A. Mammography (BRCA1)", cex.lbl = .8)

## ----plot-planes-else, eval = FALSE, fig.width = 5, fig.height = 4, fig.show = 'asis', fig.cap = "Planes showing NPV, the proportion of positive predictions (ppod), and overall accuracy (acc), as a function of sensitivity and specificity for a given prevalence.", fig.alt = "Various riskyr plane plots."----
# plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "NPV",
#            title_lbl = "B. Mammography (BRCA1)", cex.lbl = .8)
# plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "ppod", what_col = "firebrick",
#            title_lbl = "C. Mammography (BRCA1)", phi = 45, cex.lbl = .8)
# plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "acc",  what_col = "forestgreen",
#            title_lbl = "D. Mammography (BRCA1)", cex.lbl = .8)

