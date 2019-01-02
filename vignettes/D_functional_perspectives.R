## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("riskyr")  # load the "riskyr" package

## ----plot_curve_1, fig.width = 7.0, fig.height = 5.0, fig.show = 'asis', fig.cap = "Showing PPV and NPV as a function of prevalence (for a prevalance of 1% and given values of sensitivity and specificity) in the original mammography screening scenario."----
plot_curve(prev = .01, sens = .80, spec = (1 - .096), 
           what = c("prev", "PPV", "NPV"), 
           title_lbl = "Mammography screening", cex.lbl = .8)

## ----plot_curve_2, eval = FALSE, fig.width = 7.0, fig.height = 5, fig.show = 'asis', fig.cap = "Showing PPV and NPV as a function of prevalence (for an increased prevalence of 60% and given values of sensitivity and specificity)."----
#  high.prev <- .60   # assume increased prevalence due to BRCA1 mutation
#  
#  plot_curve(prev = high.prev, sens = .80, spec = (1 - .096),
#             what = c("prev", "PPV", "NPV"),
#             title_lbl = "Mammography screening (BRCA1 mutation)", cex.lbl = .80)

## ----plot_curve_3, fig.width = 7.0, fig.height = 5, fig.show = 'asis', fig.cap = "Curves that show PPV/NPV, ppod, and acc as a function of an prevalence (for given values of sensitivity and specificity) when assuming an increased prevalence of 60% and an uncertainty range of 5%."----
high.prev <- .60   # assume increased prevalence due to BRCA1 mutation

plot_curve(prev = high.prev, sens = .80, spec = (1 - .096), 
           what = c("prev", "PPV", "NPV", "ppod", "acc"), 
           title_lbl = "Mammography screening (BRCA1 mutation)", uc = .05, cex.lbl = .80)

## ----plot_plane_PPV, eval = TRUE, fig.width = 5, fig.height = 4, fig.show = 'asis', fig.cap = "Plane showing the positive predictive value (PPV) as a function of sensitivity and specificity for a given prevalence."----
plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "PPV",  
           title_lbl = "A. Mammography (BRCA1)", cex.lbl = .8)

## ----plot_planes_else, eval = FALSE, fig.width = 6, fig.height = 4.5, fig.show = 'asis', fig.cap = "Planes showing NPV, the proportion of positive predictions (ppod), and overall accuracy (acc), as a function of sensitivity and specificity for a given prevalence."----
#  plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "NPV",
#             title_lbl = "B. Mammography (BRCA1)", cex.lbl = .8)
#  plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "ppod", what_col = "firebrick",
#             title_lbl = "C. Mammography (BRCA1)", phi = 45, cex.lbl = .8)
#  plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "acc",  what_col = "forestgreen",
#             title_lbl = "D. Mammography (BRCA1)", cex.lbl = .8)

