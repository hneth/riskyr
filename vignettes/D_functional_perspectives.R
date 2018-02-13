## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("riskyr")  # load the "riskyr" package

## ----plot_curve_1, fig.width = 7.2, fig.height = 5, fig.show = 'asis', fig.cap = "Curves that show PPV and NPV as a function of an prevalence (for given values of sensitivity and specificity) in the original mammography screening scenario."----
plot_curve(prev = .01, sens = .80, spec = (1 - .096), 
           what = c("prev", "PPV", "NPV"), 
           title.lbl = "Mammography screening", cex.lbl = .8)

## ----plot_curve_2, fig.width = 7.2, fig.height = 5, fig.show = 'asis', fig.cap = "Curves that show PPV and NPV as a function of an prevalence (for given values of sensitivity and specificity) when assuming an increased prevalence of 60%."----
high.prev <- .60   # assume increased prevalence due to BRCA1 mutation

plot_curve(prev = high.prev, sens = .80, spec = (1 - .096), 
           what = c("prev", "PPV", "NPV"), 
           title.lbl = "Mammography screening (BRCA1 mutation)", cex.lbl = .8)

## ----plot_curve_3, fig.width = 7.2, fig.height = 5, fig.show = 'asis', fig.cap = "Curves that show PPV and NPV as a function of an prevalence (for given values of sensitivity and specificity) when assuming an increased prevalence of 60%."----
high.prev <- .60   # assume increased prevalence due to BRCA1 mutation

plot_curve(prev = high.prev, sens = .80, spec = (1 - .096), 
           what = c("prev", "PPV", "NPV", "ppod", "acc"), 
           title.lbl = "Mammography screening (BRCA1 mutation)", cex.lbl = .8)

## ----plot_plane, fig.width = 6, fig.height = 4.5, fig.show = 'asis', fig.cap = "Planes that show PPV, NPV, the proportion of positive predictions (ppod) and overall accuracy (acc) as a function of sensitivity and specificity (for given prevalence)."----
plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "PPV",  title.lbl = "A. Mammography (BRCA1)", cex.lbl = .8)
plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "NPV",  title.lbl = "B. Mammography (BRCA1)", cex.lbl = .8)
plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "ppod", title.lbl = "C. Mammography (BRCA1)", phi = 45, cex.lbl = .8)
plot_plane(prev = high.prev, sens = .80, spec = (1 - .096), what = "acc",  title.lbl = "D. Mammography (BRCA1)", cex.lbl = .8)

