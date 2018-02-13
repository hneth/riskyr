## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("riskyr")  # load the "riskyr" package

## ----plot_curve, fig.width = 7.2, fig.height = 5, fig.show = 'asis', fig.cap = "Curves that show PPV and NPV as a function of prevalence (for given values of sensitivity and specificity)."----
library("riskyr")  # load the "riskyr" package

plot_curve(prev = .01, sens = .80, spec = (1 - .096), 
           log.scale = TRUE, title.lbl = "Mammography screening")

## ----plot_plane, fig.width = 6, fig.height = 5, fig.show = 'asis', fig.cap = "Planes that show PPV, NPV, the proportion of positive predictions (ppod) and overall accuracy (acc) as a function of sensitivity and specificity (for given prevalence)."----
plot_plane(prev = .01, sens = .80, spec = (1 - .096), what = "PPV",  title.lbl = "Mammography screening (A)")
plot_plane(prev = .01, sens = .80, spec = (1 - .096), what = "NPV",  title.lbl = "Mammography screening (B)")
plot_plane(prev = .01, sens = .80, spec = (1 - .096), what = "ppod", title.lbl = "Mammography screening (C)")
plot_plane(prev = .01, sens = .80, spec = (1 - .096), what = "acc",  title.lbl = "Mammography screening (D)")

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

