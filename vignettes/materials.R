## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("riskyr")  # load the "riskyr" package

## ----plot_curve, fig.width = 7.2, fig.height = 5, fig.show = 'asis', fig.cap = "A plot that shows PPV and NPV as a function of prevalence (for given values of sensitivity and specificity)."----
library("riskyr")  # load the "riskyr" package

plot_curve(prev = .01, sens = .80, spec = (1 - .096), 
           log.scale = TRUE, title.lbl = "Mammography screening")

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

