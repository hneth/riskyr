## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----plot_fnet, fig.width = 7.1, fig.height = 7.5, fig.show = 'asis'-----
plot_fnet(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000, 
          area = "no", 
          title.lbl = "Mammography screening")

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

