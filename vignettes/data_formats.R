## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----plot_fnet, fig.width = 7.2, fig.height = 7.5, fig.show = 'asis'-----
plot_fnet(prev = .01, sens = .80, spec = NA, fart = .096,  # 3 essential probabilities
          N = 1000,       # 1 frequency
          area = "no",    # all boxes have the same size
          p.lbl = "nam",  # show probability names on edges
          title.lbl = "Mammography screening")

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

