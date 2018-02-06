## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----plot_fnet, fig.width = 7.2, fig.height = 7.5, fig.show = 'asis', fig.cap = "A network diagram that shows frequencies as nodes and probabilities as edges between nodes."----
plot_fnet(prev = .01, sens = .80, spec = NA, fart = .096,  # 3 essential probabilities
          N = 1000,       # 1 frequency
          area = "no",    # all boxes have the same size
          p.lbl = "nam",  # show probability names on edges
          title.lbl = "Mammography screening")

## ----plot_fnet_example, fig.width = 7.2, fig.height = 7.2, fig.show = 'asis', fig.cap = "An example network diagram that shows how probabilities can be computed as ratios of frequencies."----
plot_fnet(prev = .50, sens = .80, spec = .60,  # 3 essential probabilities
          N = 10,         # 1 frequency
          area = "no",    # all boxes have the same size
          p.lbl = "num",  # show numeric probability values on edges
          title.lbl = "A simple example")

