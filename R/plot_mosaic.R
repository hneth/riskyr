## plot_mosaic.R | riskyR
## 2018 01 16
## -----------------------------------------------
## Plot mosaicplot that expresses freq as area
## (size and proportion)

## -----------------------------------------------
## Dependencies:

# library("vcd") # moved to "Imports:" in in DESCRIPTION!
# library("grid")

## -----------------------------------------------
## plot_nftree: Plot tree diagram of natural frequencies
## (using only necessary arguments with good defaults):

## Assuming that df of popu is known [see comp_popu()]

plot_mosaic <- function(pop = popu,
                        title.lbl = txt$scen.lbl,
                        col.sdt = pal[4:7]
) {

  ## Define plot area:
  # plot(0, type = 'n')

  ## Text labels:
  cur.title.lbl <- paste0(title.lbl, ":\n", "Mosaic plot") # , "(N = ", N, ")")
  cur.par.lbl <-  paste0("(", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%)")

  ## Mosaic plot:
  ## (a) original version:
  # vcd::mosaic(Truth ~ Decision, data = pop,
  #             shade = TRUE, colorize = TRUE,
  #             split_vertical = FALSE,
  #             gp = grid::gpar(fill = matrix(data = col.sdt, nrow = 2, ncol = 2, byrow = FALSE)),
  #             main_gp = grid::gpar(fontsize = 12, fontface = 1),
  #             main = paste0(cur.title.lbl)#, "\n", cur.par.lbl)
  #             )

  ## (b) flipped version:
  vcd::mosaic(Decision ~ Truth, data = pop,
              shade = TRUE, colorize = TRUE,
              split_vertical = TRUE,
              gp = grid::gpar(fill = matrix(data = col.sdt, nrow = 2, ncol = 2, byrow = TRUE)),
              main_gp = grid::gpar(fontsize = 12, fontface = 1),
              main = paste0(cur.title.lbl)#, "\n", cur.par.lbl)
              )

  ## Title and margin text:
  # title(cur.title.lbl, adj = 0.5, line = -0.5, font.main = 1) # (left, lowered, normal font)
  # mtext(cur.par.lbl, side = 1, line = 1, adj = 1, col = grey(.33, .99), cex = .90)

}

## Check:
# plot_mosaic()
# plot_mosaic(title.lbl = "Just testing", col.sdt = "goldenrod")

## -----------------------------------------------
## (+) ToDo:

## - make mosaic plot dependent on basic parameters
##   (i.e., compute comp_popu(), rather than providing it as input)?
## - add a simpler version that only shows cond.true vs. cond.false
## - adjust parameters (zero size and gap width)
## - add labels (frequencies) to plot?

## -----------------------------------------------
## eof.
