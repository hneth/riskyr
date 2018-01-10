## plot_nftree.R | riskyR
## 2018 01 10
## -----------------------------------------------
## Plot a tree diagram of natural frequencies

## -----------------------------------------------
## Dependencies:

# library("diagram") # moved to "Imports:" in in DESCRIPTION!

## -----------------------------------------------
## plot_nftree: Plot tree diagram of natural frequencies
## (using only necessary arguments with good defaults):

## Assuming that freq (+ num txt pal) are known!

plot_nftree <- function(prev = num$prev, sens = num$sens, spec = num$spec, fart = num$fart, # key parameters
                        N = freq$N, n.true = freq$cond.true, n.false = freq$cond.false,     # freq info
                        n.hi = freq$hi, n.mi = freq$mi, n.fa = freq$fa, n.cr = freq$cr,
                        show.stuff = TRUE,           # user options [adjustable by inputs]
                        title.lbl = txt$scen.lbl,    # custom labels
                        popu.lbl = txt$popu.lbl,
                        cond.lbl = txt$cond.lbl,     # condition
                        cond.true.lbl = txt$cond.true.lbl,
                        cond.false.lbl = txt$cond.false.lbl,
                        dec.lbl = txt$dec.lbl,       # decision
                        dec.true.lbl = txt$dec.true.lbl,
                        dec.false.lbl = txt$dec.false.lbl,
                        sdt.hi.lbl = txt$sdt.hi.lbl, # SDT combinations
                        sdt.mi.lbl = txt$sdt.mi.lbl,
                        sdt.fa.lbl = txt$sdt.fa.lbl,
                        sdt.cr.lbl = txt$sdt.cr.lbl,
                        # custom colors:
                        col.N = col.sand.light, col.true = col.N, col.false = col.N,
                        col.hi = pal["hi"], col.mi = pal["mi"], col.fa = pal["fa"], col.cr = pal["cr"],
                        # shadow options:
                        col.shadow = col.sand.dark, siz.shadow = 0 # [allow using shadows]
                        ){

    ## Graphic parameters (not adjusted by user):
    # col.txt = grey(.01, alpha = .99)   # text
    # col.border <- grey(.01, alpha = .99) # borders, shadows, ...
    cex.txt <- 0.8 # adjust text size (on edges only?)
    cex.lwd <- 2.0 # adjust line width

    ## Labels:                                          # NOT used yet:
    names <- c(paste0("Population", ":\n", "N = ", N),  # popu.lbl
               paste0(cond.true.lbl, ":\n",  n.true),
               paste0(cond.false.lbl, ":\n", n.false),
               paste0(sdt.hi.lbl, ":\n", n.hi),
               paste0(sdt.mi.lbl, ":\n", n.mi),
               paste0(sdt.fa.lbl, ":\n", n.fa),
               paste0(sdt.cr.lbl, ":\n", n.cr)
    )

    ## Make matrix M:
    M <- matrix(nrow = 7, ncol = 8, byrow = TRUE, data = 0)

    prev.lbl <- paste0("prev = ", as_pc(prev), "%")

    M[2, 1] <- "prevalence"     # ERROR: WHY does prev.lbl not work???
    M[3, 1] <- "(N - n.true)"
    M[4, 2] <- "sensitivity"
    M[5, 2] <- "(n.true - n.hi)"
    M[6, 3] <- "(n.false - n.cr)"
    M[7, 3] <- "specificity"

    ## Plot matrix M:
    pp <- plotmat(M,
                  pos = c(1, 2, 4),
                  curve = 0.0, # no curve (> 0 curve left, < 0 curve right)
                  name = names,
                  box.lwd = cex.lwd,
                  box.size = .10,
                  box.prop = 2/3,
                  box.type = "square", # "circle",
                  box.col = c(col.N, col.true, col.false, col.hi, col.mi, col.fa, col.cr), # WAS: "lightyellow"
                  shadow.col = col.shadow,
                  shadow.size = siz.shadow, # .005
                  lwd = cex.lwd,
                  cex.txt = cex.txt #,
                  # main = paste0(title.lbl, ":\n", "Tree of natural frequencies (N = ", N, ")")
                  )

    ## Title:
    p.title.lbl = paste0(title.lbl, ":\n", "Tree of natural frequencies") # , "(N = ", N, ")")
    title(p.title.lbl, adj = 0.5, line = -1.5, font.main = 1) # (left, raised, normal font)

    # return(pp)

}

## Check:
# plot_nftree()
# plot_nftree(siz.shadow = .015)
# plot_nftree(col.N = "lightyellow", col.shadow = "steelblue3", siz.shadow = .015)

## -----------------------------------------------
## (+) ToDo:

## - fix ERROR above!
## - provide more info on current numeric inputs (prev, sens, spec, fart) on edges
## - Note: Parameters of num (prev, sens, spec, fart, N) are not USED above.
##         Make a version with option for re-calculating freq for current values!
## - make text color adjustable (using col.txt)
## - pimp plot (labels, colors, transparency)

## -----------------------------------------------
## eof.
