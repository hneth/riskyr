## plot_nftree.R | riskyR
## 2018 01 11
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
                        col.txt = grey(.01, alpha = .99), # black
                        col.border = col.grey.4,
                        col.N = col.sand.light,
                        col.true = col.N, col.false = col.N,
                        col.hi = pal["hi"], col.mi = pal["mi"], col.fa = pal["fa"], col.cr = pal["cr"],
                        # shadow options:
                        col.shadow = col.sand.dark, cex.shadow = 0 # [allow using shadows]
                        ){


    ## Text/labels in 7 boxes:                          # NOT used yet:
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

    ## ToDo: Use more informative arrow/edge labels:
    prev.lbl <- paste0("prev = ", as_pc(prev), "%")

    M[2, 1] <- "prevalence" # ERROR: WHY does prev.lbl not work with spaces???
    M[3, 1] <- "(N - n.true)"
    M[4, 2] <- "sensitivity"
    M[5, 2] <- "(n.true - n.hi)"
    M[6, 3] <- "(n.false - n.cr)"
    M[7, 3] <- "specificity"

    ## Plot matrix M (from diagram package):
    pp <- diagram::plotmat(M, # square coefficient matrix, specifying the links (rows = to, cols = from)
                  pos = c(1, 2, 4),
                  curve = 0.0, # no curve (> 0 curve left, < 0 curve right)
                  name = names,
                  relsize	= .98, # a scaling factor for the size of the graph
                  lwd = 1.5,
                  ## Boxes:
                  box.size = .11, # length of box
                  box.prop = 2/3, # proportionality (length/width) ratio of box
                  box.type = "rect", # "ellipse", "diamond", "circle", "hexa", "multi", "none"
                  box.col = c(col.N, col.true, col.false, col.hi, col.mi, col.fa, col.cr), # WAS: "lightyellow"
                  box.lcol = col.border,
                  box.lwd = 2.0,
                  lcol = col.border, # default color for box and arrow lines
                  ## Text in Boxes:
                  txt.col = col.txt,
                  box.cex = .95, # relative size of text in boxes
                  txt.font = 1, # 1 = plain, 2 = bold, ...
                  ## Arrows:
                  cex.txt = .90, # relative size of arrow text
                  arr.pos = .50, # relative position of arrowhead on arrow segment/curve
                  arr.type = "triangle", # one of "curved", "triangle", "circle", "ellipse", "T", "simple"
                  arr.length = .20,
                  arr.width = .15,
                  arr.col = col.border,
                  shadow.size = cex.shadow, # .005
                  shadow.col = col.shadow #,
                  # main = paste0(title.lbl, ":\n", "Tree of natural frequencies (N = ", N, ")")
                  )

    ## Title:
    cur.title.lbl = paste0(title.lbl, ":\n", "Tree of natural frequencies") # , "(N = ", N, ")")
    title(cur.title.lbl, adj = 0.5, line = -1.0, font.main = 1) # (left, lowered, normal font)

    ## Margin text:
    cur.par.lbl <-  paste0("(", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%)")
    mtext(cur.par.lbl, side = 1, line = 1, adj = 1, col = grey(.33, .99), cex = .90)

    # return(pp) # returns elements of diagram object

}

## Check:
# plot_nftree()
# plot_nftree(col.txt = "black", col.border = col.sand.dark,  cex.shadow = .011)
# plot_nftree(col.N = "lightyellow", col.shadow = "steelblue3", cex.shadow = .015)


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
