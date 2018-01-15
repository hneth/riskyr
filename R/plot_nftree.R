## plot_nftree.R | riskyR
## 2018 01 15
## -----------------------------------------------
## Plot a tree diagram of natural frequencies

## -----------------------------------------------
## Dependencies:

# library("diagram") # moved to "Imports:" in in DESCRIPTION!

## -----------------------------------------------
## plot_nftree: Plot tree diagram of natural frequencies
## (using only necessary arguments with good defaults):

## Assuming that freq$N (+ num txt pal) is known!

plot_nftree <- function(prev = num$prev, sens = num$sens, spec = num$spec, fart = num$fart, # key parameters
                          N = freq$N,     # freq info
                          box.area = "o", # "o"...other (default), "s"...square, "r"...rectangular
                          ## Labels:
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
                          ## Colors:
                          col.boxes = pal[1:7],
                          # col.N = col.sand.light,
                          # col.true = col.N, col.false = col.N,
                          # col.hi = pal["hi"], col.mi = pal["mi"], col.fa = pal["fa"], col.cr = pal["cr"],
                          col.txt = grey(.01, alpha = .99), # black
                          col.border = col.grey.4,
                          ## Shadows:
                          col.shadow = col.sand.dark,
                          cex.shadow = 0 # [allow using shadows]
){


  ## Compute cur.freq based on current parameters:
  cur.freq <- comp_freq(N, prev, sens, spec, round = TRUE)
  n.true <- cur.freq$cond.true
  n.true = cur.freq$cond.true
  n.false = cur.freq$cond.false
  n.hi = cur.freq$hi
  n.mi = cur.freq$mi
  n.fa = cur.freq$fa
  n.cr = cur.freq$cr

  ## Text/labels in 7 boxes:                          # NOT used yet:
  ## Default:
  names <- c(paste0("Population", ":\n", "N = ", N),  # popu.lbl
             paste0(cond.true.lbl, ":\n",  n.true),
             paste0(cond.false.lbl, ":\n", n.false),
             paste0(sdt.hi.lbl, ":\n", n.hi),
             paste0(sdt.mi.lbl, ":\n", n.mi),
             paste0(sdt.fa.lbl, ":\n", n.fa),
             paste0(sdt.cr.lbl, ":\n", n.cr)
  )
  ## Alternative:
  if (box.area != "o") {
    ## Reduced names (as areas get quite small):
    names <- c(paste0("N = ", N),  # popu.lbl
               paste0("true:\n",  n.true),
               paste0("false:\n", n.false),
               paste0("hi:\n", n.hi),
               paste0("mi:\n", n.mi),
               paste0("fa:\n", n.fa),
               paste0("cr:\n", n.cr)
    )
  }

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

  ## Distinguish between 3 different plot types (based on box.area setting):
  ## 1. default case: Rectangles of same width and height (non-proportional)
  if (box.area == "o") {
    x.pop <- .11   # basic width of population box
    x.y.pop <- 2/3 # basic proportion is rectangular (width > height)

    x.boxes <- rep(x.pop, 7)    # all boxes have the same width
    x.y.prop <- rep(x.y.pop, 7) # all boxes have the same proportion
  }

  ## 2. squares that sum to area at next higher level:
  if (box.area == "s") {

    x.pop <- .10 # basic width of population box: Area N = x.pop^2
    x.y.pop <- 1/1 # square

    ## Determine other box widths by proportions in freq:
    x.true <- sqrt(n.true/N * x.pop^2)
    x.false <- sqrt(n.false/N * x.pop^2)
    if (!all.equal(x.pop^2, sum(x.true^2, x.false^2))) {
      warning("sumtree 1: Sum of True and False area differs from Population area.")
    }

    x.hi <- sqrt(n.hi/N * x.pop^2)
    x.mi <- sqrt(n.mi/N * x.pop^2)
    x.fa <- sqrt(n.fa/N * x.pop^2)
    x.cr <- sqrt(n.cr/N * x.pop^2)
    if (!all.equal(x.true^2, sum(x.hi^2, x.mi^2))) {
      warning("sumtree 2: Sum of HI and MI area differs from cond True area.")
    }
    if (!all.equal(x.false^2, sum(x.fa^2, x.cr^2))) {
      warning("sumtree 3: Sum of FA and CR area differs from cond False area.")
    }
    if (!all.equal(x.pop^2, sum(x.hi^2, x.mi^2, x.fa^2, x.cr^2))) {
      warning("sumtree 4: Population area differs from the area sum of all 4 SDT cases.")
    }

    x.boxes <- c(x.pop, x.true, x.false, x.hi, x.mi, x.fa, x.cr)
    x.y.prop <- rep(x.y.pop, 7) # all boxes have the same proportion (squares)
  }

  ## 3. rectangles that sum to area at next higher level:
  if (box.area == "r") {

    x.pop <- .10   # basic width x of population box: Area N = x.pop^2
    x.y.pop <- 1/1 # square

    ## Determine other box widths by proportions in freq:
    x.true <- (n.true/N) * x.pop # scale x.pop by proportion
    x.y.true <- x.pop/x.true

    x.false <- n.false/N * x.pop # scale x.pop by proportion
    x.y.false <- x.pop/x.false

    if (!all.equal(x.pop^2, sum((x.true * x.pop), (x.false * x.pop)))) {
      warning("rectree 1: Sum of True + False areas differs from Population area.")
    }

    x.hi <- (n.hi/n.true) * x.pop # scale x.pop by sens
    x.y.hi <- x.true/x.hi

    x.mi <-  (1 - (n.hi/n.true)) * x.pop # scale x.pop by (1 - sens)
    x.y.mi <- x.true/x.mi

    x.cr <- (n.cr/n.false) * x.pop # scale x.pop by spec
    x.y.cr <- x.false/x.cr

    x.fa <- (1 - (n.cr/n.false)) * x.pop # scale x.pop by (1 - spec)
    x.y.fa <- x.false/x.fa


    if (!all.equal((x.true * x.pop), sum((x.hi * x.true), (x.mi * x.true)))) {
      warning("rectree 2: Sum of HI + MI area differs from cond True area.")
    }
    if (!all.equal((x.false * x.pop), sum((x.fa * x.false), (x.cr * x.false)))) {
      warning("rectree 3: Sum of FA + CR area differs from cond False area.")
    }
    if (!all.equal((x.pop^2), sum((x.hi * x.true), (x.mi * x.true), (x.fa * x.false), (x.cr * x.false)))) {
      warning("rectree 4: Population area differs from the area sum of all 4 SDT cases.")
    }

    x.boxes <- c(x.pop, x.true, x.false, x.hi, x.mi, x.fa, x.cr) # specific widths
    x.y.prop <- c(x.y.pop, x.y.true, x.y.false, x.y.hi, x.y.mi, x.y.fa, x.y.cr) # specific proportions
  }

  ## Plot matrix M (from diagram package):
  pp <- diagram::plotmat(M, # square coefficient matrix, specifying the links (rows = to, cols = from)
                         pos = c(1, 2, 4),
                         curve = 0.0, # no curve (> 0 curve left, < 0 curve right)
                         name = names,
                         relsize	= .98, # a scaling factor for the size of the graph
                         lwd = 1.5,
                         ## Boxes:
                         box.size = x.boxes,  # widths of boxes
                         box.prop = x.y.prop, # proportionality (length/width) ratio of boxes
                         box.type = "rect", # "ellipse", "diamond", "circle", "hexa", "multi", "none"
                         box.col = col.boxes, # scalar or vector of length 7.
                         # c(col.N, col.true, col.false, col.hi, col.mi, col.fa, col.cr), # WAS: "lightyellow"
                         box.lcol = col.border,
                         box.lwd = 2.0,
                         lcol = col.border, # default color for box and arrow lines
                         ## Text in Boxes:
                         txt.col = col.txt,
                         box.cex = .85, # relative size of text in boxes
                         txt.font = 1, # 1 = plain, 2 = bold, ...
                         ## Arrows:
                         cex.txt = .80, # relative size of arrow text
                         arr.pos = .50, # relative position of arrowhead on arrow segment/curve
                         arr.type = "triangle", # one of "curved", "triangle", "circle", "ellipse", "T", "simple"
                         arr.length = .20,
                         arr.width = .15,
                         arr.col = col.border,
                         shadow.size = cex.shadow, # .005
                         shadow.col = col.shadow #,
                         # main = paste0(title.lbl, ":\n", "Sum tree of natural frequencies (N = ", N, ")")
  )

  ## Title:
  if (box.area == "o") {type.lbl <- "Tree"}
  if (box.area == "s") {type.lbl <- "Area (square) tree"}
  if (box.area == "r") {type.lbl <- "Area (rectangle) tree"}
  cur.title.lbl <- paste0(title.lbl, ":\n", type.lbl, " of natural frequencies") # , "(N = ", N, ")")
  title(cur.title.lbl, adj = 0.5, line = -0.5, font.main = 1) # (left, lowered, normal font)

  ## Margin text:
  cur.par.lbl <-  paste0("(", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%)")
  mtext(cur.par.lbl, side = 1, line = 1, adj = 1, col = grey(.33, .99), cex = .90)

  # return(pp) # returns elements of diagram object

}

## Check:
# plot_nftree(prev = .20, sens = .75, spec = .90, N = 100, box.area = "r")
# plot_nftree(box.area = "s", col.boxes = "gold", col.border = "steelblue4", col.shadow = "steelblue4", cex.shadow = .008)
# plot_nftree(box.area = "r", col.txt = "steelblue4", col.boxes = "lightyellow", col.border = "steelblue4", cex.shadow = .005, col.shadow = "black")

## -----------------------------------------------
## (+) ToDo:

## - 1. provide more info on current numeric inputs (prev, sens, spec, fart) on edges
## - 2. Make version with options for
##         a - providing fart rather than spec
##         b - freq rather than prev, sens, spec
## - 3. make text color adjustable (using col.txt)
## - 4. pimp plot (labels, colors, transparency)

## -----------------------------------------------
## eof.
