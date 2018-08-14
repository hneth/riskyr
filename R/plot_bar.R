## plot_bar.R | riskyr
## 2018 08 14
## -----------------------------------------------
## Plot bar (a family of) charts that express freq as area
## (size and proportion)
## from 3 essential probabilities (prev, sens, spec)
## or current population data.frame popu.

## (Using only base graphics.)
## -----------------------------------------------

## Helper function: Plot a box and its text labels ----------
plot_box <- function(category, name,   # category and name (as character)
                     freq,             # frequency (as number)
                     box.x,  box.y,    # coordinates x and y
                     box.lx, box.ly,   # lengths of box width and height
                     show.freq = TRUE  # option to show frequency labels
) {

  ## Box parameters:
  box.lwd <- 1  # line width of border around rect (default = 1)

  ## Text parameters:
  # col.lbl <- pal["txt"]  # defined in pal
  cex.lbl <- .90   # scaling factor for text labels
  cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  h.shift <- .05   # horizontal shifting of labels
  v.shift <- .05   # vertical shifting of labels

  ## Plot rect:
  rect(xleft  = (box.x - box.lx/2), ybottom = box.y,
       xright = (box.x + box.lx/2), ytop    = (box.y + box.ly),
       col = pal[name], border = pal["brd"], lwd = box.lwd)

  ## Plot box title (on baseline):
  text(x = box.x, y = box.y,
       labels = category, pos = 1,
       xpd = TRUE, col = pal["txt"], cex = cex.lbl.sm)

  ## Plot box label (centered):
  if (show.freq) {
    mid.y <- box.y + box.ly/2  # y-value of mid point

    text(x = box.x, y = mid.y,
         labels = paste0(name, " = ", freq), # pos = 3,
         xpd = TRUE, col = pal["txt"], cex = cex.lbl.sm)
  }

}


#' Plot bar charts of population frequencies.
#'
#' \code{plot_bar} draws bar charts that
#' represent the proportions of frequencies in the current
#' population \code{\link{popu}} as relatives sizes of
#' rectangular areas.
#'
#' If a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' is provided, new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch.  Otherwise, the existing
#' population \code{\link{popu}} is shown.
#'
#' Rectangles corresponding to the areas of the mosaic plot
#' can be visualized byopting for vertical rectangles (by selecting
#' the option \code{box = "vr"}) in \code{\link{plot_tree}}
#' and \code{\link{plot_fnet}}.
#'
#'
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{sens} is provided.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param N The number of individuals in the population.
#' (This value is not represented in the plot,
#' but used when new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch from current probabilities.)
#'
#' @param by A character code specifying the perspective
#' (or 1st category by which the population is split into subsets)
#' with 2 options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition (vertical split first);
#'   \item \code{"dc"} ... by decision (horizontal split first).
#'   }
#'
#' @param show.freq Boolean option for showing frequencies
#' (i.e., of \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, and
#' \code{\link{cr}}) in the plot.
#' Default: \code{show.freq = FALSE}.
#'
#' @param show.prob Boolean option for showing visual help lines to mark
#' generating metrics (e.g., \code{\link{prev}}, \code{\link{sens}}, and
#' \code{\link{spec}}) in the plot.
#' Default: \code{show.prob = FALSE}.
#'
#' @param show.accu Option for showing current
#' accuracy metrics \code{\link{accu}} in the plot.
#' Default: \code{show.accu = TRUE}.
#'
#' @param w.acc Weigthing parameter \code{w} used to compute
#' weighted accuracy \code{w.acc} in \code{\link{comp_accu}}.
#' Default: \code{w.acc = .50}.
#'
#' @param title.lbl Text label for current plot title.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param col.pal Color palette.
#' Default: \code{col.pal = pal} (see \code{\link{pal}} and \code{\link{init_pal}}).
#'
#' @examples
#'
#' # Basics:
#' plot_bar()  # => plot with default options
#' plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test 1")
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .66, title.lbl = "Test 2", show.freq = TRUE)
#'
#' # Note:
#' plot_bar(N = 3, prev = .33, sens = .75, spec = .66,
#'          title.lbl = "Test rounding effects",
#'          show.freq = TRUE)  # => Rounding of freq, but not of proportions shown in plot.
#'
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics box
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics abline
#' @importFrom graphics rect
#' @importFrom graphics arrows
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{comp_popu}} computes the current population;
#' \code{\link{popu}} contains the current population;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings
#'
#' @export

plot_bar <- function(prev = num$prev,             # probabilities
                     sens = num$sens, mirt = NA,
                     spec = num$spec, fart = NA,
                     N = num$N,  # not needed in Mosaic plot (but used in comp_freq below)
                     ## Options:
                     by = "all",  # "cd"...condition 1st vs. "dc"...decision 1st
                     show.freq = FALSE,  # show labels of frequencies in plot
                     show.prob = FALSE,  # show help_line (for metrics, e.g., prev, sens, spec)?
                     show.accu = TRUE,   # compute and show accuracy metrics
                     w.acc = .50,        # weight w for wacc (from 0 to 1)
                     ## Text and color options: ##
                     title.lbl = txt$scen.lbl,
                     col.pal = pal
) {

  ## (0) Handle arguments and deprecated arguments: ----------

  # ## (0) Get probabilities from global numeric parameters (num):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec

  ## Currently fixed parameters:
  gap <- 2.0/100  # width of gap between 2 main subgroups (direction set via "by" argument)
  show.prob.comp <- TRUE  # show help_line for complements of prob (e.g, prev, sens, spec)?

  ## (1) Compute or use current popu: ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur.freq and popu based on current parameters (N and probabilities):
    cur.freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = TRUE)  # compute cur.freq (with round = TRUE)

    ## Assign (only needed) elements of cur.freq:
    n.hi  <- cur.freq$hi
    n.mi  <- cur.freq$mi
    n.fa  <- cur.freq$fa
    n.cr  <- cur.freq$cr

    ## (c) Compute cur.popu from computed frequencies:
    cur.popu <- comp_popu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr)  # compute cur.popu (from 4 essential frequencies)

    # warning("Generated new population (cur.popu) to draw mosaic plot...")


  } else {  # (B) NO valid set of probabilities was provided:

    ## Use the current popu:
    cur.popu <- popu

    # warning("Using existing population (popu) to draw mosaic plot...")

  } # if (is_valid_prob_set...)

  ## (2) Text labels: ----------

  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Custom bar plot") # , "(N = ", N, ")")

  cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  # cur.dec.lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
  cur.par.lbl <- cur.cond.lbl

  ## (3) Accuracy: ----------

  if (show.accu) {
    cur.accu <- comp_accu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr, w = w.acc)  # compute accuracy info
    cur.accu.lbl <- make_accu_lbl(acc = cur.accu$acc, w = w.acc, wacc = cur.accu$wacc, mcc = cur.accu$mcc)  # use utility function

    # mtext(cur.accu.lbl, side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = .85)
    cur.par.lbl <- paste0(cur.par.lbl, "\n", cur.accu.lbl, "\n")  # add accuracy lbl to existing cur.par.lbl
  }

  ## (4) Define plot and margin areas: ----------

  ## Margin areas:
  par(oma = c(3, 2, 1, 1) + 0.1)  # outer margins: bottom has 3 lines of space
  par(mar = c(4, 2, 4, 2) + 0.1)  # margin: default: c(5.1, 4.1, 4.1, 2.1)

  ## Graphical parameters:

  ## Color info (defined in init_pal):
  # col.prev <- prev.li  # prev help line
  # col.sens <- sens.li  # sens help line
  # col.spec <- spec.li  # spec help line
  # col.bord <- grey(.20, .99) # NA # grey(.11, .99)  # borders and labels (NA removes borders)

  ## Box parameters:
  box.lwd <- 1  # line width of border around rect (default = 1)

  ## Point appearance:
  pt.pch <- 21    # pch symbol of points
  pt.cex <- 1.4   # cex scaling of points
  pt.lwd <- 1.6   # lwd of point borders

  ## Text labels:
  # col.lbl <- pal["txt"]  # defined in pal
  cex.lbl <- .90   # scaling factor for text labels
  cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  h.shift <- .05   # horizontal shifting of labels
  v.shift <- .05   # vertical shifting of labels

  # help line properties (main metrics):
  lty.help <- 1    # line type
  lwd.help <- 2.5  # line width

  ## Custom bar plot: ----------

  if (by == "all") {

    ## Plot dimensions:
    xlim = c(0, 1)
    ylim = c(0, N)

    ## Plot area setup:
    plot(x = 1,
         xlim = xlim, ylim = ylim,
         type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         bty = "n",
         fg = grey(.50, alpha = .99)
    )

    ## Mark plot and margin area:
    col.plot <- "grey80"
    # box("plot", col = col.plot)
    mar.col <- "grey60"
    # box("figure", col = mar.col)

    ## Axes:
    # axis(side = 1)
    axis(side = 2, las = 2) # y-axis

    ## Grid:
    grid(nx = NA, ny = NULL,
         col = grey(.75, .99), lty = 1,
         lwd = par("lwd"), equilogs = TRUE)

    ## Size parameters:
    lbase <- N    # length of base side
    lelse <- .10  # length of other side (free parameter)
    scale <- 1.0  # scaling factor (0-1)


    ## A: vertical bars: ----------

    ## Dimensions of boxes: ----
    ## (lengths of x- and y-side):

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    # Number and basic width of columns:
    nr.col <- 5         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Offset from base line:
    x.base <- 0  # offset x
    y.base <- 0  # offset y

    # # N:
    # n.ly <- b.ly
    # n.lx <- b.lx
    #
    # # SDT (hi, mi, fa, cr):
    # hi.ly <- (n.ly * prev) * sens              # recomputes n.hi
    # mi.ly <- (n.ly * prev) * (1 - sens)        # recomputes n.mi
    # cr.ly <- (n.ly * (1 - prev)) * spec
    # fa.ly <- (n.ly * (1 - prev)) * (1 - spec)
    #
    # hi.lx <- b.lx
    # mi.lx <- b.lx
    # cr.lx <- b.lx
    # fa.lx <- b.lx
    #
    # # cond.true and cond.false:
    # cond.true.ly  <- (hi.ly + mi.ly)
    # cond.false.ly <- (fa.ly + cr.ly)
    #
    # cond.true.lx  <- b.lx
    # cond.false.lx <- b.lx
    #
    # # dec.pos and dec.neg:
    # dec.pos.ly <- (hi.ly + fa.ly)
    # dec.neg.ly <- (mi.ly + cr.ly)
    #
    # dec.pos.lx <- b.lx
    # dec.neg.lx <- b.lx
    #
    # # dec.cor and dec.err:
    # dec.cor.ly <- (hi.ly + cr.ly)
    # dec.err.ly <- (mi.ly + fa.ly)
    #
    # dec.cor.lx <- b.lx
    # dec.err.lx <- b.lx


    ## Positions of boxes: ----
    ## (x- and y-coordinate of left bottom corner):

    ## N column:
    # col.nr <- 1
    # n.x <- x.base + (col.nr * col.x) - (col.x/2)  # column mid point
    # n.y <- y.base

    ## cond.true vs. cond.false column:
    # col.nr <- 2
    # cond.true.x <- x.base + (col.nr * col.x) - (col.x/2) - n.lx/2  # column mid point - object width/2
    # cond.false.x <- cond.true.x

    # cond.true.y <- y.base
    # cond.false.y <- cond.true.y + cond.true.ly

    # # dec.pos vs. dec.neg column:
    # col.nr <- 4
    # dec.pos.x <- x.base + (col.nr * col.x) - (col.x/2) - n.lx/2  # column mid point - object width/2
    # dec.neg.x <- dec.pos.x
    #
    # dec.pos.y <- y.base
    # dec.neg.y <- dec.pos.y + dec.pos.ly
    #
    # # dec.cor vs. dec.err column:
    # col.nr <- 5
    # dec.cor.x <- x.base + (col.nr * col.x) - (col.x/2) - n.lx/2  # column mid point - object width/2
    # dec.err.x <- dec.cor.x
    #
    # dec.cor.y <- y.base
    # dec.err.y <- dec.cor.y + dec.cor.ly


    ## Draw rectangles: ------

    # (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate

    # Plot 1 box:
    plot_box(category = "popu", name = "N", freq = N,
             box.x  = n.x,
             box.y  = n.y,
             box.lx = b.lx,
             box.ly = n.ly,
             show.freq)

    ## (b) SDT column: ----

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # y-coordinates:
    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    # Plot 4 boxes:
    plot_box(category = "cell", name = "hi", freq = n.hi,
             box.x  = hi.x,
             box.y  = hi.y,
             box.lx = b.lx,
             box.ly = hi.ly,
             show.freq)

    plot_box(category = NULL, name = "mi", freq = n.mi,
             box.x  = mi.x,
             box.y  = mi.y,
             box.lx = b.lx,
             box.ly = mi.ly,
             show.freq)

    plot_box(category = NULL, name = "fa", freq = n.fa,
             box.x  = fa.x,
             box.y  = fa.y,
             box.lx = b.lx,
             box.ly = fa.ly,
             show.freq)

    plot_box(category = NULL, name = "cr", freq = n.cr,
             box.x  = cr.x,
             box.y  = cr.y,
             box.lx = b.lx,
             box.ly = cr.ly,
             show.freq)

    ## (c) Condition column: ----

    # heights:
    cond.true.ly  <- (n.ly * prev)        # re-computes cond.true (without rounding)
    cond.false.ly <- (n.ly * (1 - prev))  # re-computes cond.false (without rounding)

    # x-coordinates:
    col.nr <- 2
    cond.true.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    cond.false.x <- cond.true.x

    # y-coordinates:
    cond.true.y <- y.base
    cond.false.y <- cond.true.y + cond.true.ly

    # Plot 2 boxes:
    plot_box(category = "cond", name = "true", freq = (n.hi + n.mi),
             box.x  = cond.true.x,
             box.y  = cond.true.y,
             box.lx = b.lx,
             box.ly = cond.true.ly,
             show.freq)
    plot_box(category = NA, name = "false", freq = (n.fa + n.cr),
             box.x  = cond.false.x,
             box.y  = cond.false.y,
             box.lx = b.lx,
             box.ly = cond.false.ly,
             show.freq)

    ## (d) Decision column: ----

    # heights:
    dec.pos.ly <- hi.ly + fa.ly
    dec.neg.ly <- mi.ly + cr.ly

    # x-coordinates:
    col.nr <- 4
    dec.pos.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.neg.x <- dec.pos.x

    # y-coordinates:
    dec.pos.y <- y.base
    dec.neg.y <- dec.pos.y + dec.pos.ly

    # Plot 2 boxes:
    plot_box(category = "dec", name = "pos", freq = (n.hi + n.fa),
             box.x  = dec.pos.x,
             box.y  = dec.pos.y,
             box.lx = b.lx,
             box.ly = dec.pos.ly,
             show.freq)
    plot_box(category = NA, name = "neg", freq = (n.mi + n.cr),
             box.x  = dec.neg.x,
             box.y  = dec.neg.y,
             box.lx = b.lx,
             box.ly = dec.neg.ly,
             show.freq)


    ## (e) Accuracy column: ----

    # heights:
    dec.cor.ly <- hi.ly + cr.ly
    dec.err.ly <- mi.ly + fa.ly

    # x-coordinates:
    col.nr <- 5
    dec.cor.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.err.x <- dec.cor.x

    # y-coordinates:
    dec.cor.y <- y.base
    dec.err.y <- dec.cor.y + dec.cor.ly

    # Plot 2 boxes:
    plot_box(category = "accu", name = "cor", freq = (n.hi + n.cr),
             box.x  = dec.cor.x,
             box.y  = dec.cor.y,
             box.lx = b.lx,
             box.ly = dec.cor.ly,
             show.freq)
    plot_box(category = NA, name = "err", freq = (n.mi + n.fa),
             box.x  = dec.err.x,
             box.y  = dec.err.y,
             box.lx = b.lx,
             box.ly = dec.err.ly,
             show.freq)


    ## +++ here now +++ ##

    # # dec.pos vs. dec.neg column:
    # rect(xleft = dec.pos.x, ybottom = dec.pos.y,
    #      xright = dec.pos.x + dec.pos.lx, ytop = dec.pos.y + dec.pos.ly,
    #      col = pal["pos"], border = pal["brd"], lwd = box.lwd)  # dec.pos
    # rect(xleft = dec.neg.x, ybottom = dec.neg.y,
    #      xright = dec.neg.x + dec.neg.lx, ytop = dec.neg.y + dec.neg.ly,
    #      col = pal["neg"], border = pal["brd"], lwd = box.lwd)  # dec.neg

    # # dec.cor vs. dec.err column:
    # rect(xleft = dec.cor.x, ybottom = dec.cor.y,
    #      xright = dec.cor.x + dec.cor.lx, ytop = dec.cor.y + dec.cor.ly,
    #      col = pal["cor"], border = pal["brd"], lwd = box.lwd)  # dec.cor
    # rect(xleft = dec.err.x, ybottom = dec.err.y,
    #      xright = dec.err.x + dec.err.lx, ytop = dec.err.y + dec.err.ly,
    #      col = pal["err"], border = pal["brd"], lwd = box.lwd)  # dec.err






  }

  else if (by == "dc") {

    ## Using bar plot: ----------

    ## Plot dimensions:
    xlim = c(0, (5 + gap))
    ylim = c(0, 100)

    ftab <- cbind(c(N, 0, 0, 0), # N
                  c(n.hi + n.mi, 0, n.fa + n.cr, 0), # by condition
                  c(n.hi,  n.mi, n.fa,  n.cr),       # 4 sdt categories
                  c(n.hi + n.fa, 0, n.mi + n.cr, 0)  # by decision
    )
    colnames(ftab) <- c("N", "by cd", "sdt", "by dc")
    rownames(ftab) <- c("hi", "mi", "fa", "cr")

    # barplot(ftab,
    #         # main = "Plot title",
    #         xlab = "x-axis label",
    #         ylab = "y-axis label",
    #         col = col.sdt,
    #         legend = rev(rownames(ftab)),
    #         add = TRUE
    #)


  } # if (by == ...)

  ## Title and margin text:
  # title(cur.title.lbl, adj = 0.5, line = 1.5, font.main = 1) # (centered, raised, normal font)
  title(cur.title.lbl, adj = 0.0, line = 1.5, font.main = 1) # (left, raised, normal font)

}




## Check:
{
  ## Basics:
  # plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test plot")
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, title.lbl = "Test plot", show.freq = TRUE)

  ## Note:
  # plot_bar(N = 3, prev = .33, sens = .75, spec = .66,
  #          title.lbl = "Test rounding effects",
  #          show.freq = TRUE)  # => Rounding of freq, but not of proportions shown in plot.
}

## -----------------------------------------------
## (+) ToDo:

## - scale 1 dimension by N (and add axis)
## - add area labels (in center of area)
## - add probabilitiy indicators (arrows and labels)
## - alternative arrangements: horizontal (flip coord), dodged bars, ...
## - ...

## -----------------------------------------------
## eof.
