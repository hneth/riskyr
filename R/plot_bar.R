## plot_bar.R | riskyr
## 2018 08 14
## -----------------------------------------------
## Plot bar (a family of) charts that express freq as area
## (size and proportion)
## from 3 essential probabilities (prev, sens, spec)
## or current population data.frame popu.

## (Using only base graphics.)
## -----------------------------------------------

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
#' # Basics:
#' plot_bar()  # => default options
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
                     by = "cd",  # "cd"...condition 1st vs. "dc"...decision 1st
                     show.freq = FALSE,  # show labels of 4 frequencies in plot
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

  ## Plot dimensions:
  xlim = c(0, 1)
  ylim = c(0, 1)

  ## Plot area setup:
  plot(x = 1,
       xlim = xlim, ylim = ylim,
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "n",
       fg = grey(.50, alpha = .99)
  )

  ## Mark plot and margin area:
  col.plot <- "grey80"
  box("plot", col = col.plot)
  mar.col <- "grey60"
  # box("figure", col = mar.col)

  ## Graphical parameters:
  col.prev <- prev.li  # prev help line
  col.sens <- sens.li  # sens help line
  col.spec <- spec.li  # spec help line
  col.bord <- grey(.20, .99) # NA # grey(.11, .99)  # borders and labels (NA removes borders)

  ## Point appearance:
  pt.pch <- 21    # pch symbol of points
  pt.cex <- 1.4   # cex scaling of points
  pt.lwd <- 1.6   # lwd of point borders

  ## Text labels:
  col.lbl <- grey(.11, .99)
  cex.lbl <- .90   # scaling factor for text labels
  cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  h.shift <- .05   # horizontal shifting of labels
  v.shift <- .05   # vertical shifting of labels

  # help line properties (main metrics):
  lty.help <- 1    # line type
  lwd.help <- 2.5  # line width

  ## Custom bar plot: ----------

  if (by == "cd") {

    ## Size parameters:
    lbase <- 1.0  # length of base side
    lelse <- .10  # length of other side (free parameter)
    scale <- 1.0  # scaling factor (0-1)

    ## A: vertical orientation: ----

    ## Dimensions of boxes: ----
    ## (lengths of x- and y-side):

    # N:
    n.ly <- lbase * scale  # basic length (scaled constant)
    n.lx <- lelse * scale  # basic width (scaled constant)

    # cond.true and cond.false:
    cond.true.ly  <- n.ly * prev
    cond.false.ly <- 1 - cond.true.ly
    cond.true.lx  <- n.lx
    cond.false.lx <- n.lx

    # SDT (hi, mi, fa, cr):
    hi.ly <- cond.true.ly * sens
    mi.ly <- cond.true.ly - hi.ly
    cr.ly <- cond.false.ly * spec
    fa.ly <- cond.false.ly - cr.ly

    hi.lx <- n.lx
    mi.lx <- n.lx
    cr.lx <- n.lx
    fa.lx <- n.lx

    # dec.pos and dec.neg:
    dec.pos.ly <- hi.ly + fa.ly
    dec.neg.ly <- 1 - dec.pos.ly

    dec.pos.lx <- n.lx
    dec.neg.lx <- n.lx

    # dec.cor and dec.err:
    dec.cor.ly <- hi.ly + cr.ly
    dec.err.ly <- 1 - dec.cor.ly

    dec.cor.lx <- n.lx
    dec.err.lx <- n.lx


    ## Positions of boxes: ----
    ## (x- and y-coordinate of left bottom corner):

    n.col <- 5        # number of (vertical) columns
    col.x <- 1/n.col  # corresponding column width

    x.base <- 0  # offset x
    y.base <- 0  # offset y

    # N column:
    col.nr <- 1
    n.x <- x.base + (col.nr * col.x) - (col.x/2) - n.lx/2  # column mid point - object width/2
    n.y <- y.base

    # cond.true vs. cond.false column:
    col.nr <- 2
    cond.true.x <- x.base + (col.nr * col.x) - (col.x/2) - n.lx/2  # column mid point - object width/2
    cond.false.x <- cond.true.x

    cond.true.y <- y.base
    cond.false.y <- cond.true.y + cond.true.ly

    # SDT column:
    col.nr <- 3
    hi.x <- x.base + (col.nr * col.x) - (col.x/2) - hi.lx/2  # column mid point - object width/2
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    # dec.pos vs. dec.neg column:
    col.nr <- 4
    dec.pos.x <- x.base + (col.nr * col.x) - (col.x/2) - n.lx/2  # column mid point - object width/2
    dec.neg.x <- dec.pos.x

    dec.pos.y <- y.base
    dec.neg.y <- dec.pos.y + dec.pos.ly

    # dec.cor vs. dec.err column:
    col.nr <- 5
    dec.cor.x <- x.base + (col.nr * col.x) - (col.x/2) - n.lx/2  # column mid point - object width/2
    dec.err.x <- dec.cor.x

    dec.cor.y <- y.base
    dec.err.y <- dec.cor.y + dec.cor.ly

    ## Draw rectangles: ----

    # N column:
    rect(xleft = n.x, ybottom = n.y, xright = n.x + n.lx, ytop = n.y + n.ly,
         col = pal["N"], border = col.bord)  # N

    # cond.true vs. cond.false column:
    rect(xleft = cond.true.x, ybottom = cond.true.y,
         xright = cond.true.x + cond.true.lx, ytop = cond.true.y + cond.true.ly,
         col = pal["true"], border = col.bord)  # cond.true
    rect(xleft = cond.false.x, ybottom = cond.false.y,
         xright = cond.false.x + cond.false.lx, ytop = cond.false.y + cond.false.ly,
         col = pal["false"], border = col.bord)  # cond.false

    # SDT (hi, mi, fa, cr) column:
    rect(xleft = hi.x, ybottom = hi.y, xright = hi.x + hi.lx, ytop = hi.y + hi.ly,
         col = pal["hi"], border = col.bord)  # hi
    rect(xleft = mi.x, ybottom = mi.y, xright = mi.x + mi.lx, ytop = mi.y + mi.ly,
         col = pal["mi"], border = col.bord)  # mi
    rect(xleft = fa.x, ybottom = fa.y, xright = fa.x + fa.lx, ytop = fa.y + fa.ly,
         col = pal["fa"], border = col.bord)  # fa
    rect(xleft = cr.x, ybottom = cr.y, xright = cr.x + cr.lx, ytop = cr.y + cr.ly,
         col = pal["cr"], border = col.bord)  # cr

    # dec.pos vs. dec.neg column:
    rect(xleft = dec.pos.x, ybottom = dec.pos.y,
         xright = dec.pos.x + dec.pos.lx, ytop = dec.pos.y + dec.pos.ly,
         col = pal["pos"], border = col.bord)  # dec.pos
    rect(xleft = dec.neg.x, ybottom = dec.neg.y,
         xright = dec.neg.x + dec.neg.lx, ytop = dec.neg.y + dec.neg.ly,
         col = pal["neg"], border = col.bord)  # dec.neg

    # dec.cor vs. dec.err column:
    rect(xleft = dec.cor.x, ybottom = dec.cor.y,
         xright = dec.cor.x + dec.cor.lx, ytop = dec.cor.y + dec.cor.ly,
         col = pal["cor"], border = col.bord)  # dec.cor
    rect(xleft = dec.err.x, ybottom = dec.err.y,
         xright = dec.err.x + dec.err.lx, ytop = dec.err.y + dec.err.ly,
         col = pal["err"], border = col.bord)  # dec.err



    ## +++ here now +++ ##


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

    barplot(ftab,
            # main = "Plot title",
            xlab = "x-axis label",
            ylab = "y-axis label",
            col = col.sdt,
            legend = rev(rownames(ftab)),
            add = TRUE
    )


  } # if (by == ...)

  ## Title and margin text:
  # title(cur.title.lbl, adj = 0.5, line = 1.5, font.main = 1) # (centered, raised, normal font)
  title(cur.title.lbl, adj = 0.0, line = 1.5, font.main = 1) # (left, raised, normal font)

}

## Check:
{
  plot_bar(prev = .33, sens = .75, spec = .60)
  # plot_bar(title.lbl = "")

}


## -----------------------------------------------
## (+) ToDo:

## - add labels for (prev, sens, spec), (ppod, PPV, NPV), bacc.
## - add Decisions panel.
## - consider adding 3rd perspective: (c) by correspondence (accu)
## - make mosaic plot dependent on basic parameters
##   (i.e., compute comp_popu() from probabilities and N,
##    rather than providing it as input)?
## - add a simpler version that only shows cond.true vs. cond.false
## - adjust parameters (zero size and gap width)
## - add labels (frequencies) to plot?

## -----------------------------------------------
## eof.
