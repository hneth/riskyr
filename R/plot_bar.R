## plot_bar.R | riskyr
## 2018 08 31
## -----------------------------------------------

## Plot bar (a family of) charts that express freq types as lengths ------
## (size and proportion) from 3 essential probabilities (prev, sens, spec),
## or current population data.frame popu.

## plot_bar: Documentation ---------

## Note: plot_bar computes 4 essential frequencies,
##       but does NOT update global freq and prob objects.
##       It is NOT "smart" by NOT automatically deriving
##       freq and prob labels from global objects!

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
#' (or the dimension by which the population is split into 2 subsets)
#' with the following options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition;
#'   \item \code{"dc"} ... by decision;
#'   \item \code{"ac"} ... by accuracy;
#'   \item \code{"all"} ... shows all subdivisions.
#'   }
#'
#' @param dir Number of directions of bars. Options:
#' \enumerate{
#'   \item \code{1} ... uni-directional bars;
#'   \item \code{2} ... bi-directional (positive vs. negative) bars;
#'   }
#'
#' @param show.freq Boolean option for showing frequencies
#' (i.e., of \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, and
#' \code{\link{cr}}) in the plot.
#' Default: \code{show.freq = TRUE}.
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
#' weighted accuracy \code{w.acc} in \code{\link{comp_accu_freq}}.
#' Default: \code{w.acc = .50}.
#'
#' @param title.lbl Text label for current plot title.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param col.pal Color palette.
#' Default: \code{col.pal = pal} (see \code{\link{pal}} and \code{\link{init_pal}}).
#'
#' @param ... Other (graphical) parameters
#' (e.g., \code{cex}, \code{lwd}, ...).
#'
#' @examples
#' # Basics:
#' plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test 1")
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60,
#'          title.lbl = "Test 2")  # by "all" (default)
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd",
#'          title.lbl = "Test 3a")  # by condition
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", dir = 2,
#'          title.lbl = "Test 3b")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc",
#'          title.lbl = "Test 4a")  # by decision
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", dir = 2,
#'          title.lbl = "Test 4b")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac",
#'          title.lbl = "Test 5a")  # by accuracy
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", dir = 2,
#'          title.lbl = "Test 5b")  # bi-directional
#'
#' # Note:
#' plot_bar(N = 3, prev = .33, sens = .75, spec = .66,
#'          title.lbl = "Test rounding effects",
#'          show.freq = TRUE)  # => Rounding of freq, but not of proportions shown in plot.
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
#' @importFrom graphics barplot
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

## plot_bar: Definition ---------

plot_bar <- function(prev = num$prev,             # probabilities
                     sens = num$sens, mirt = NA,
                     spec = num$spec, fart = NA,
                     N = num$N,                   # population size N
                     ## Options:
                     by = "all",  # "cd"...condition, "dc"...decision; "all".
                     dir = 1,     # directions: 1 vs. 2
                     show.freq = TRUE,   # show labels of frequencies in plot
                     show.prob = FALSE,  # show help_line (for metrics, e.g., prev, sens, spec)?
                     show.accu = TRUE,   # compute and show accuracy metrics
                     w.acc = .50,        # weight w for wacc (from 0 to 1)
                     ## Text and color options: ##
                     title.lbl = txt$scen.lbl,
                     col.pal = pal,      # color palette
                     ...  # other (graphical) parameters: lwd, cex, ...
) {

  ## (0) Handle arguments and deprecated arguments: ----------

  # ## (0) Get probabilities from global numeric parameters (num):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec

  ## (1) Compute or use current popu: ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute freq based on current parameters (N and probabilities):
    freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = TRUE)  # compute freq (with round = TRUE)

    ## ToDo: Update global freq and prob objects
    ##       to use label_freq and label_prob functions.

    ## Assign (only needed) elements based on freq:
    n.hi  <- freq$hi
    n.mi  <- freq$mi
    n.fa  <- freq$fa
    n.cr  <- freq$cr

    ## (c) Compute cur.popu from computed frequencies:
    # cur.popu <- comp_popu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr)  # compute cur.popu (from 4 essential frequencies)

    ## warning("Generated new population (cur.popu) to plot...")

  } else {  # (B) NO valid set of probabilities was provided:

    ## Use the current popu:
    # cur.popu <- popu

    ## warning("Using existing population (popu) to plot...")

  } # if (is_valid_prob_set...)

  ## (2) Text labels: ----------

  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Custom bar plot") # , "(N = ", N, ")")

  # cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  # # cur.dec.lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
  # cur.par.lbl <- cur.cond.lbl
  #
  # type_label <- "freq type"  # to be derived below.

  ## (3) Accuracy: ----------

  if (show.accu) {
    cur.accu <- comp_accu_freq(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr, w = w.acc)  # compute accuracy info from (rounded) freq
    cur.accu.lbl <- make_accu_lbl(acc = cur.accu$acc, w = w.acc, wacc = cur.accu$wacc, mcc = cur.accu$mcc)  # use utility function

    # mtext(cur.accu.lbl, side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = .85)
    # cur.par.lbl <- paste0(cur.par.lbl, "\n", cur.accu.lbl, "\n")  # add accuracy lbl to existing cur.par.lbl
  }

  ## (4) Define plot and margin areas: ----------

  ## Margin areas:
  par(oma = c(3, 2, 1, 1) + 0.1)  # outer margins: bottom has 3 lines of space
  par(mar = c(4, 2, 4, 2) + 0.1)  # margin: default: c(5.1, 4.1, 4.1, 2.1)

  ## (5) Graphical parameters: ----

  # Offset from base line:
  x.base <- 0  # offset x
  y.base <- 0  # offset y

  ## Color info (NOW defined in init_pal):
  # col.prev <- prev.li  # prev help line
  # col.sens <- sens.li  # sens help line
  # col.spec <- spec.li  # spec help line
  # col.bord <- grey(.20, .99) # NA # grey(.11, .99)  # borders and labels (NA removes borders)

  ## Currently fixed parameters:
  # gap <- 2.0/100  # width of gap between 2 main subgroups (direction set via "by" argument)
  # show.prob.comp <- TRUE  # show help_line for complements of prob (e.g, prev, sens, spec)?

  ## Box appearance:
  # box.lwd <- 1  # line width of border around rect box (default = 1)

  ## Point appearance:
  # pt.pch <- 21    # pch symbol of points
  # pt.cex <- 1.4   # cex scaling of points
  # pt.lwd <- 1.6   # lwd of point borders

  ## Text label appearance:
  # col.lbl <- pal["txt"]  # defined in pal
  # cex.lbl <- .90   # scaling factor for text labels
  # cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  # h.shift <- .05   # horizontal shifting of labels
  # v.shift <- .05   # vertical shifting of labels

  ## help line properties (main metrics):
  # lty.help <- 1    # line type
  # lwd.help <- 2.5  # line width


  ## (6) Define plot area: ----------

  ## Plot dimensions:
  xlim = c(0, 1)

  if (dir == 1) {
    y.min <- 0
  } else if (dir == 2) {
    y.min <- -N
  }
  ylim = c(y.min, N)

  ## Plot area setup:
  plot(x = 1,
       xlim = xlim, ylim = ylim,
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "n",
       fg = grey(.50, alpha = .99)
  )

  ## Mark plot and margin area:
  # col.plot <- "grey80"
  # box("plot", col = col.plot)
  # mar.col <- "grey60"
  # box("figure", col = mar.col)

  ## Axes:
  # axis(side = 1, las = 1) # x-axis, horizontal labels
  axis(side = 2, las = 2) # y-axis, horizontal labels

  ## Grid:
  grid(nx = NA, ny = NULL,  # y-axes only (at tick marks)
       col = grey(.75, .99), lty = 1,
       lwd = par("lwd"), equilogs = TRUE)


  ## (7) Custom bar plot: ----------

  if (by == "all") {

    ## (1): 5 vertical bars: ----------

    # Number and basic width of columns:
    nr.col <- 5         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate
    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", fnum = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # # Column name: freq type
    # type_label <- paste0(comp_freq_type("N"))  # determine freq type
    # text(x = n.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)

    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"], # col = comp_freq_col("N"),
                     ...)

    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      fa.ly <- -1 * fa.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      cr.y <- y.base
      fa.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", fnum = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", fnum = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", fnum = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", fnum = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("hi"))  # determine freq type
    # text(x = hi.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Condition column: ----

    # x-coordinates:
    col.nr <- 2
    cond.true.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    cond.false.x <- cond.true.x

    # heights:
    cond.true.ly  <- (n.ly * prev)        # re-computes cond.true (without rounding)
    cond.false.ly <- (n.ly * (1 - prev))  # re-computes cond.false (without rounding)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.ly <- -1 * cond.false.ly
    }

    # y-coordinates (given heights):
    cond.true.y <- y.base
    cond.false.y <- cond.true.y + cond.true.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "cond.true", fnum = (n.hi + n.mi),
              box.x  = cond.true.x,
              box.y  = cond.true.y,
              box.lx = b.lx,
              box.ly = cond.true.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cond.false", fnum = (n.fa + n.cr),
              box.x  = cond.false.x,
              box.y  = cond.false.y,
              box.lx = b.lx,
              box.ly = cond.false.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("cond.true"))  # determine freq type
    # text(x = cond.true.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)
    plot_ftype_label("cond.true", cond.true.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond.true"),
                     ...)

    ## (d) Decision column: ----

    # x-coordinates:
    col.nr <- 4
    dec.pos.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.neg.x <- dec.pos.x

    # heights:
    dec.pos.ly <- abs(hi.ly) + abs(fa.ly)
    dec.neg.ly <- abs(mi.ly) + abs(cr.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.ly <- -1 * dec.neg.ly
    }

    # y-coordinates (given heights):
    dec.pos.y <- y.base
    dec.neg.y <- dec.pos.y + dec.pos.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.pos", fnum = (n.hi + n.fa),
              box.x  = dec.pos.x,
              box.y  = dec.pos.y,
              box.lx = b.lx,
              box.ly = dec.pos.ly,
              show.freq, ...)
    plot_vbox(type = NA, fname = "dec.neg", fnum = (n.mi + n.cr),
              box.x  = dec.neg.x,
              box.y  = dec.neg.y,
              box.lx = b.lx,
              box.ly = dec.neg.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("dec.pos"))  # determine freq type
    # text(x = dec.pos.x, y = y.min,
    #     labels = type_label, pos = 1,
    #     xpd = TRUE, col = pal["txt"],
    #     ...)
    plot_ftype_label("dec.pos", dec.pos.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.pos"),
                     ...)

    ## (e) Accuracy column: ----

    # x-coordinates:
    col.nr <- 5
    dec.cor.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.err.x <- dec.cor.x

    # heights:
    dec.cor.ly <- abs(hi.ly) + abs(cr.ly)
    dec.err.ly <- abs(mi.ly) + abs(fa.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.ly <- -1 * dec.err.ly
    }

    # y-coordinates (given heights):
    dec.cor.y <- y.base
    dec.err.y <- dec.cor.y + dec.cor.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.cor", fnum = (n.hi + n.cr),
              box.x  = dec.cor.x,
              box.y  = dec.cor.y,
              box.lx = b.lx,
              box.ly = dec.cor.ly,
              show.freq, ...)
    plot_vbox(type = NA, fname = "dec.err", fnum = (n.mi + n.fa),
              box.x  = dec.err.x,
              box.y  = dec.err.y,
              box.lx = b.lx,
              box.ly = dec.err.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("dec.cor"))  # determine freq type
    # text(x = dec.cor.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)
    plot_ftype_label("dec.cor", dec.cor.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.cor"),
                     ...)

  } # if (by == "all")

  else if (by == "cd") {

    ## (2): 3 vertical bars (condition in middle): ----------

    # Number and basic width of columns:
    nr.col <- 3         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate

    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", fnum = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # Column name: freq type
    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("N"),
                     ...)

    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      fa.ly <- -1 * fa.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      cr.y <- y.base
      fa.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", fnum = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", fnum = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", fnum = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", fnum = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    # Column name: freq type
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Condition column: ----

    # x-coordinates:
    col.nr <- 2
    cond.true.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    cond.false.x <- cond.true.x

    # heights:
    cond.true.ly  <- (n.ly * prev)        # re-computes cond.true (without rounding)
    cond.false.ly <- (n.ly * (1 - prev))  # re-computes cond.false (without rounding)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.ly <- -1 * cond.false.ly
    }

    # y-coordinates (given heights):
    cond.true.y <- y.base
    cond.false.y <- cond.true.y + cond.true.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "cond.true", fnum = (n.hi + n.mi),
              box.x  = cond.true.x,
              box.y  = cond.true.y,
              box.lx = b.lx,
              box.ly = cond.true.ly,
              show.freq, ...)
    plot_vbox(type = NA, fname = "cond.false", fnum = (n.fa + n.cr),
              box.x  = cond.false.x,
              box.y  = cond.false.y,
              box.lx = b.lx,
              box.ly = cond.false.ly,
              show.freq, ...)

    # Column name: freq type
    plot_ftype_label("cond.true", cond.true.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond.true"),
                     ...)


  } # if (by == "cd")

  else if (by == "dc") {

    ## (3): 3 vertical bars (decision in middle): ----------

    # Number and basic width of columns:
    nr.col <- 3         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate

    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", fnum = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("N"),
                     ...)


    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi.ly <- -1 * mi.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    fa.y <- hi.y + hi.ly
    cr.y <- fa.y + fa.ly
    mi.y <- cr.y + cr.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      cr.y <- y.base
      mi.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", fnum = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", fnum = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", fnum = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", fnum = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Decision column: ----

    # x-coordinates:
    col.nr <- 2
    dec.pos.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.neg.x <- dec.pos.x

    # heights:
    dec.pos.ly <- abs(hi.ly) + abs(fa.ly)
    dec.neg.ly <- abs(mi.ly) + abs(cr.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.ly <- -1 * dec.neg.ly
    }

    # y-coordinates (given heights):
    dec.pos.y <- y.base
    dec.neg.y <- dec.pos.y + dec.pos.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.pos", fnum = (n.hi + n.fa),
              box.x  = dec.pos.x,
              box.y  = dec.pos.y,
              box.lx = b.lx,
              box.ly = dec.pos.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "dec.neg", fnum = (n.mi + n.cr),
              box.x  = dec.neg.x,
              box.y  = dec.neg.y,
              box.lx = b.lx,
              box.ly = dec.neg.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("dec.pos", dec.pos.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.pos"),
                     ...)

  } # if (by == "dc")

  if (by == "ac") {

    ## (4): 3 vertical bars (accuracy in middle): ----------

    # Number and basic width of columns:
    nr.col <- 3         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate

    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", fnum = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("N"),
                     ...)


    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi.ly <- -1 * mi.ly
      fa.ly <- -1 * fa.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    cr.y <- hi.y + hi.ly
    mi.y <- cr.y + cr.ly
    fa.y <- mi.y + mi.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi.y <- y.base
      fa.y <- mi.y + mi.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", fnum = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", fnum = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", fnum = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", fnum = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Accuracy column: ----

    # x-coordinates:
    col.nr <- 2
    dec.cor.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.err.x <- dec.cor.x

    # heights:
    dec.cor.ly <- abs(hi.ly) + abs(cr.ly)
    dec.err.ly <- abs(mi.ly) + abs(fa.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.ly <- -1 * dec.err.ly
    }

    # y-coordinates (given heights):
    dec.cor.y <- y.base
    dec.err.y <- dec.cor.y + dec.cor.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.cor", fnum = (n.hi + n.cr),
              box.x  = dec.cor.x,
              box.y  = dec.cor.y,
              box.lx = b.lx,
              box.ly = dec.cor.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "dec.err", fnum = (n.mi + n.fa),
              box.x  = dec.err.x,
              box.y  = dec.err.y,
              box.lx = b.lx,
              box.ly = dec.err.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("dec.cor", dec.cor.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.cor"),
                     ...)

  } # if (by == "ac")

  else if (by == "xxx") {

    ## Using bar plot: ----------

    gap <- 0

    ## Plot dimensions:
    xlim = c(0, (5 + gap))
    ylim = c(0, N)

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
            col = rev(c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])),
            legend = rev(rownames(ftab)),
            add = FALSE
    )

  } # if (by == "xxx")


  ## (8) Title and margin text: --------

  # title(cur.title.lbl, adj = 0.5, line = 1.5, font.main = 1) # (centered, raised, normal font)
  title(cur.title.lbl, adj = 0.0, line = 1.5, font.main = 1) # (left, raised, normal font)


}


### Check:
## Basics:
# plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test 1")
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, title.lbl = "Test 2")  # by = "all" (default)
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", title.lbl = "Test 3a")  # by condition
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", title.lbl = "Test 3b", dir = 2) # bi-directional
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", title.lbl = "Test 4a")  # by decision
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", title.lbl = "Test 4b", dir = 2) # bi-directional
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", title.lbl = "Test 5a")  # by accuracy
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", title.lbl = "Test 5a", dir = 2) # bi-directional
#
## Note:
# plot_bar(N = 100, prev = .1, sens = .8, spec = .7, by = "cd", title.lbl = "Test", dir = 2, )

# plot_bar(N = 3, prev = .33, sens = .75, spec = .66,
#          title.lbl = "Test rounding effects",
#          show.freq = TRUE)  # => Rounding of freq, but not of proportions shown in plot.



## (*) Done: ----------

## - Scale 1 dimension by N (and add axis) [2018 08 13].
## - Add area labels (in center of area)   [2018 08 14].
## - Add options for by ("all", "cd", "dc", "ac") and
##                   dir (1 vs. 2)         [2018 08 15].


## (+) ToDo: ----------

## - Use text labels defined in txt.def and init_txt (incl. accuracy).
## - Add probabilitiy indicators (arrows and labels)
## - Allow alternative arrangements: horizontal (flip coord), dodged bars, ...
## - ...

## eof. ----------
