## plot_bar.R | riskyr
## 2018 09 21
## -----------------------------------------------

## Plot bar (a family of) charts that express freq types as lengths ------
## (size and proportion) from 3 essential probabilities (prev, sens, spec),
## or current population data.frame popu.

## plot_bar: Documentation ---------

## Notes:
## (1) Bar heights are based on frequencies (rounded or not rounded),
##     OR on exact probabilities (based on scale setting).
## (2) plot_bar computes freq from prob,
##     but does NOT update global freq and prob objects.
##     It is NOT "smart" by NOT automatically deriving
##     freq and prob labels from global objects!

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
#' By default, \code{plot_bar} uses current frequencies
#' (i.e., rounded or not rounded, depending on the value of \code{round})
#' as bar heights, rather than using exact probabilities to
#' scale bar heights (i.e., default scaling is \code{scale = "f"}).
#' Using the option \code{scale = "p"} scales bar heights
#' by probabilities (e.g., showing bars for non-natural frequencies
#' even when frequencies are rounded).
#' When \code{round = FALSE}, the bar heights for \code{scale = "f"}
#' coincide with those for \code{scale = "p"}.
#'
#' The distinction between \code{scale = "f"} and
#' \code{scale = "p"} is practically irrelevant for
#' large populations sizes \code{\link{N}}
#' (or when all \code{\link{freq} > 10}), but useful for small values of
#' \code{\link{N}} (or scenarios with rounded \code{\link{freq} < 10}).
#'
#' \code{plot_bar} contrasts compound frequencies along 1 dimension (height).
#' See \code{\link{plot_mosaic}} for 2-dimensional visualizations (as areas)
#' and various \code{box}) options in
#' \code{\link{plot_tree}} and \code{\link{plot_fnet}}
#' for similar functions.
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
#' @param N  The number of individuals in the population.
#' (This value is not represented in the plot,
#' but used when new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch from current probabilities.)
#'
#'
#' @param by  A character code specifying the perspective
#' (or the dimension by which the population is split into 2 subsets)
#' with the following options:
#'   \enumerate{
#'   \item \code{by = "cd"}: by condition;
#'   \item \code{by = "dc"}: by decision;
#'   \item \code{by = "ac"}: by accuracy;
#'   \item \code{by = "all"} combines perspectives (5 bars, default).
#'   }
#'
#' @param dir  Number of directions in which bars are plotted.
#' Options:
#' \enumerate{
#'   \item \code{dir = 1}: uni-directional bars (all up, default);
#'   \item \code{dir = 2}: bi-directional bars (up vs. down).
#'   }
#'
#' @param scale  Scale the heights of bars either
#' by current frequencies (\code{scale = "f"}) or
#' by exact probabilities (\code{scale = "p"}).
#' Default: \code{scale = "f"}.
#' When \code{round = FALSE}, both settings yield the same bar heights.
#'
#' @param round  Boolean option specifying whether computed frequencies
#' are rounded to integers. Default: \code{round = TRUE}.
#'
#' @param f_lbl  Type of frequency labels, as character code with the following options:
#' \enumerate{
#'   \item \code{f_lbl = "nam"}: names;
#'   \item \code{f_lbl = "num"}: numeric values;
#'   \item \code{f_lbl = "abb"}: abbreviated names;
#'   \item \code{f_lbl = NA/NULL/"no"}: no labels;
#'   \item \code{f_lbl = "default"}: abbreviated names and numeric values.
#'   }
#'
#' @param title_lbl  Main plot title.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param col_pal  Color palette to use.
#' Default: \code{col_pal = pal} (see \code{\link{pal}} and \code{\link{init_pal}}).
#'
#' @param show_freq  Boolean option for showing essential frequencies
#' (i.e., of \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, and
#' \code{\link{cr}}) on the margin of the plot.
#' Default: \code{show_freq = TRUE}.
#'
#' @param show_prob  Boolean option for showing essential probabilities
#' (e.g., \code{\link{prev}}, \code{\link{sens}}, and
#' \code{\link{spec}}) on the margin of the plot.
#' Default: \code{show_prob = TRUE}.
#'
#' @param show_accu  Boolean option for showing current
#' accuracy metrics \code{\link{accu}} on the margin of the plot.
#' Default: \code{show_accu = TRUE}.
#'
#' @param w_acc  Weigthing parameter \code{w} used to compute
#' weighted accuracy \code{w.acc} in \code{\link{comp_accu_freq}}.
#' Default: \code{w_acc = .50}.
#'
#' @param ...  Other (graphical) parameters
#' (e.g., \code{cex}, \code{lwd}, ...).
#'
#'
#' @examples
#' # Basics:
#' plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test 1")
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60,
#'          title.lbl = "Test 2")  # by "all" (default)
#'
#' # Perspectives (by):
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd",
#'          title.lbl = "Test 3a")  # by condition
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", dir = 2,
#'          title.lbl = "Test 3b", f_lbl = "num")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc",
#'          title.lbl = "Test 4a")  # by decision
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", dir = 2,
#'          title.lbl = "Test 4b", f_lbl = "num")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac",
#'          title.lbl = "Test 5a")  # by accuracy
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", dir = 2,
#'          title.lbl = "Test 5b", f_lbl = "num")  # bi-directional
#'
#' # Frequency labels (f_lbl):
#' plot_bar(f_lbl = "default")  # default labels: name = num
#' plot_bar(f_lbl = "nam")      # name only
#' plot_bar(f_lbl = "num")      # numeric value only
#' plot_bar(f_lbl = "abb")      # abbreviated name
#' plot_bar(f_lbl = NA)         # no labels (NA/NULL/"no")
#'
#' # Scaling and rounding effects:
#' plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#'          scale = "f", round = TRUE,
#'          title.lbl = "Rounding (1)") # => Scale by freq and round freq.
#' plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#'          scale = "p", round = TRUE,
#'          title.lbl = "Rounding (2)") # => Scale by prob and round freq.
#' plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#'          scale = "f", round = FALSE,
#'          title.lbl = "Rounding (3)") # => Scale by freq and do NOT round freq.
#' plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#'          scale = "p", round = FALSE,
#'          title.lbl = "Rounding (4)") # => Scale by prob and do NOT round freq.
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
                     ## Specific options:
                     by = "all",    # perspective: "cd"...condition, "dc"...decision; "ac" accuracy, default: "all".
                     dir = 1,       # directions: 1 (default) vs. 2
                     scale = "f",   # scale bars: "f" ... freq (default), "p" ... prob.
                     round = TRUE,  # should freq be rounded to integers? (default: TRUE)
                     ## Text and color:
                     f_lbl = "default",         # type of freq labels: "default" (current fname = current fnum), "nam"/"num"/"abb"
                     title_lbl = txt$scen.lbl,  # main title of plot
                     col_pal = pal,             # color palette
                     ## General options:
                     show_freq = TRUE,   # show essential freq values on plot margin
                     show_prob = TRUE,   # show essential prob value on plot margin (NOT help_line between bars)
                     show_accu = TRUE,   # show (exact OR freq-based) accuracy metrics on plot margin
                     w_acc = .50,        # weight w for wacc (from 0 to 1)
                     ...  # other (graphical) parameters: lwd, cex, ...
) {

  ## (0) Handle arguments and deprecated arguments: ----------

  # ## (0) Get probabilities from global numeric parameters (num):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec

  ## Increase robustness by anticipating and correcting common entry errors: ------

  if (is.null(by) || is.na(by) || by == "def" || by == "default" || by == "any")  { by <- "all"}  # default/null
  if (by == "cond") { by <- "cd" }
  if (by == "dec")  { by <- "dc" }
  if (by == "acc")  { by <- "ac" }

  if (is.null(dir) || is.na(dir) ) { dir <- 1 }  # default/null
  if (dir > 2) { dir <- 2 }

  if (scale == "def" || scale == "default" || is.null(scale) || is.na(scale) ) { scale <- "f" }  # default/null
  if (scale == "freq") { scale <- "f" }  # default/null
  if (scale == "prob") { scale <- "p" }

  # if (exists("lwd") && lwd == 0) { lwd <- .01 }


  ## (1) Compute or use current popu: ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2]  # gets sens (if not provided)
    mirt <- prob_quintet[3]  # gets mirt (if not provided)
    spec <- prob_quintet[4]  # gets spec (if not provided)
    fart <- prob_quintet[5]  # gets fart (if not provided)

    ## (b) Compute freq based on current parameters (N and probabilities):
    freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = round)  # compute freq (default: round = TRUE)

    ## ToDo: Update global freq and prob objects
    ##       to use label_freq and label_prob functions.

    ## Assign (only needed) elements based on freq:
    hi  <- freq$hi
    mi  <- freq$mi
    fa  <- freq$fa
    cr  <- freq$cr

    ## (c) Compute cur.popu from computed frequencies:
    # cur.popu <- comp_popu(hi = hi, mi = mi, fa = fa, cr = cr)  # compute cur.popu (from 4 essential frequencies)

    ## warning("Generated new population (cur.popu) to plot...")

  } else {  # (B) NO valid set of probabilities was provided:

    ## Use the current popu:
    # cur.popu <- popu

    ## warning("Using existing population (popu) to plot...")

  } # if (is_valid_prob_set...)

  ## (2) Text labels: ----------

  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)
  cur_title_lbl <- paste0(title_lbl, "Bar plot of frequencies") # , "(N = ", N, ")")

  # cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  # # cur.dec.lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
  # cur.par.lbl <- cur.cond.lbl
  #
  # type_label <- "freq type"  # to be derived below.

  ## (3) Define plot and margin areas: ----------

  ## Margin areas:
  par(oma = c(3, 2, 1, 1) + 0.1)  # outer margins: bottom has 3 lines of space
  par(mar = c(4, 2, 4, 2) + 0.1)  # margin: default: c(5.1, 4.1, 4.1, 2.1)

  ## (4) Graphical parameters: ----

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


  ## (5) Define plot area: ----------

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


  ## (6) Custom bar plot: ----------

  ##   (A) Define N and 4 SDT cases (for all perspectives): ------

  ##     (a) Define basic length parameters: ----

  # Number and basic width of columns:
  if (by == "all") {
    nr.col <- 5       # 5 (vertical) columns
  } else {
    nr.col <- 3       # 3 (vertical) columns
  }
  col.x  <- 1/nr.col  # corresponding column width (x)

  # Length/height (y) of bars:
  lbase <- N               # length of base side (vertical: y)
  lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
  sf <- 1.0                # scaling factor (0-1)

  # Basic height (ly) and width (lx):
  b.ly <- lbase * sf  # basic height (scaled constant)
  b.lx <- lelse * sf  # basic width (scaled constant)


  ##     (b) Define and plot N column: ----

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
  plot_vbox(ftype = NA, fname = "N", fnum = N,
            box.x  = n.x,
            box.y  = n.y,
            box.lx = b.lx,
            box.ly = n.ly,
            lbl_type = f_lbl, ...)

  # Label N column:
  plot_ftype_label("N", n.x, y.min, pos = 1,
                   col = pal["txt"], # col = comp_freq_col("N"),
                   ...)

  ##     (c) Define 4 SDT cases/cells: ----

  # x-coordinates:
  col.nr <- 3
  hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
  mi.x <- hi.x
  fa.x <- hi.x
  cr.x <- hi.x

  # heights (ly):
  # 2 ways of computing bar heights:
  if (scale == "p") {

    # (1) Compute heights (ly) from current probabilities (without any rounding):
    hi.ly <- (n.ly * prev) * sens              # re-computes hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes fa (without rounding)

  } else if (scale == "f") {

    # (2) Take heights (ly) from current frequencies (with or without rounding, based on round option):
    hi.ly <- hi   # freq of hi (with/without rounding)
    mi.ly <- mi   # freq of mi (with/without rounding)
    cr.ly <- cr   # freq of cr (with/without rounding)
    fa.ly <- fa   # freq of fa (with/without rounding)

  } else { # any other setting:

    # as in (2) Take heights (ly) from current frequencies (with or without rounding, based on round option):
    hi.ly <- hi   # freq of hi (with/without rounding)
    mi.ly <- mi   # freq of mi (with/without rounding)
    cr.ly <- cr   # freq of cr (with/without rounding)
    fa.ly <- fa   # freq of fa (with/without rounding)

  } # (scale == ...)

  # Label SDT column:
  plot_ftype_label("hi", hi.x, y.min, pos = 1,
                   col = pal["txt"],
                   # col = comp_freq_col("hi"),
                   ...)

  ##   (B) Perspective-specific settings: ------
  if (by == "all") {

    ## (a) SDT column: ----

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 2 bars:
      fa.ly <- -1 * fa.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    if (dir == 2) {
      ## reverse y-coordinates (y) of 2 bars:
      cr.y <- y.base
      fa.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              lbl_type = f_lbl, ...)

    ## (b) Condition column: ----

    # x-coordinates:
    col.nr <- 2
    cond.true.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    cond.false.x <- cond.true.x

    # heights (ly) as sum of previous heights (4 cases/cells):
    cond.true.ly  <- abs(hi.ly) + abs(mi.ly)
    cond.false.ly <- abs(fa.ly) + abs(cr.ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 1 bar:
      cond.false.ly <- -1 * cond.false.ly
    }

    # y-coordinates (given heights):
    cond.true.y <- y.base
    cond.false.y <- cond.true.y + cond.true.ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 1 bar:
      cond.false.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "cond.true", fnum = (hi + mi),
              box.x  = cond.true.x,
              box.y  = cond.true.y,
              box.lx = b.lx,
              box.ly = cond.true.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "cond.false", fnum = (fa + cr),
              box.x  = cond.false.x,
              box.y  = cond.false.y,
              box.lx = b.lx,
              box.ly = cond.false.ly,
              lbl_type = f_lbl, ...)

    # Label cond column:
    plot_ftype_label("cond.true", cond.true.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond.true"),
                     ...)

    ## (c) Decision column: ----

    # x-coordinates:
    col.nr <- 4
    dec.pos.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.neg.x <- dec.pos.x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec.pos.ly <- abs(hi.ly) + abs(fa.ly)
    dec.neg.ly <- abs(mi.ly) + abs(cr.ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 1 bar:
      dec.neg.ly <- -1 * dec.neg.ly
    }

    # y-coordinates (given heights):
    dec.pos.y <- y.base
    dec.neg.y <- dec.pos.y + dec.pos.ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 1 bar:
      dec.neg.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "dec.pos", fnum = (hi + fa),
              box.x  = dec.pos.x,
              box.y  = dec.pos.y,
              box.lx = b.lx,
              box.ly = dec.pos.ly,
              lbl_type = f_lbl, ...)
    plot_vbox(ftype = NA, fname = "dec.neg", fnum = (mi + cr),
              box.x  = dec.neg.x,
              box.y  = dec.neg.y,
              box.lx = b.lx,
              box.ly = dec.neg.ly,
              lbl_type = f_lbl, ...)

    # Label dec column:
    plot_ftype_label("dec.pos", dec.pos.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.pos"),
                     ...)

    ## (d) Accuracy column: ----

    # x-coordinates:
    col.nr <- 5
    dec.cor.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.err.x <- dec.cor.x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec.cor.ly <- abs(hi.ly) + abs(cr.ly)
    dec.err.ly <- abs(mi.ly) + abs(fa.ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 1 bar:
      dec.err.ly <- -1 * dec.err.ly
    }

    # y-coordinates (given heights):
    dec.cor.y <- y.base
    dec.err.y <- dec.cor.y + dec.cor.ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 1 bar:
      dec.err.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "dec.cor", fnum = (hi + cr),
              box.x  = dec.cor.x,
              box.y  = dec.cor.y,
              box.lx = b.lx,
              box.ly = dec.cor.ly,
              lbl_type = f_lbl, ...)
    plot_vbox(ftype = NA, fname = "dec.err", fnum = (mi + fa),
              box.x  = dec.err.x,
              box.y  = dec.err.y,
              box.lx = b.lx,
              box.ly = dec.err.ly,
              lbl_type = f_lbl, ...)

    # Label acc column:
    plot_ftype_label("dec.cor", dec.cor.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.cor"),
                     ...)

  } # if (by == "all")

  else if (by == "cd") {

    ## (2): 3 vertical bars (condition in middle): ----------

    ## (a) SDT column: ----

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 2 bars:
      fa.ly <- -1 * fa.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 2 bars:
      cr.y <- y.base
      fa.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              lbl_type = f_lbl, ...)


    ## (b) Condition column: ----

    # x-coordinates:
    col.nr <- 2
    cond.true.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    cond.false.x <- cond.true.x

    # heights (ly) as sum of previous heights (4 cases/cells):
    cond.true.ly  <- abs(hi.ly) + abs(mi.ly)
    cond.false.ly <- abs(fa.ly) + abs(cr.ly)

    # Reverse some directions:
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
    plot_vbox(ftype = NA, fname = "cond.true", fnum = (hi + mi),
              box.x  = cond.true.x,
              box.y  = cond.true.y,
              box.lx = b.lx,
              box.ly = cond.true.ly,
              lbl_type = f_lbl, ...)
    plot_vbox(ftype = NA, fname = "cond.false", fnum = (fa + cr),
              box.x  = cond.false.x,
              box.y  = cond.false.y,
              box.lx = b.lx,
              box.ly = cond.false.ly,
              lbl_type = f_lbl, ...)

    # Label cond column:
    plot_ftype_label("cond.true", cond.true.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond.true"),
                     ...)


  } # if (by == "cd")

  else if (by == "dc") {

    ## (3): 3 vertical bars (decision in middle): ----------

    ## (a) SDT column: ----

    # Reverse some directions:
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
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              lbl_type = f_lbl, ...)

    ## (b) Decision column: ----

    # x-coordinates:
    col.nr <- 2
    dec.pos.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.neg.x <- dec.pos.x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec.pos.ly <- abs(hi.ly) + abs(fa.ly)
    dec.neg.ly <- abs(mi.ly) + abs(cr.ly)

    # Reverse some directions:
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
    plot_vbox(ftype = NA, fname = "dec.pos", fnum = (hi + fa),
              box.x  = dec.pos.x,
              box.y  = dec.pos.y,
              box.lx = b.lx,
              box.ly = dec.pos.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "dec.neg", fnum = (mi + cr),
              box.x  = dec.neg.x,
              box.y  = dec.neg.y,
              box.lx = b.lx,
              box.ly = dec.neg.ly,
              lbl_type = f_lbl, ...)

    # Label dec column:
    plot_ftype_label("dec.pos", dec.pos.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.pos"),
                     ...)

  } # if (by == "dc")

  if (by == "ac") {

    ## (4): 3 vertical bars (accuracy in middle): ----------

    ## (a) SDT column: ----

    # Reverse some directions:
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
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              lbl_type = f_lbl, ...)

    ## (b) Accuracy column: ----

    # x-coordinates:
    col.nr <- 2
    dec.cor.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.err.x <- dec.cor.x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec.cor.ly <- abs(hi.ly) + abs(cr.ly)
    dec.err.ly <- abs(mi.ly) + abs(fa.ly)

    # Reverse some directions:
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
    plot_vbox(ftype = NA, fname = "dec.cor", fnum = (hi + cr),
              box.x  = dec.cor.x,
              box.y  = dec.cor.y,
              box.lx = b.lx,
              box.ly = dec.cor.ly,
              lbl_type = f_lbl, ...)

    plot_vbox(ftype = NA, fname = "dec.err", fnum = (mi + fa),
              box.x  = dec.err.x,
              box.y  = dec.err.y,
              box.lx = b.lx,
              box.ly = dec.err.ly,
              lbl_type = f_lbl, ...)

    # Label acc column:
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
                  c(hi + mi, 0, fa + cr, 0), # by condition
                  c(hi,  mi, fa,  cr),       # 4 sdt categories
                  c(hi + fa, 0, mi + cr, 0)  # by decision
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


  ## (7) Title: --------

  # title(cur_title_lbl, adj = 0.5, line = 1.5, font.main = 1) # (centered, raised, normal font)
  title(cur_title_lbl, adj = 0.0, line = 1.5, font.main = 1) # (left, raised, normal font)

  ## (8) Margins: ------

  plot_mar(show_freq = show_freq, show_cond = show_prob, show_dec = TRUE,
           show_accu = show_accu, accu_from_freq = round,  # default: accu_from_freq = FALSE.  Use accu_from_freq = round to show accuracy based on freq!
           note = ""   # "Some noteworthy remark here."
  )

}


### Check: --------

## Basics:
# plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test 1")
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60,
#          title.lbl = "Test 2")  # by "all" (default)
#
## Perspectives:
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd",
#          title.lbl = "Test 3a")  # by condition
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", dir = 2,
#          title.lbl = "Test 3b")  # bi-directional
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc",
#          title.lbl = "Test 4a")  # by decision
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", dir = 2,
#          title.lbl = "Test 4b")  # bi-directional
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac",
#          title.lbl = "Test 5a")  # by accuracy
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", dir = 2,
#        title.lbl = "Test 5b", f_lbl = "num")  # bi-directional
#
## Scaling and rounding effects:
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "f", round = TRUE,
#          title.lbl = "Rounding (1)") # => Scale by freq and round freq.
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "p", round = TRUE,
#          title.lbl = "Rounding (2)") # => Scale by prob and round freq.
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "f", round = FALSE,
#          title.lbl = "Rounding (3)") # => Scale by freq and do NOT round freq.
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "p", round = FALSE,
#          title.lbl = "Rounding (4)") # => Scale by prob and do NOT round freq.
#
## f_lbl: different types of freq labels:
# plot_bar(f_lbl = "default")  # default labels: name = num
# plot_bar(f_lbl = "nam")  # name only
# plot_bar(f_lbl = "num")  # numeric value only
# plot_bar(f_lbl = "abb")  # abbreviated name
# plot_bar(f_lbl = NA)     # no labels (NA/NULL/"no")


## (*) Done: ----------

## - Scale 1 dimension by N (and add axis) [2018 08 13].
## - Add area labels (in center of area)   [2018 08 14].
## - Add options for by ("all", "cd", "dc", "ac") and
##                   dir (1 vs. 2)         [2018 08 15].
## - Add different f_lbl options.          [2018 09 21].

## (+) ToDo: ----------

## - Use make_box and plot_fbox_list (rather than plot_vbox).
## - Use text labels defined in txt.def and init_txt (incl. accuracy).
## - Add probabilitiy indicators (arrows and labels).
## - Allow alternative arrangements: horizontal (flip coord?), dodged bars, ...
## - ...

## eof. ----------
