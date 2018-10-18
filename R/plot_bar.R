## plot_bar.R | riskyr
## 2018 10 18
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
#' When \code{round = FALSE}, bar heights for \code{scale = "f"}
#' and for \code{scale = "p"} are identical.
#'
#' The distinction between \code{scale = "f"} and
#' \code{scale = "p"} matters mostly for
#' small populations sizes \code{\link{N}}
#' (e.g., when \code{\link{N} < 100}).
#' For rounded and small frequency values (e.g., \code{\link{freq} < 10})
#' switching from \code{scale = "f"} to \code{scale = "p"}
#' yields different plots.
#'
#' \code{plot_bar} contrasts compound frequencies along 1 dimension (height).
#' See \code{\link{plot_mosaic}} for 2-dimensional visualizations (as areas)
#' and various \code{box}) options in
#' \code{\link{plot_tree}} and \code{\link{plot_fnet}}
#' for related functions.
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
#' are to be rounded to integers.
#' Default: \code{round = TRUE}.
#'
#' @param f_lbl  Type of frequency labels, as character code with the following options:
#' \enumerate{
#'   \item \code{f_lbl = "nam"}: names;
#'   \item \code{f_lbl = "num"}: numeric values (default);
#'   \item \code{f_lbl = "abb"}: abbreviated names;
#'   \item \code{f_lbl = NA/NULL/"no"}: no labels;
#'   \item \code{f_lbl = "any"}: abbreviated names and numeric values (abb = num).
#'   }
#'
#' @param f_lwd  Line width of frequency box (border).
#' Values of \code{NA/NULL/0} set \code{lwd} to
#' invisible \code{tiny_lwd <- .001} and \code{lty <- 0} (\code{"blank"}).
#' Default: \code{f_lwd = 0}.
#'
#' @param title_lbl  Main plot title.
#' Default: \code{title_lbl = txt$scen.lbl}.
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
#' plot_bar(prev = .33, sens = .75, spec = .66, title_lbl = "Test 1")
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60,
#'          title_lbl = "Test 2")  # by "all" (default)
#'
#' # Perspectives (by):
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd",
#'          title_lbl = "Test 3a")  # by condition
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", dir = 2,
#'          title_lbl = "Test 3b", f_lbl = "num")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc",
#'          title_lbl = "Test 4a")  # by decision
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", dir = 2,
#'          title_lbl = "Test 4b", f_lbl = "num")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac",
#'          title_lbl = "Test 5a")  # by accuracy
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", dir = 2,
#'          title_lbl = "Test 5b", f_lbl = "num")  # bi-directional
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
#'          title_lbl = "Rounding (1)") # => Scale by freq and round freq.
#' plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#'          scale = "p", round = TRUE,
#'          title_lbl = "Rounding (2)") # => Scale by prob and round freq.
#' plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#'          scale = "f", round = FALSE,
#'          title_lbl = "Rounding (3)") # => Scale by freq and do NOT round freq.
#' plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#'          scale = "p", round = FALSE,
#'          title_lbl = "Rounding (4)") # => Scale by prob and do NOT round freq.
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
#' @importFrom graphics lines
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
                     by = "all",     # perspective: "cd"...condition, "dc"...decision; "ac" accuracy, default: "all".
                     dir = 1,        # directions: 1 (default) vs. 2
                     scale = "f",    # scale bars: "f" ... freq (default), "p" ... prob.
                     round = TRUE,   # round freq to integers? (default: round = TRUE).
                     # Freq boxes:
                     f_lbl = "num",  # type of freq labels: "nam"/"num"/"abb", NA/NULL/"no", or "default" (fname = fnum).
                     f_lwd = .001,   # lwd of boxes: NULL vs. 1 vs. .001 (default)
                     # Text and color:
                     lbl_txt = txt,  # label text; was: title_lbl = txt$scen.lbl,  # main title of plot
                     col_pal = pal,  # color palette
                     # Generic options:
                     show_freq = TRUE,   # show essential freq values on plot margin
                     show_prob = TRUE,   # show essential prob value on plot margin (NOT help_line between bars)
                     show_accu = TRUE,   # show (exact OR freq-based) accuracy metrics on plot margin
                     w_acc = .50,        # weight w for wacc (from 0 to 1)
                     ...  # other (graphical) parameters: lwd, cex, ...
) {

  ## (0) Handle arguments and deprecated arguments: ----------

  ## (a) Get probabilities from global numeric parameters (num/prob):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec

  ## (b) Interpret arguments and increase robustness: ------

  # by perspective:
  if ( !is.null(by) && !is.na(by) ) { by <- tolower(by) }  # by in lowercase
  if (is.null(by) || is.na(by) || by == "def" || by == "default" || by == "any")  { by <- "all"}  # default/null
  if (by == "cond") { by <- "cd" }
  if (by == "dec")  { by <- "dc" }
  if (by == "acc")  { by <- "ac" }

  # Invalid perspective:
  if ((by %in% c("cd", "dc", "ac", "all")) == FALSE) {
    warning("Invalid perspective! Valid by = {'cd', 'dc', 'ac', 'all'}.\nUsing by = 'all'...")
    by <- "all"  # default
  }

  # dir:
  if (is.null(dir) || is.na(dir) || (dir <= 1)) { dir <- 1 }  # default/null
  if (dir > 2) { dir <- 2 }

  # scale:
  if (scale == "def" || scale == "default" || is.null(scale) || is.na(scale) ) { scale <- "f" }  # default/null
  if (scale == "freq") { scale <- "f" }  # default/null
  if (scale == "prob") { scale <- "p" }

  # f_lbl:
  if (is.null(f_lbl) || is.na(f_lbl)) { f_lbl <- "no" }
  f_lbl <- tolower(f_lbl)
  if (f_lbl == "val") (f_lbl <- "num")
  if (f_lbl == "namnum" || f_lbl == "namval" || f_lbl == "abbnum") (f_lbl <- "default")

  # f_lwd & lty:
  if ( is.null(f_lwd) || is.na(f_lwd) || f_lwd <= 0 ) {

    tiny_lwd <- .001   # tiny, invisible width
    f_lwd <- tiny_lwd  # to avoid error (for lwd = 0)
    lty <- 0           # "blank" (no lines) [only when f_lty and p_lty are NOT used]

  }

  ## (c) Additional parameters (currently fixed):
  # n_digits_bar <- 5  # n_digits to round freq to in bar plot (when round = FALSE)

  # Offset from base line:
  x.base <- 0  # offset x
  y.base <- 0  # offset y

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
    # n_digits = n_digits_bar)  # Removed n_digits parameter in comp_freq!

    ## ToDo: Update GLOBAL freq and prob objects
    ##       (e.g., to use label_freq/label_prob and plot_mar functions).

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

  title_lbl <- txt$scen.lbl
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

  ## Color info (NOW defined in init_pal):
  # col.prev <- col_p[1]  # prev.li  # prev help line
  # col.sens <- col_p[2]  # sens.li  # sens help line
  # col.spec <- col_p[3]  # spec.li  # spec help line
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
       col = grey(.50, .50), lty = 1,
       lwd = (par("lwd") * .50), equilogs = TRUE)

  ## Horizontal base line (y = 0):
  lines(c(0, 1), c(0, 0), col = pal["brd"], lwd = par("lwd"))

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
            box_x  = n.x,
            box_y  = n.y,
            box_lx = b.lx,
            box_ly = n.ly,
            cur_txt = lbl_txt, cur_pal = col_pal,
            lbl_type = f_lbl,
            lwd = f_lwd, ...)

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
              box_x  = hi.x,
              box_y  = hi.y,
              box_lx = b.lx,
              box_ly = hi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi.x,
              box_y  = mi.y,
              box_lx = b.lx,
              box_ly = mi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa.x,
              box_y  = fa.y,
              box_lx = b.lx,
              box_ly = fa.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr.x,
              box_y  = cr.y,
              box_lx = b.lx,
              box_ly = cr.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

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
              box_x  = cond.true.x,
              box_y  = cond.true.y,
              box_lx = b.lx,
              box_ly = cond.true.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "cond.false", fnum = (fa + cr),
              box_x  = cond.false.x,
              box_y  = cond.false.y,
              box_lx = b.lx,
              box_ly = cond.false.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

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
              box_x  = dec.pos.x,
              box_y  = dec.pos.y,
              box_lx = b.lx,
              box_ly = dec.pos.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "dec.neg", fnum = (mi + cr),
              box_x  = dec.neg.x,
              box_y  = dec.neg.y,
              box_lx = b.lx,
              box_ly = dec.neg.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

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
              box_x  = dec.cor.x,
              box_y  = dec.cor.y,
              box_lx = b.lx,
              box_ly = dec.cor.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "dec.err", fnum = (mi + fa),
              box_x  = dec.err.x,
              box_y  = dec.err.y,
              box_lx = b.lx,
              box_ly = dec.err.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    # Label acc column:
    plot_ftype_label("dec.cor", dec.cor.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.cor"),
                     ...)

  } else if (by == "cd") {

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
              box_x  = hi.x,
              box_y  = hi.y,
              box_lx = b.lx,
              box_ly = hi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi.x,
              box_y  = mi.y,
              box_lx = b.lx,
              box_ly = mi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa.x,
              box_y  = fa.y,
              box_lx = b.lx,
              box_ly = fa.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr.x,
              box_y  = cr.y,
              box_lx = b.lx,
              box_ly = cr.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)


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
              box_x  = cond.true.x,
              box_y  = cond.true.y,
              box_lx = b.lx,
              box_ly = cond.true.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "cond.false", fnum = (fa + cr),
              box_x  = cond.false.x,
              box_y  = cond.false.y,
              box_lx = b.lx,
              box_ly = cond.false.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    # Label cond column:
    plot_ftype_label("cond.true", cond.true.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond.true"),
                     ...)


  } else if (by == "dc") {

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
              box_x  = hi.x,
              box_y  = hi.y,
              box_lx = b.lx,
              box_ly = hi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi.x,
              box_y  = mi.y,
              box_lx = b.lx,
              box_ly = mi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa.x,
              box_y  = fa.y,
              box_lx = b.lx,
              box_ly = fa.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr.x,
              box_y  = cr.y,
              box_lx = b.lx,
              box_ly = cr.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

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
              box_x  = dec.pos.x,
              box_y  = dec.pos.y,
              box_lx = b.lx,
              box_ly = dec.pos.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "dec.neg", fnum = (mi + cr),
              box_x  = dec.neg.x,
              box_y  = dec.neg.y,
              box_lx = b.lx,
              box_ly = dec.neg.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

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
              box_x  = hi.x,
              box_y  = hi.y,
              box_lx = b.lx,
              box_ly = hi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi.x,
              box_y  = mi.y,
              box_lx = b.lx,
              box_ly = mi.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa.x,
              box_y  = fa.y,
              box_lx = b.lx,
              box_ly = fa.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr.x,
              box_y  = cr.y,
              box_lx = b.lx,
              box_ly = cr.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

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
              box_x  = dec.cor.x,
              box_y  = dec.cor.y,
              box_lx = b.lx,
              box_ly = dec.cor.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    plot_vbox(ftype = NA, fname = "dec.err", fnum = (mi + fa),
              box_x  = dec.err.x,
              box_y  = dec.err.y,
              box_lx = b.lx,
              box_ly = dec.err.ly,
              cur_txt = lbl_txt, cur_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, ...)

    # Label acc column:
    plot_ftype_label("dec.cor", dec.cor.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.cor"),
                     ...)

  } else if (by == "xxx") {

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

  # Plot GLOBAL freq/prob/accu values:
  plot_mar(show_freq = show_freq, show_cond = show_prob, show_dec = TRUE,
           show_accu = show_accu, accu_from_freq = round,  # default: accu_from_freq = FALSE.  Use accu_from_freq = round to show accuracy based on freq!
           note = "Showing global values on margin."   # "Some noteworthy remark here."
  )

}


### Check: --------

## Basics:
# plot_bar(prev = .33, sens = .75, spec = .66, title_lbl = "Test 1")
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60,
#          title_lbl = "Test 2")  # by "all" (default)
#
## Perspectives:
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd",
#          title_lbl = "Test 3a")  # by condition
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", dir = 2,
#          title_lbl = "Test 3b")  # bi-directional
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc",
#          title_lbl = "Test 4a")  # by decision
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", dir = 2,
#          title_lbl = "Test 4b")  # bi-directional
#
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac",
#          title_lbl = "Test 5a")  # by accuracy
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", dir = 2,
#        title_lbl = "Test 5b", f_lbl = "num")  # bi-directional
#
## Scaling and rounding effects:
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "f", round = TRUE,
#          title_lbl = "Rounding (1)") # => Scale by freq and round freq.
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "p", round = TRUE,
#          title_lbl = "Rounding (2)") # => Scale by prob and round freq.
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "f", round = FALSE,
#          title_lbl = "Rounding (3)") # => Scale by freq and do NOT round freq.
# plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
#          scale = "p", round = FALSE,
#          title_lbl = "Rounding (4)") # => Scale by prob and do NOT round freq.
#
## f_lbl: different types of freq labels:
# plot_bar(f_lbl = "nam")  # name only
# plot_bar(f_lbl = "num")  # numeric value only (default)
# plot_bar(f_lbl = "aBB")  # abbreviated name (lowercase)
# plot_bar(f_lbl = NA)     # no labels (NA/NULL/"no")
# plot_bar(f_lbl = "any")  # default labels: name = num


## (*) Done: ----------

## - Scale 1 dimension by N (and add axis).   [2018 08 13]
## - Add area labels (in center of area).     [2018 08 14]
## - Add options for by ("all", "cd", "dc", "ac") and
##                   dir (1 vs. 2).           [2018 08 15]
## - Add various f_lbl options.               [2018 09 21]
## - Modify defaults and increase robustness. [2018 09 25]


## (+) ToDo: ----------

## - Re-write with plot_fbox and plot_fbox_list (rather than plot_vbox).
## - Use text labels defined in txt.def and init_txt (incl. accuracy).
## - Add probabilitiy links (arrows and labels).
## - Allow alternative arrangements: horizontal (flip coord?), dodged bars, ...
## - ...

## eof. ----------
