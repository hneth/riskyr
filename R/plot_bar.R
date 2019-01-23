## plot_bar.R | riskyr
## 2019 01 04
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
#' For large population sizes \code{\link{N}} and
#' when \code{round = FALSE}, both settings yield the same bar heights.
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
#' Default: \code{f_lwd = 1}.
#'
#' @param lty  Line type of frequency box (border).
#' Values of \code{NA/NULL/0} set \code{lty} to
#' \code{lty <- 0}.
#' Default: \code{lty = 0} (i.e., no line).
#'
#' @param title_lbl  Text label for current plot title.
#' Default: \code{title_lbl = txt$scen_lbl}.
#'
#' @param lbl_txt  Current text information (for labels, titles, etc.).
#' Default: \code{lbl_txt = \link{txt}} (see \code{\link{init_txt}}).
#'
#' @param col_pal  Current color palette.
#' Default: \code{col_pal = \link{pal}} (see \code{\link{init_pal}}).
#'
#' @param mar_notes  Boolean option for showing margin notes.
#' Default: \code{mar_notes = TRUE}.
#'
#' @param ...  Other (graphical) parameters
#' (e.g., \code{cex}, \code{font}, \code{lty}, etc.).
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
#' # Customize colors and text:
#' plot_bar(dir = 1, f_lbl = "num", col_pal = pal_org)
#' plot_bar(dir = 2, f_lbl = "nam", col_pal = pal_mod)
#'
#' # Frequency labels (f_lbl):
#' plot_bar(f_lbl = "def")  # default labels: name = num
#' plot_bar(f_lbl = "nam")  # name only
#' plot_bar(f_lbl = "num")  # numeric value only
#' plot_bar(f_lbl = "abb")  # abbreviated name
#' plot_bar(f_lbl = NA)     # no labels (NA/NULL/"no")
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

                     # Specific options:
                     by = "all",     # perspective: "cd"...condition, "dc"...decision; "ac" accuracy, default: "all".
                     dir = 1,        # directions: 1 (default) vs. 2
                     scale = "f",    # scale bars: "f" ... freq (default), "p" ... prob.
                     round = TRUE,   # round freq to integers? (default: round = TRUE).

                     # Freq boxes:
                     f_lbl = "num",  # type of freq labels: "nam"/"num"/"abb", NA/NULL/"no", or "default" (fname = fnum).
                     f_lwd = 1,      # lwd of boxes: NULL vs. 1 vs. .001 (default)
                     lty = 0,        # default line type (0: no line, 1: solid line, etc.)

                     # Text and color:
                     lbl_txt = txt,             # labels and text elements
                     title_lbl = txt$scen_lbl,  # main plot title
                     col_pal = pal,             # color palette

                     # Generic options:
                     mar_notes = TRUE,   # show margin notes?
                     # show_freq = TRUE,   # show essential freq values on plot margin
                     # show_prob = TRUE,   # show essential prob value on plot margin (NOT help_line between bars)
                     # show_accu = TRUE,   # show (exact OR freq-based) accuracy metrics on plot margin
                     # w_acc = .50,        # weight w for wacc (from 0 to 1)

                     ...  # other (graphical) parameters: cex, font, lty, etc.
) {

  ## (0) Handle arguments and deprecated arguments: ----------


  ## (1) Prepare parameters: ----------

  ## (A) Generic:

  opar <- par(no.readonly = TRUE)  # copy of current settings
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## (B) Interpret arguments and increase robustness: ------

  # by perspective:
  if ( !is.null(by) && !is.na(by) ) { by <- tolower(by) }  # by in lowercase
  if (is.null(by) || is.na(by) || by == "def" || by == "default" || by == "any")  { by <- "all"}  # default/null
  if (by == "cond") { by <- "cd" }
  if (by == "dec")  { by <- "dc" }
  if (by == "acc")  { by <- "ac" }

  # Invalid perspective:
  if ((by %in% c("cd", "dc", "ac", "all")) == FALSE) {
    message("Invalid perspective! Valid by = {'cd', 'dc', 'ac', 'all'}.\nUsing by = 'all'...")
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
  tiny_lwd <- .001   # initialize tiny, invisible width

  if (is.null(lty) || is.na(lty) || (lty < 0)) { lty <- 0 }  # default/null

  if ( is.null(f_lwd) || is.na(f_lwd) || f_lwd <= 0 ) {

    f_lwd <- tiny_lwd  # to avoid error (for lwd = 0)
    lty <- 0           # "blank" (no lines) [only when f_lty and p_lty are NOT used]

  }

  ## (c) Additional parameters (currently fixed):
  # n_digits_bar <- 5  # n_digits to round freq to in bar plot (when round = FALSE)

  # Offset from base line:
  x_base <- 0  # offset x
  y_base <- 0  # offset y

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

    ## (c) Assign (only needed) elements based on freq:
    hi  <- freq$hi
    mi  <- freq$mi
    fa  <- freq$fa
    cr  <- freq$cr

  } else {  # (B) NO valid set of probabilities was provided:

    message("No valid set of probabilities provided. Using global freq for bar plot.")

  } # if (is_valid_prob_set...)

  ## (2) Text labels: ----------

  # Plot title:
  if (is.null(title_lbl)) { title_lbl <- "" }              # adjust NULL to "" (i.e., no title)
  if (is.na(title_lbl)) { title_lbl <- lbl_txt$scen_lbl }  # use scen_lbl as default plot title


  ## (3) Colors / color palettes: ---------

  # (a) Set plot background color:
  par(bg = col_pal[["bg"]])  # col_pal[["bg"]] / "white" / NA (for transparent background)

  # (b) Detect and handle special cases of color equality (e.g., pal_bwp):
  if ( (par("bg") %in% col_pal[1:11]) && # if bg is equal to ANY fbox color AND
       ((f_lwd <= tiny_lwd) || (lty == 0)) ) {  # f_lwd is tiny_lwd OR lty is zero (default):
    if (f_lwd <= tiny_lwd) {f_lwd <- 1}
    if (lty == 0) {lty <- 1}
  }


  ## (4) Define plot and margin areas: ----------

  ## Define margin areas:

  if (nchar(title_lbl) > 0) { n_lines_top <- 3 } else { n_lines_top <- 0 }
  if (mar_notes) { n_lines_bot <- 3 } else { n_lines_bot <- 1 }

  par(mar = c(n_lines_bot, 2, n_lines_top, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(3, 1, 1, 1) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.


  ## Axis label locations:
  # par(mgp = c(3, 1, 0)) # default: c(3, 1, 0)

  ## Orientation of the tick mark labels (and corresponding mtext captions below):
  # par(las = 0)  # Options: parallel to the axis (0 = default), horizontal (1), perpendicular to axis (2), vertical (3).


  ## (5) Graphical parameters: ----

  ## Color info (NOW defined in init_pal):
  # col_prev <- col_p[1]  # prev.li  # prev help line
  # col_sens <- col_p[2]  # sens.li  # sens help line
  # col_spec <- col_p[3]  # spec.li  # spec help line
  # col_bord <- grey(.20, .99) # NA # grey(.11, .99)  # borders and labels (NA removes borders)

  ## Currently fixed parameters:
  # gap <- 2.0/100  # width of gap between 2 main subgroups (direction set via "by" argument)
  # show_prob_comp <- TRUE  # show help_line for complements of prob (e.g, prev, sens, spec)?

  ## Box appearance:
  # box_lwd <- 1  # line width of border around rect box (default = 1)

  ## Point appearance:
  # pt_pch <- 21    # pch symbol of points
  # pt_cex <- 1.4   # cex scaling of points
  # pt_lwd <- 1.6   # lwd of point borders

  ## Text label appearance:
  # col_lbl <- pal["txt"]  # defined in pal
  # cex_lbl <- .90   # scaling factor for text labels
  # cex_lbl_sm <- if (cex_lbl > .50) {cex_lbl - .10} else {cex_lbl}  # slightly smaller than cex_lbl
  # h_shift <- .05   # horizontal shifting of labels
  # v_shift <- .05   # vertical shifting of labels

  ## help line properties (main metrics):
  # lty_help <- 1    # line type
  # lwd_help <- 2.5  # line width


  ## (6) Define plot area: ----------

  ## Plot dimensions:
  xlim = c(0, 1)

  if (dir == 1) {
    y_min <- 0
  } else if (dir == 2) {
    y_min <- -N
  }
  ylim = c(y_min, N)

  ## Plot area setup:
  plot(x = 1,
       xlim = xlim, ylim = ylim,
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "n",
       fg = grey(.50, alpha = .99)
  )

  ## Mark plot and margin area:
  # col_plot <- "forestgreen"
  # box("plot", col = col_plot)
  # mar_col <- "firebrick"
  # box("figure", col = mar_col)

  ## Axes:
  # axis(side = 1, las = 1) # x-axis, horizontal labels
  axis(side = 2, las = 2) # y-axis, horizontal labels

  ## Grid:
  grid(nx = NA, ny = NULL,  # y-axes only (at tick marks)
       col = grey(.50, .50), lty = 1,
       lwd = (par("lwd") * .50), equilogs = TRUE)

  ## Horizontal base line (y = 0):
  lines(c(0, 1), c(0, 0), col = pal["brd"], lwd = par("lwd"))

  ## (7) Custom bar plot: ----------

  ##   (A) Define N and 4 SDT cases (for all perspectives): ------

  ##     (a) Define basic length parameters: ----

  # Number and basic width of columns:
  if (by == "all") {
    nr.col <- 5       # 5 (vertical) columns
  } else {
    nr.col <- 3       # 3 (vertical) columns
  }
  col_x  <- 1/nr.col  # corresponding column width (x)

  # Length/height (y) of bars:
  lbase <- N               # length of base side (vertical: y)
  lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
  sf <- 1.0                # scaling factor (0-1)

  # Basic height (ly) and width (lx):
  b_ly <- lbase * sf  # basic height (scaled constant)
  b_lx <- lelse * sf  # basic width (scaled constant)


  ##     (b) Define and plot N column: ----

  # Dimensions and coordinates:
  n_ly <- b_ly    # height (y)
  col_nr <- 1     # column number (out of nr.col)
  n_x  <- (x_base + (col_nr * col_x) - (col_x/2))  # x-coordinate: mid point of column col_nr
  n_y  <- y_base  # y-coordinate
  if (dir == 2) {
    ## center N bar around 0:
    n_y  <- y_base - N/2
  }

  # Plot 1 box:
  plot_vbox(ftype = NA, fname = "N", fnum = N,
            box_x  = n_x,
            box_y  = n_y,
            box_lx = b_lx,
            box_ly = n_ly,
            lbl_txt = lbl_txt, col_pal = col_pal,
            lbl_type = f_lbl, lwd = f_lwd, lty = lty,
            ...)

  # Label N column:
  plot_ftype_label("N", n_x, y_min, pos = 1,
                   col = pal["txt"], # col = comp_freq_col("N"),
                   ...)

  ##     (c) Define 4 SDT cases/cells: ----

  # x-coordinates:
  col_nr <- 3
  hi_x <- (x_base + (col_nr * col_x) - (col_x/2))  # mid point of column col_nr
  mi_x <- hi_x
  fa_x <- hi_x
  cr_x <- hi_x

  # heights (ly):
  # 2 ways of computing bar heights:
  if (scale == "p") {

    # (1) Compute heights (ly) from current probabilities (without any rounding):
    hi_ly <- (n_ly * prev) * sens              # re-computes hi (without rounding)
    mi_ly <- (n_ly * prev) * (1 - sens)        # re-computes mi (without rounding)
    cr_ly <- (n_ly * (1 - prev)) * spec        # re-computes cr (without rounding)
    fa_ly <- (n_ly * (1 - prev)) * (1 - spec)  # re-computes fa (without rounding)

  } else if (scale == "f") {

    # (2) Take heights (ly) from current frequencies (with or without rounding, based on round option):
    hi_ly <- hi   # freq of hi (with/without rounding)
    mi_ly <- mi   # freq of mi (with/without rounding)
    cr_ly <- cr   # freq of cr (with/without rounding)
    fa_ly <- fa   # freq of fa (with/without rounding)

  } else { # any other setting:

    # as in (2) Take heights (ly) from current frequencies (with or without rounding, based on round option):
    hi_ly <- hi   # freq of hi (with/without rounding)
    mi_ly <- mi   # freq of mi (with/without rounding)
    cr_ly <- cr   # freq of cr (with/without rounding)
    fa_ly <- fa   # freq of fa (with/without rounding)

  } # (scale == ...)

  # Label SDT column:
  plot_ftype_label("hi", hi_x, y_min, pos = 1,
                   col = pal["txt"],
                   # col = comp_freq_col("hi"),
                   ...)

  ##   (B) Perspective-specific settings: ------
  if (by == "all") {

    ## (a) SDT column: ----

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 2 bars:
      fa_ly <- -1 * fa_ly
      cr_ly <- -1 * cr_ly
    }

    # y-coordinates (given heights):
    hi_y <- y_base
    mi_y <- hi_y + hi_ly
    fa_y <- mi_y + mi_ly
    cr_y <- fa_y + fa_ly

    if (dir == 2) {
      ## reverse y-coordinates (y) of 2 bars:
      cr_y <- y_base
      fa_y <- cr_y + cr_ly
    }

    # Plot 4 boxes:
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box_x  = hi_x,
              box_y  = hi_y,
              box_lx = b_lx,
              box_ly = hi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi_x,
              box_y  = mi_y,
              box_lx = b_lx,
              box_ly = mi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa_x,
              box_y  = fa_y,
              box_lx = b_lx,
              box_ly = fa_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr_x,
              box_y  = cr_y,
              box_lx = b_lx,
              box_ly = cr_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    ## (b) Condition column: ----

    # x-coordinates:
    col_nr <- 2
    cond_true_x <- (x_base + (col_nr * col_x) - (col_x/2))  # mid point of column col_nr
    cond_false_x <- cond_true_x

    # heights (ly) as sum of previous heights (4 cases/cells):
    cond_true_ly  <- abs(hi_ly) + abs(mi_ly)
    cond_false_ly <- abs(fa_ly) + abs(cr_ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 1 bar:
      cond_false_ly <- -1 * cond_false_ly
    }

    # y-coordinates (given heights):
    cond_true_y <- y_base
    cond_false_y <- cond_true_y + cond_true_ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 1 bar:
      cond_false_y <- y_base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "cond_true", fnum = (hi + mi),
              box_x  = cond_true_x,
              box_y  = cond_true_y,
              box_lx = b_lx,
              box_ly = cond_true_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "cond_false", fnum = (fa + cr),
              box_x  = cond_false_x,
              box_y  = cond_false_y,
              box_lx = b_lx,
              box_ly = cond_false_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    # Label cond column:
    plot_ftype_label("cond_true", cond_true_x, y_min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond_true"),
                     ...)

    ## (c) Decision column: ----

    # x-coordinates:
    col_nr <- 4
    dec_pos_x <- (x_base + (col_nr * col_x) - (col_x/2))  # mid point of column col_nr
    dec_neg_x <- dec_pos_x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec_pos_ly <- abs(hi_ly) + abs(fa_ly)
    dec_neg_ly <- abs(mi_ly) + abs(cr_ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 1 bar:
      dec_neg_ly <- -1 * dec_neg_ly
    }

    # y-coordinates (given heights):
    dec_pos_y <- y_base
    dec_neg_y <- dec_pos_y + dec_pos_ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 1 bar:
      dec_neg_y <- y_base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "dec_pos", fnum = (hi + fa),
              box_x  = dec_pos_x,
              box_y  = dec_pos_y,
              box_lx = b_lx,
              box_ly = dec_pos_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "dec_neg", fnum = (mi + cr),
              box_x  = dec_neg_x,
              box_y  = dec_neg_y,
              box_lx = b_lx,
              box_ly = dec_neg_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    # Label dec column:
    plot_ftype_label("dec_pos", dec_pos_x, y_min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec_pos"),
                     ...)

    ## (d) Accuracy column: ----

    # x-coordinates:
    col_nr <- 5
    dec_cor_x <- (x_base + (col_nr * col_x) - (col_x/2))  # mid point of column col_nr
    dec_err_x <- dec_cor_x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec_cor_ly <- abs(hi_ly) + abs(cr_ly)
    dec_err_ly <- abs(mi_ly) + abs(fa_ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 1 bar:
      dec_err_ly <- -1 * dec_err_ly
    }

    # y-coordinates (given heights):
    dec_cor_y <- y_base
    dec_err_y <- dec_cor_y + dec_cor_ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 1 bar:
      dec_err_y <- y_base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "dec_cor", fnum = (hi + cr),
              box_x  = dec_cor_x,
              box_y  = dec_cor_y,
              box_lx = b_lx,
              box_ly = dec_cor_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "dec_err", fnum = (mi + fa),
              box_x  = dec_err_x,
              box_y  = dec_err_y,
              box_lx = b_lx,
              box_ly = dec_err_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    # Label acc column:
    plot_ftype_label("dec_cor", dec_cor_x, y_min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec_cor"),
                     ...)

  } else if (by == "cd") {

    ## (2): 3 vertical bars (condition in middle): ----------

    ## (a) SDT column: ----

    # Reverse some directions:
    if (dir == 2) {
      ## reverse height (ly) of 2 bars:
      fa_ly <- -1 * fa_ly
      cr_ly <- -1 * cr_ly
    }

    # y-coordinates (given heights):
    hi_y <- y_base
    mi_y <- hi_y + hi_ly
    fa_y <- mi_y + mi_ly
    cr_y <- fa_y + fa_ly

    if (dir == 2) {
      ## reverse y-coordinate (y) of 2 bars:
      cr_y <- y_base
      fa_y <- cr_y + cr_ly
    }

    # Plot 4 boxes:
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box_x  = hi_x,
              box_y  = hi_y,
              box_lx = b_lx,
              box_ly = hi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi_x,
              box_y  = mi_y,
              box_lx = b_lx,
              box_ly = mi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa_x,
              box_y  = fa_y,
              box_lx = b_lx,
              box_ly = fa_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr_x,
              box_y  = cr_y,
              box_lx = b_lx,
              box_ly = cr_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)


    ## (b) Condition column: ----

    # x-coordinates:
    col_nr <- 2
    cond_true_x <- (x_base + (col_nr * col_x) - (col_x/2))  # mid point of column col_nr
    cond_false_x <- cond_true_x

    # heights (ly) as sum of previous heights (4 cases/cells):
    cond_true_ly  <- abs(hi_ly) + abs(mi_ly)
    cond_false_ly <- abs(fa_ly) + abs(cr_ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond_false_ly <- -1 * cond_false_ly
    }

    # y-coordinates (given heights):
    cond_true_y <- y_base
    cond_false_y <- cond_true_y + cond_true_ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond_false_y <- y_base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "cond_true", fnum = (hi + mi),
              box_x  = cond_true_x,
              box_y  = cond_true_y,
              box_lx = b_lx,
              box_ly = cond_true_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "cond_false", fnum = (fa + cr),
              box_x  = cond_false_x,
              box_y  = cond_false_y,
              box_lx = b_lx,
              box_ly = cond_false_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    # Label cond column:
    plot_ftype_label("cond_true", cond_true_x, y_min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond_true"),
                     ...)


  } else if (by == "dc") {

    ## (3): 3 vertical bars (decision in middle): ----------

    ## (a) SDT column: ----

    # Reverse some directions:
    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi_ly <- -1 * mi_ly
      cr_ly <- -1 * cr_ly
    }

    # y-coordinates (given heights):
    hi_y <- y_base
    fa_y <- hi_y + hi_ly
    cr_y <- fa_y + fa_ly
    mi_y <- cr_y + cr_ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      cr_y <- y_base
      mi_y <- cr_y + cr_ly
    }

    # Plot 4 boxes:
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box_x  = hi_x,
              box_y  = hi_y,
              box_lx = b_lx,
              box_ly = hi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi_x,
              box_y  = mi_y,
              box_lx = b_lx,
              box_ly = mi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa_x,
              box_y  = fa_y,
              box_lx = b_lx,
              box_ly = fa_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr_x,
              box_y  = cr_y,
              box_lx = b_lx,
              box_ly = cr_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    ## (b) Decision column: ----

    # x-coordinates:
    col_nr <- 2
    dec_pos_x <- (x_base + (col_nr * col_x) - (col_x/2))  # mid point of column col_nr
    dec_neg_x <- dec_pos_x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec_pos_ly <- abs(hi_ly) + abs(fa_ly)
    dec_neg_ly <- abs(mi_ly) + abs(cr_ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec_neg_ly <- -1 * dec_neg_ly
    }

    # y-coordinates (given heights):
    dec_pos_y <- y_base
    dec_neg_y <- dec_pos_y + dec_pos_ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec_neg_y <- y_base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "dec_pos", fnum = (hi + fa),
              box_x  = dec_pos_x,
              box_y  = dec_pos_y,
              box_lx = b_lx,
              box_ly = dec_pos_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "dec_neg", fnum = (mi + cr),
              box_x  = dec_neg_x,
              box_y  = dec_neg_y,
              box_lx = b_lx,
              box_ly = dec_neg_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    # Label dec column:
    plot_ftype_label("dec_pos", dec_pos_x, y_min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec_pos"),
                     ...)

  } # if (by == "dc")

  if (by == "ac") {

    ## (4): 3 vertical bars (accuracy in middle): ----------

    ## (a) SDT column: ----

    # Reverse some directions:
    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi_ly <- -1 * mi_ly
      fa_ly <- -1 * fa_ly
    }

    # y-coordinates (given heights):
    hi_y <- y_base
    cr_y <- hi_y + hi_ly
    mi_y <- cr_y + cr_ly
    fa_y <- mi_y + mi_ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi_y <- y_base
      fa_y <- mi_y + mi_ly
    }

    # Plot 4 boxes:
    plot_vbox(ftype = NA, fname = "hi", fnum = hi,
              box_x  = hi_x,
              box_y  = hi_y,
              box_lx = b_lx,
              box_ly = hi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "mi", fnum = mi,
              box_x  = mi_x,
              box_y  = mi_y,
              box_lx = b_lx,
              box_ly = mi_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "fa", fnum = fa,
              box_x  = fa_x,
              box_y  = fa_y,
              box_lx = b_lx,
              box_ly = fa_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "cr", fnum = cr,
              box_x  = cr_x,
              box_y  = cr_y,
              box_lx = b_lx,
              box_ly = cr_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    ## (b) Accuracy column: ----

    # x-coordinates:
    col_nr <- 2
    dec_cor_x <- (x_base + (col_nr * col_x) - (col_x/2))  # mid point of column col_nr
    dec_err_x <- dec_cor_x

    # heights (ly) as sum of previous heights (4 cases/cells):
    dec_cor_ly <- abs(hi_ly) + abs(cr_ly)
    dec_err_ly <- abs(mi_ly) + abs(fa_ly)

    # Reverse some directions:
    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec_err_ly <- -1 * dec_err_ly
    }

    # y-coordinates (given heights):
    dec_cor_y <- y_base
    dec_err_y <- dec_cor_y + dec_cor_ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec_err_y <- y_base
    }

    # Plot 2 boxes:
    plot_vbox(ftype = NA, fname = "dec_cor", fnum = (hi + cr),
              box_x  = dec_cor_x,
              box_y  = dec_cor_y,
              box_lx = b_lx,
              box_ly = dec_cor_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    plot_vbox(ftype = NA, fname = "dec_err", fnum = (mi + fa),
              box_x  = dec_err_x,
              box_y  = dec_err_y,
              box_lx = b_lx,
              box_ly = dec_err_ly,
              lbl_txt = lbl_txt, col_pal = col_pal,
              lbl_type = f_lbl, lwd = f_lwd, lty = lty,
              ...)

    # Label acc column:
    plot_ftype_label("dec_cor", dec_cor_x, y_min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec_cor"),
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


  ## (8) Title: --------

  # Define parts:
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } else {
    type_lbl <- paste0("Bar plot of frequencies (by ", as.character(by), ")")  # plot name: Bar/etc.
  }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = +1, font.main = 1, cex.main = 1.2)  # (left, raised by +1, normal font)


  ## (9) Margins: ------

  if (mar_notes) {

    # ## Plot GLOBAL freq/prob/accu values:
    # plot_mar(show_freq = show_freq, show_cond = show_prob, show_dec = TRUE,
    #          show_accu = show_accu, accu_from_freq = round,  # default: accu_from_freq = FALSE.  Use accu_from_freq = round to show accuracy based on freq!
    #          note = "Showing global values on margin."   # "Some noteworthy remark here."
    # )

    # Note:
    note_lbl <- ""  # initialize
    #if (scale == "f") {
    note_lbl <- label_note(area = "bar", scale = scale)
    #}

    plot_mar(show_freq = TRUE, show_cond = TRUE, show_dec = TRUE,
             show_accu = TRUE, accu_from_freq = FALSE,
             note = note_lbl,
             cur_freq = freq, cur_prob = prob, lbl_txt = lbl_txt)

  } # if (mar_notes) etc.


  ## (+) Finish: ---------

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible()# restores par(opar)

} # plot_bar end.


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


## Retired parameters: ----------

# @param show_freq  Boolean option for showing essential frequencies
# (i.e., of \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, and
# \code{\link{cr}}) on the margin of the plot.
# Default: \code{show_freq = TRUE}.
#
# @param show_prob  Boolean option for showing essential probabilities
# (e.g., \code{\link{prev}}, \code{\link{sens}}, and
# \code{\link{spec}}) on the margin of the plot.
# Default: \code{show_prob = TRUE}.
#
# @param show_accu  Boolean option for showing current
# accuracy metrics \code{\link{accu}} on the margin of the plot.
# Default: \code{show_accu = TRUE}.
#
# @param w_acc  Weighting parameter \code{w} used to compute
# weighted accuracy \code{w.acc} in \code{\link{comp_accu_freq}}.
# Default: \code{w_acc = .50}.


## (*) Done: ----------

## - Scale 1 dimension by N (and add axis).   [2018 08 13]
## - Add area labels (in center of area).     [2018 08 14]
## - Add options for by ("all", "cd", "dc", "ac") and
##                   dir (1 vs. 2).           [2018 08 15]
## - Add various f_lbl options.               [2018 09 21]
## - Modify defaults and increase robustness. [2018 09 25]

## (+) ToDo: ----------

## - Re-write with plot_fbox and plot_fbox_list (rather than plot_vbox).
## - Use text labels defined in txt_def and init_txt (incl. accuracy).
## - Add probabilitiy links (arrows and labels).
## - Allow alternative arrangements: horizontal (flip coord?), dodged bars, ...
## - ...

## eof. ----------
