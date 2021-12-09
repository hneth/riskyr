## plot_crisk.R | riskyr
## 2021 12 09
## Plot cumulative risk curve
## -----------------------------------------------

## (1) plot_crisk: Documentation ------

#' Plot a cumulative risk curve.
#'
#' \code{plot_crisk} creates visualizations of
#' cumulative risks.
#'
#' Assumes data inputs \code{x} and \code{y},
#' which correspond to each other so that
#' \code{y} is a (monotonically increasing)
#' probability density function (over risk increments
#' expressed as a function of \code{x}).
#'
#'
#' @param x Values on an x-dimension on which risk is expressed
#' (required, as a vector).
#'
#' @param y Values of cumulative risks on an y-dimension
#' (required, as a vector),
#' as monotonically increasing percentage values
#' (0 <= y <= 100).
#' Pairs of \code{x} and \code{y} are assumed to
#' correspond to each other.
#'
#' @param x_from Start value of risk increment.
#'
#' @param x_to End value of risk increment.
#'
#' @param fit_curve Boolean: Fit a curve to \code{x}-\code{y}-data?
#' Default: \code{fit_curve = FALSE}.
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
#' A suitable value of \code{\link{N}} is computed, if not provided.
#' Note: \code{\link{N}} is not represented in the plot,
#' but used for computing frequency information \code{\link{freq}}
#' from current probabilities \code{\link{prob}}.
#'
#' @param by A character code specifying 2 perspectives that split the population into subsets,
#' with 6 options:
#'   \enumerate{
#'   \item \code{"cddc"}: by condition (cd) and by decision (dc) (default);
#'   \item \code{"cdac"}: by condition (cd) and by accuracy (ac);
#'   \item \code{"dccd"}: by decision (dc) and by condition (cd);
#'   \item \code{"dcac"}: by decision (dc) and by accuracy (ac);
#'   \item \code{"accd"}: by accuracy (ac) and by condition (cd);
#'   \item \code{"acdc"}: by accuracy (ac) and by decision (dc).
#'   }
#'
#' @param p_split Primary perspective for population split,
#' with 2 options:
#'   \enumerate{
#'   \item \code{"v"}: vertical (default);
#'   \item \code{"h"}: horizontal.
#'   }
#'
#' @param area A character code specifying the shape of the main area,
#' with 2 options:
#'   \enumerate{
#'   \item \code{"sq"}: main area is scaled to square (default);
#'   \item \code{"no"}: no scaling (rectangular area fills plot size).
#'   }
#'
#' @param scale Scale probabilities and corresponding area dimensions either by
#' exact probability or by (rounded or non-rounded) frequency, with 2 options:
#'   \enumerate{
#'   \item \code{"p"}: scale main area dimensions by exact probability (default);
#'   \item \code{"f"}: re-compute probabilities from (rounded or non-rounded) frequencies
#'   and scale main area dimensions by their frequency.
#'   }
#'  Note: \code{scale} setting matters for the display of probability values and for
#'  area plots with small population sizes \code{\link{N}} when \code{round = TRUE}.
#'
#' @param round A Boolean option specifying whether computed frequencies
#' are rounded to integers. Default: \code{round = TRUE}.
#'
#' @param sample Boolean value that determines whether frequency values
#' are sampled from \code{N}, given the probability values of
#' \code{prev}, \code{sens}, and \code{spec}.
#' Default: \code{sample = FALSE}.
#'
#' @param sum_w Border width of 2 perspective summaries
#' (on top and left borders) of main area as a proportion of area size
#' (i.e., in range \code{0 <= sum_w <= 1}).
#' Default: \code{sum_w = .10}.
#' Setting \code{sum_w = 0}, \code{NA}, or \code{NULL} removes summaries;
#' setting \code{sum_w = 1} scales summaries to same size as main areas.
#'
#' @param gaps Size of gaps (as binary numeric vector) specifying
#' the width of vertical and horizontal gaps as proportions of area size.
#' Defaults: \code{gaps = c(.02, .00)} for \code{p_split = "v"} and
#' \code{gaps = c(.00, .02)} for \code{p_split = "h"}.
#'
#' @param f_lbl Type of label for showing frequency values in 4 main areas,
#' with 6 options:
#'   \enumerate{
#'   \item \code{"def"}: abbreviated names and frequency values;
#'   \item \code{"abb"}: abbreviated frequency names only (as specified in code);
#'   \item \code{"nam"}: names only (as specified in \code{lbl_txt = txt});
#'   \item \code{"num"}: numeric frequency values only (default);
#'   \item \code{"namnum"}: names (as specified in \code{lbl_txt = txt}) and numeric values;
#'   \item \code{"no"}: no frequency labels (same for \code{f_lbl = NA} or \code{NULL}).
#'   }
#'
#' @param f_lbl_sep Label separator for main frequencies
#' (used for \code{f_lbl = "def" OR "namnum"}).
#' Use \code{f_lbl_sep = ":\n"} to add a line break between name and numeric value.
#' Default: \code{f_lbl_sep = NA} (set to \code{" = "} or \code{":\n"} based on \code{f_lbl}).
#'
#' @param f_lbl_sum Type of label for showing frequency values in summary cells,
#' with same 6 options as \code{f_lbl} (above).
#' Default: \code{f_lbl_sum = "num"}: numeric values only.
#'
#' @param f_lbl_hd Type of label for showing frequency values in header,
#' with same 6 options as \code{f_lbl} (above).
#' Default: \code{f_lbl_hd = "nam"}: names only (as specified in \code{lbl_txt = txt}).
#'
#' @param f_lwd Line width of areas.
#' Default: \code{f_lwd = 0}.
#'
#' @param p_lbl Type of label for showing 3 key probability links and values,
#' with 7 options:
#'   \enumerate{
#'   \item \code{"def"}: show links and abbreviated names and probability values;
#'   \item \code{"abb"}: show links and abbreviated probability names;
#'   \item \code{"nam"}: show links and probability names (as specified in code);
#'   \item \code{"num"}: show links and numeric probability values;
#'   \item \code{"namnum"}: show links with names and numeric probability values;
#'   \item \code{"no"}: show links with no labels;
#'   \item \code{NA}: show no labels or links (same for \code{p_lbl = NULL}, default).
#'   }
#'
#' @param arr_c Arrow code for symbols at ends of probability links
#' (as a numeric value \code{-3 <= arr_c <= +6}),
#' with the following options:
#'   \itemize{
#'   \item \code{-1} to \code{-3}: points at one/other/both end/s;
#'   \item \code{0}: no symbols;
#'   \item \code{+1} to \code{+3}: V-arrow at one/other/both end/s;
#'   \item \code{+4} to \code{+6}: T-arrow at one/other/both end/s.
#' }
#' Default: \code{arr_c = -3} (points at both ends).
#'
#' @param col_p Colors of probability links (as vector of 3 colors).
#' Default: \code{col_p = c(grey(.15, .99), "yellow", "yellow")}.
#' (Also consider: "black", "cornsilk", "whitesmoke").
#'
#' @param brd_dis Distance of probability links from area border
#' (as proportion of area width).
#' Default: \code{brd_dis = .06}.
#' Note: Adjust to avoid overlapping labels.
#' Negative values show links outside of main area.
#'
#' @param lbl_txt Default label set for text elements.
#' Default: \code{lbl_txt = \link{txt}}.
#'
#' @param title_lbl Text label for current plot title.
#' Default: \code{title_lbl = txt$scen_lbl}.
#'
#' @param cex_lbl Scaling factor for text labels (frequencies and headers).
#' Default: \code{cex_lbl = .90}.
#'
#' @param cex_p_lbl Scaling factor for text labels (probabilities).
#' Default: \code{cex_p_lbl = cex_lbl - .05}.
#'
#' @param col_pal Color palette.
#' Default: \code{col_pal = \link{pal}}.
#'
#' @param mar_notes Boolean option for showing margin notes.
#' Default: \code{mar_notes = FALSE}.
#'
#' @param ... Other (graphical) parameters.
#'
#' @return Nothing (NULL).
#'
#' @examples
#' # plot_crisk(x = seq(0, 100, by = 10),
#' #            y = c(0, 0, 0, 10, 30, 50, 70, 80, 0, 0, 0))
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
#'
#' @importFrom stats smooth.spline
#' @importFrom stats predict
#'
#' @family visualization functions


## (2) plot_crisk: Definition ------

plot_crisk <- function(x,  # x-values (as vector)
                       y,  # y-values (as vector)

                       x_from = NA, # start value of x-increment
                       x_to = NA,   # end value of x-increment

                       fit_curve = FALSE,  # fit a curve to x-y-data?

                       ## Legacy:
                       prev = num$prev,    # probabilities
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,
                       N = num$N,          # population size N

                       ## Plot options:
                       by = "cddc",        # 2 perspectives (top + left): by = "cd" "dc" "ac"  (default: "cddc")
                       p_split = "v",      # primary/perspective split: "v": vertical vs. "h": horizontal
                       area = "sq",        # sq" (default: correcting x-values for aspect ratio of current plot) vs. "no" (NA, NULL, "fix", "hr" )
                       scale = "p",        # "p": exact probabilities (default) vs. "f": Re-compute prob from (rounded or non-rounded) freq.
                       round = TRUE,       # round freq values to integers? (default: round = TRUE), when not rounded: n_digits = 2 (currently fixed).
                       sample = FALSE,     # sample freq values from probabilities?

                       ## Freq boxes:
                       sum_w = .10,        # border width: (default: sum_w = .10), setting sum_w = NULL/NA/<=0  hides top and left panels.
                       gaps = c(NA, NA),   # c(v_gap, h_gap). Note: c(NA, NA) is changed to defaults: c(.02, 0) if p_split = "v"; c(0, .02) if p_split = "h".

                       f_lbl = "num",      # freq label: "def" vs. "abb"/"nam"/"num"/"namnum". (Set to "no"/NA/NULL to hide freq labels).
                       f_lbl_sep = NA,     # freq label separator (default: " = ", use ":\n" to add an extra line break)
                       f_lbl_sum = "num",  # freq label of summary cells (bottom row and right column)
                       f_lbl_hd  = "nam",  # freq labels of headers at top (for columns) and left (for rows)
                       f_lwd = 0,          # lwd of freq boxes: 0 (set to tiny_lwd, lty = 0) vs. 1 (numeric), or NULL/NA (set to 0).
                       # f_lty = 0,        # lty of freq boxes: 1 ("solid") vs. 0 ("blank"), etc. (currently not used)

                       ## Prob links:
                       p_lbl = NA,         # prob label: "def" (default) vs. "abb"/"nam"/"num"/"namnum". (Set to "no"/NA/NULL to hide prob lines).
                       # p_lwd,            # lwd of prob links: set to default = 1 (currently not used)
                       # p_lty,            # lty of prob links: set to default = 1 (currently not used)
                       arr_c = -3,         # arrow code (-3 to +6): 0: no arrow, 1--3: V-shape, 4--6: T-shape, -1 to -3: point at ends.
                       col_p = c(grey(.15, .99), "yellow", "yellow"),  # colors for prob-links: use 1-3 bright colors (visible on SDT rectangles). WAS: "black", "cornsilk", "whitesmoke"
                       brd_dis = .06,      # distance of prob links from border. (Adjust to avoid overlapping labels).

                       ## Text and color:
                       lbl_txt = txt,        # labels and text elements
                       title_lbl = "Title",  # main plot title
                       cex_lbl = .90,        # size of freq & text labels
                       cex_p_lbl = NA,       # size of prob labels (set to cex_lbl - .05 by default)
                       col_pal = pal_crisk,  # color palette

                       ## Generic options:
                       mar_notes = FALSE,   # show margin notes?
                       ...                  # other (graphical) parameters (passed to plot_line and plot_ftype_label)
) {

  ## (0) Initialize: ------

  delta_x_specified <- FALSE

  ## (1) Check inputs: --------

  # (a) Data in x and y:
  if (length(x) != length(y)){
    message("plot_crisk: x and y must have the same length.")
    return(NA)
  }

  if (any(diff(y) < 0)){ message("plot_crisk: y is assumed to be monotonically increasing.") }

  # (b) Values x_from and x_to:
  if ((!is.na(x_from)) && (x_from < min(x))) { message("plot_crisk: x_from is lower than min(x).") }

  if ((!is.na(x_to)) && (x_to > max(x))) { message("plot_crisk: x_to exceeds max(x).") }

  if (!is.na(x_from) && (!is.na(x_to))){ # x-interval specified:

    delta_x_specified <- TRUE

    if (x_to < x_from){
      message("plot_crisk: x_from exceeds x_to: Swapping values.")
      x_temp <- x_from
      x_from <- x_to
      x_to <- x_temp
    }

    # Need to fit a curve to x-y-data?
    if (!fit_curve & (!(x_from %in% x) | !(x_to %in% x))){

      # Require fit_curve:
      message("plot_crisk: x_from OR x_to NOT in x: Using fit_curve = TRUE.")
      fit_curve <- TRUE

    }

    # Compute delta-x:
    delta_x <- (x_to - x_from)


  } # if x-interval specified.


  ## (2) Compute values: --------

  # (a) Plot ranges:

  if (delta_x_specified){
    x_min <- min(x, x_from, x_to)  # 0
    x_max <- max(x, x_from, x_to)
  } else {
    x_min <- min(x)  # 0
    x_max <- max(x)
  }

  y_min <-   0  # min(y)
  y_max <- 100  # max(y)
  if (max(y) < 50) { y_max <- max(y) }  # small y-values

  x_range <- c(x_min, x_max)
  y_range <- c(y_min, y_max)

  # (b) Risk increments (of y-values):
  rinc_y <- incsum(y)

  # (c) Fit curve:
  if (fit_curve){

    # 1. Fit a curve to x-y-values:
    # plot(x, y)

    ## (a) Loess:
    # fit_loess <- stats::loess(y ~ x)
    # lines(fit_loess, col = pal_seeblau[[2]], lwd = 1)
    # lines(stats::predict(fit_loess), col = pal_seeblau[[4]], lwd = 1, lty = 2)

    ## (b) smoothing spline:
    fit_spline <- stats::smooth.spline(x, y, spar = .10)
    # lines(fit_spline, col = pal_pinky[[2]], lwd = 1)
    # lines(stats::predict(fit_spline), col = pal_pinky[[4]], lwd = 1, lty = 2)

    ## (c) Predict a range of points:
    # y_pred <- stats::predict(fit_spline, x = seq(10, 90, 1))
    # y_pred

    ## correct extreme predictions:
    # y_pred$y[y_pred$y < min(y)] <- min(y)  # minima
    # y_pred$y[y_pred$y > max(y)] <- max(y)  # maxima
    # y_pred


    # 2. Predict y-values of delta-x-interval:
    if (delta_x_specified){

      ## (d) Predict a PAIR of y-values (for x_from and x_to):
      # y_pred <- stats::predict(fit_spline, x = seq(x_from, x_to, length.out = 2))
      # y_pred$y[y_pred$y < min(y)] <- min(y)  # minima
      # y_pred$y[y_pred$y > max(y)] <- max(y)  # maxima
      # y_pred

      ## (e) Predict a SEQUENCE of y-values (for x_from and x_to):
      y_pred <- stats::predict(fit_spline, x = seq(x_from, x_to, by = 1))
      y_pred$y[y_pred$y < min(y)] <- min(y)  # minima
      y_pred$y[y_pred$y > max(y)] <- max(y)  # maxima
      # y_pred

      # lines(y_pred, lwd = 2, col = make_transparent(Seeblau, alpha = 2/3))

    }

  } # if fit_curve etc.


  # (d) Compute y-interval:
  if (delta_x_specified){

    # y_from:
    if (x_from %in% x){

      y_from <- y[x == x_from]  # use existing data value

    } else {

      if (fit_curve) {
        y_from <- y_pred$y[1]  # use 1st predicted y-value
      } else {
        message("Either provide x_from data OR use fit_curve == TRUE.")
      }

    }

    # y_to:
    if (x_to %in% x){

      y_to <- y[x == x_to]  # use existing data value

    } else {

      if (fit_curve) {
        y_to <- y_pred$y[length(y_pred$y)]  # use last predicted y-value
      } else {
        message("Either provide x_to data OR use fit_curve == TRUE.")
      }

    }

    # Compute delta-y:
    delta_y <- (y_to - y_from)

  } # if (delta_x_specified) end.


  ## (3) Plot parameters: --------

  ## (A) Generic:

  opar <- par(no.readonly = TRUE)  # copy of current settings
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## (B) User input parameters:

  ## Additional parameters (currently fixed): ----

  lty <- 1  # default

  # Labels:
  x_unit <- "years"
  x_ax_lbl <- paste0("Age (in ", x_unit, ")")
  y_ax_lbl <- "Risk"

  # Sizes:
  cex_axs  <- 1.0
  cex_txt  <- .95
  cex_pts  <- 2.0

  # Switches:
  show_inc  <- TRUE  # FALSE
  show_pass <- TRUE  # FALSE
  show_rem  <- TRUE  # FALSE
  show_aux  <- TRUE  # FALSE


  ## (4) Define plot and margin areas: --------

  ## Define margin areas:

  if (nchar(title_lbl) > 0) { n_lines_top <- 4 } else { n_lines_top <- 3 }
  if (mar_notes) { n_lines_bot <- 5 } else { n_lines_bot <- 4 }

  par(mar = c(n_lines_bot, 4, n_lines_top, 3) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.

  ## Axis label locations:
  par(mgp = c(3, 1, 0)) # default: c(3, 1, 0)

  ## Orientation of the tick mark labels (and corresponding mtext captions below):
  par(las = 0)  # Options: parallel to the axis (0 = default), horizontal (1), perpendicular to axis (2), vertical (3).


  ## (5) Plot setup: --------

  ## Plot dimensions:

  ## Draw empty plot:
  plot(0, 0, type = "n",      # type = "n" hides the points
       xlab = "x", ylab = "y",
       xlim = c(x_min, x_max), ylim = c(y_min, y_max),
       axes = FALSE)

  ## Axes:
  axis(side = 1, las = 1) # x-axis, horizontal labels
  axis(side = 2, las = 2) # y-axis, horizontal labels

  ## Grid:
  grid(nx = NULL, ny = NULL,  # NULL: at tick marks, NA: no lines
       col = grey(.75, .99), lty = 2, lwd = par("lwd"), equilogs = TRUE)


  ## (+) Draw plot points: --------

  ## Grid of points:
  # grid_x <- rep(seq(0, 1, by = .25), times = length(seq(0, 1, by = .25))) # x/horizontal
  # grid_y <- rep(seq(0, 1, by = .25), each =  length(seq(0, 1, by = .25))) # y/vertical
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)        # grid points

  # points(grid_x * scale_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)  # grid points (scaled)
  # points(0, 0, pch = 1, col = grey(.33, .50), cex = 1)  # mark origin


  ## (+) Main: Custom crisk plot: ---------


  # +++ here now +++

  # (+) Plot points: ------

  if (delta_x_specified){

    col_from <- pal_seegruen[[4]]
    col_to   <- pal_bordeaux[[4]]

    points(x_from, y_from, pch = 21, cex = (cex_pts + 0), bg = make_transparent(col_from, alpha = .20), col = col_from)
    points(x_to,   y_to,   pch = 21, cex = (cex_pts + 0), bg = make_transparent(col_to,   alpha = .20), col = col_to)

  }


  # (+) Cumulative curve: ------

  col_cum <- make_transparent(col_pal["cum"], alpha = 1)  # make_transparent(Seeblau, alpha = .90)
  alf_cum <- .85

  if (!fit_curve){
    lines(x = x, y = y, lwd = 2, lty = 1, col = make_transparent(col_cum, alpha = alf_cum))  # actual values
  } else {
    lines(fit_spline, lwd = 2, lty = 1, col = make_transparent(col_cum, alpha = alf_cum))  # fitted values
  }

  points(x = x, y = y, pch = 20, cex = cex_pts, col = make_transparent(col_cum, alpha = alf_cum))




  ## (+) Plot other stuff: --------

  # box_else <- make_box("else_box", 9, -2, b_w, b_h)  # define some arbitrary box
  # plot(box_else, col = "firebrick1", cex = 1/2, font = 2)     # plot box


  ## (+) Title: ------

  # Define parts:
  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)

  if (title_lbl == "") {  # if title has been set to "":
    type_lbl <- ""        # assume that no subtitle is desired either
  } else {
    type_lbl <- paste0("Cumulative risk", " (by ", as.character("unit"), ")")  # plot type and unit
  }

  # Compose label:
  cur_title_lbl <- paste0(title_lbl, type_lbl)

  # Plot title:
  title(cur_title_lbl, adj = 0, line = 0, font.main = 1, cex.main = 1.2)  # (left, not raised, normal font)


  ## (+) Margins: ------

  if (mar_notes) {

    # Note:
    note_lbl <- ""  # initialize

    if (scale == "f") {
      note_lbl <- label_note(area = area, scale = scale)
    }

    plot_mar(show_freq = TRUE, show_cond = TRUE, show_dec = TRUE,
             show_accu = TRUE, accu_from_freq = FALSE,
             note = note_lbl,
             cur_freq = freq, cur_prob = prob, lbl_txt = lbl_txt)

  } # if (mar_notes) etc.


  ## Finish: -----

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible()# restores par(opar)

} # plot_crisk end.


## (3) Check: ------

# x <- seq(0, 100, by = 10)
# y <- c(0, 0, 0, 10, 25, 50, 75, 80, 85, 85, 85)
#
# plot_crisk(x, y)
# plot_crisk(x, y, fit_curve = TRUE)
# plot_crisk(x, y, x_from = 40, x_to = 60)  # provided points
# plot_crisk(x, y, x_from = 46, x_to = 65)  # predicted points
#
# # small y-values and linear increase:
# plot_crisk(x = 1:10, y = seq(1, 10, by = 1), x_from = 4,   x_to = 6)    # provided points
# plot_crisk(x = 1:10, y = seq(1, 10, by = 1), x_from = 4.5, x_to = 6.5)  # predicted points



## (+) ToDo: ------

## - etc.


## (*) Done: ----------

## - etc.

## eof. ----------
