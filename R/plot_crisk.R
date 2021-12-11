## plot_crisk.R | riskyr
## 2021 12 11
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
#' @param x_to End value of risk increment.
#'
#' @param fit_curve Boolean: Fit a curve to \code{x}-\code{y}-data?
#' Default: \code{fit_curve = FALSE}.
#'
#' @param show_inc Boolean: Show risk increments?
#' Default: \code{show_inc = FALSE}.
#'
#' @param show_pas Boolean: Show past/passed risk?
#' Default: \code{show_pas = TRUE}.
#'
#' @param show_rem Boolean: Show remaining risk?
#' Default: \code{show_rem = TRUE}.
#'
#' @param show_aux Boolean: Show auxiliary elements (lines and points)?
#' Default: \code{show_aux = TRUE}.
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
#' Default: \code{title_lbl = "Cumulative risk"}.
#'
#' @param x_lbl Text label of x-axis (at bottom).
#' Default: \code{x_lbl = "Age (in years)"}.
#'
#' @param y_lbl Text label of y-axis (on left).
#' Default: \code{y_lbl = "Population risk"}.
#'
#' @param y2_lbl Text label of 2nd y-axis (on right).
#' Default: \code{y2_lbl = ""} (formerly "Remaining risk").
#'
#' @param cex_lbl Scaling factor for text labels.
#' Default: \code{cex_lbl = .95}.
#'
#' @param cex_p_lbl Scaling factor for text labels (probabilities).
#' Default: \code{cex_p_lbl = cex_lbl - .05}.
#'
#' @param col_pal Color palette.
#' Default: \code{col_pal = \link{pal}}.
#'
#' @param mar_notes Boolean option for showing margin notes.
#' Default: \code{mar_notes = TRUE}.
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
#' @importFrom graphics lines
#' @importFrom graphics segments
#' @importFrom graphics legend
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

                       show_inc = FALSE,  # Boolean: Show risk increments?
                       show_pas = TRUE,   # Boolean: Show past/passed risk?
                       show_rem = TRUE,   # Boolean: Show remaining risk?
                       show_aux = TRUE,   # Boolean: Show auxiliary elements (lines and points)?


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

                       title_lbl = "Cumulative risk", # plot title
                       x_lbl  = "Age (in years)",     # label of x-axis (at bottom)
                       y_lbl  = "Population risk",    # label of y-axis (on left)
                       y2_lbl = "",                   # label of 2nd y-axis (on right)

                       cex_lbl = .95,        # size of text labels
                       cex_p_lbl = NA,       # size of prob labels (set to cex_lbl - .05 by default)
                       col_pal = pal_crisk,  # color palette

                       ## Generic options:
                       mar_notes = TRUE,    # show margin notes?
                       ...                  # other (graphical) parameters (passed to plot_line and plot_ftype_label)
) {

  ## (0) Initialize: ------

  # Labels:
  # x_unit  <- "years"
  x_ax_lbl  <- x_lbl   # paste0("Age (in ", x_unit, ")")
  y_ax_lbl  <- y_lbl   # "Population risk"
  y2_ax_lbl <- y2_lbl  # "Remaining risk"

  # Sizes:
  cex_axs  <- 1.0
  cex_pts  <- 2.0

  # More Boolean switches:
  show_poly  <- show_aux
  show_delta <- show_aux
  show_popu  <- show_aux
  show_high  <- show_aux
  show_vals  <- show_aux
  show_grid  <- show_aux

  delta_x_specified <- FALSE


  ## (1) Check user inputs: --------

  # (a) Data in x and y:
  if (length(x) != length(y)){
    message("plot_crisk: x and y must have the same length.")
    return(NA)
  }

  if (any(diff(y) < 0)){ message("plot_crisk: y is assumed to be monotonically increasing.") }

  # (b) Values x_from and x_to:
  if ((!is.na(x_from)) && (x_from < min(x))) { message("plot_crisk: x_from is lower than min(x).") }

  if ((!is.na(x_to)) && (x_to > max(x))) { message("plot_crisk: x_to exceeds max(x).") }

  if ((is.na(x_from)) && (show_pas)) {
    message("plot_crisk: Showing passed risk requires x_from.")
    show_pas <- FALSE
  }

  if ((is.na(x_from)) && (show_pas)) {
    message("plot_crisk: Showing passed risk requires x_from.")
    show_pas <- FALSE
  }

  if ((is.na(x_from)) && (show_rem)) {
    message("plot_crisk: Showing remaining risk requires x_from.")
    show_rem <- FALSE
  }

  if (is.na(x_from) && (!is.na(x_to))){
    message("plot_crisk: x_to without x_from. Using x_from <- min(x).")
    x_from <- min(x)
  }

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
      message("plot_crisk: x_from or x_to not in x: Using fit_curve = TRUE.")
      fit_curve <- TRUE

    }

    # Compute delta-x:
    delta_x <- (x_to - x_from)

  } # if x-interval specified.

  # (+) Other user inputs:

  # if (is.na(title_lbl)) {title_lbl <- ""}


  ## (2) Compute plot values: --------

  # (a) Plot ranges:

  if (delta_x_specified){
    x_min <- min(x, x_from)  # 0
    x_max <- max(x, x_to)
  } else {
    x_min <- min(x)  # 0
    x_max <- max(x)
  }

  y_min <- 0  # min(y)

  if ((max(y) < 50) | (max(y) > 100)) { # small or large scale:
    y_max <- max(y)
  } else {
    y_max <- 100
  }

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


  # (d) Compute y-values:

  if (!is.na(x_from)){ # compute y_from:

    if (x_from %in% x){

      y_from <- y[x == x_from]  # use existing data value

    } else {

      if (fit_curve) {
        y_from <- y_pred$y[1]  # use 1st predicted y-value
      } else {
        message("Either provide x_from data OR use fit_curve == TRUE.")
      }

    }

    # Population parts:
    popu_pas <- y_from          # past/passed population proportion
    popu_rem <- (100 - y_from)  # remaining population proportion

    # Remaining risk:
    risk_max <- ((y_max - y_from)/popu_rem) * 100  # maximum remaining risk


  } # if (x_from) end.


  if (!is.na(x_to)){ # compute y_to:

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

    # Risk increment (of delta_y):
    risk_delta <- (delta_y / popu_rem) * 100  # current risk increment (from y_from to y_to)

  } # if (x_to) end.


  ## (3) Plot parameters: --------

  ## (A) Generic:

  opar <- par(no.readonly = TRUE)  # copy of current settings
  on.exit(par(opar))  # par(opar)  # restore original settings

  ## (B) Additional plot parameters (currently fixed): ----

  lty <- 1  # default


  ## (4) Define plot and margin areas: --------

  ## Define margin areas:

  # if (nchar(title_lbl) > 0) { n_lines_top <- 4 } else { n_lines_top <- 3 }
  # if (mar_notes) { n_lines_bot <- 5 } else { n_lines_bot <- 4 }
  n_lines_top <- 3
  n_lines_bot <- 5

  par(mar = c(n_lines_bot, 4, n_lines_top, 3.4) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.1)                      # outer margins; default: par("oma") = 0 0 0 0.

  # Axis label locations:
  par(mgp = c(3, 1, 0))  # default: c(3, 1, 0)

  # Orientation of the tick mark labels (and corresponding mtext captions below):
  par(las = 0)  # Options: parallel to the axis (0 = default), horizontal (1), perpendicular to axis (2), vertical (3).


  ## (5) Plot setup: --------

  # Plot dimensions:

  # Draw empty plot:
  plot(0, 0, type = "n",      # type = "n" hides the points
       xlab = x_ax_lbl, ylab = y_ax_lbl, cex.axis = cex_axs,
       xlim = c(x_min, x_max), ylim = c(y_min, y_max),
       axes = FALSE)

  # Axes:
  axis(side = 1, las = 1, cex.axis = cex_axs)  # x-axis, horizontal labels
  axis(side = 2, las = 1, cex.axis = cex_axs)  # y-axis, horizontal labels

  # Grid:
  if (show_grid){
    grid(nx = NULL, ny = NULL,  # NA: no lines; NULL: at tick marks
         col = grey(.50, alpha = .50), lty = 1, lwd = par("lwd")/3, equilogs = TRUE)
  }

  ## (+) Draw plot points: --------

  # # Grid of points:
  # grid_x <- rep(seq(x_min, x_max, by = 10), times = length(seq(x_min, x_max, by = 10))) # x/horizontal
  # grid_y <- rep(seq(y_min, y_max, by = 10), each =  length(seq(y_min, y_max, by = 10))) # y/vertical
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)        # grid points
  #
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)  # grid points (scaled)
  # points(0, 0, pch = 1, col = grey(.33, .50), cex = 1)  # mark origin


  ## (+) Main: Custom crisk plot: ---------

  # Constants:
  x_adj  <- (3 * x_max/100)  # x-value adjustment (for labels next to y-axes)

  # Colors:
  col_txt <- make_transparent(col_pal["txt"], alpha = 1)
  alf_txt <- 1.0

  col_aux <- make_transparent(col_pal["aux"], alpha = 1)
  alf_aux <- .90

  col_pass <- make_transparent(col_pal["pass"], alpha = 1)
  alf_pass <- .25

  col_rem <- make_transparent(col_pal["rem"], alpha = 1)
  alf_rem <- .25

  col_poly <- make_transparent(col_pal["poly"], alpha = 1)
  alf_poly <- .25

  col_popu <- make_transparent(col_pal["popu"], alpha = 1)
  alf_popu <- 1

  col_delta <- make_transparent(col_pal["delta"], alpha = 1)

  col_rinc <- make_transparent(col_pal["rinc"], alpha = 1)

  col_cum <- make_transparent(col_pal["cum"], alpha = 1)
  alf_cum <- .85

  col_hi <- make_transparent(col_pal["hi"], alpha = 1)  # Pinky
  alf_hi <- .85


  # (1) Rectangles: ------

  x_rfin <- x_max  # right end of rectangles

  # (a) past/passed risk:
  if (!is.na(x_from) & show_pas){

    rect(xleft = x_min, ybottom = y_min, xright = x_rfin, ytop = y_from,
         border = NA, density = NA, col = make_transparent(col_pass, alpha = alf_pass))
    text(x = (x_min + x_from/2), y = y_from, labels = "Passed risk",
         col = make_transparent(col_txt, alpha = alf_aux), cex = cex_lbl, font = 1, pos = 1, xpd = TRUE)  # past/passed risk label

    # Show 1st point (x_from y_from):
    points(x_from, y_from, pch = 21, cex = (cex_pts - 0.3),
           col = col_aux, bg = make_transparent(col_pass, alpha = .40), lwd = 1.5)

  } # if (show_pas) end.

  # (b) remaining risk:
  if (!is.na(x_from) & show_rem){

    rect(xleft = x_from, ybottom = y_from, xright = x_rfin, ytop = y_max,
         border = NA, density = NA, col = make_transparent(col_rem, alpha = alf_rem))
    text(x = (x_from + (x_max - x_from)/2), y = y_max, labels = "Remaining risk",
         col = make_transparent(col_txt, alpha = alf_aux), cex = cex_lbl, font = 1, pos = 1, xpd = TRUE)  # remaining risk label

    # Show 1st point (x_from y_from):
    if (!is.na(x_from) & !show_pas){  # 1st point not yet drawn:
      points(x_from, y_from, pch = 21, cex = (cex_pts - 0.3),
             col = col_aux, bg = make_transparent(col_pass, alpha = .40), lwd = 1.5)
    }

  } # if (show_rem) end.


  # (2) Polygon of risk increment: ------

  if (show_poly & delta_x_specified){

    # (a) Define polygon boundaries:
    if ((x_from %in% x) & (x_to %in% x)){

      # use existing data values:
      y_upper <- y[(x >= x_from) & (x <= x_to)]
      x_upper <- x[(x >= x_from) & (x <= x_to)]

    } else {

      if (fit_curve) {

        y_upper <- y_pred$y  # use predicted y-values
        x_upper <- y_pred$x  # use predictors

      } else {

        message("Either provide x_from data OR use fit_curve == TRUE.")

      }

    } # if (x_from/x_to in x) end.

    y_lower <- rep(y_from, length(y_upper))
    x_lower <- rev(x_upper)

    xxp <- c(x_upper, x_lower)  # x-values
    yyp <- c(y_upper, y_lower)  # y-values

    # (b) Draw polygon:

    # polygon(xxp, yyp, border = NA, density = 20, col = make_transparent(col_delta, alpha = 1))  # lined polygon, OR
    polygon(xxp, yyp, border = NA, density = NA, col = make_transparent(col_poly, alpha = alf_poly))  # filled polygon

  } # if (show_poly) end.


  # (+) Delta-x and delta-y segments: ------

  if (show_delta & delta_x_specified){

    f_1 <- 1/2  # scaling factor for delta label position

    # (a) delta-x (horizontal):
    segments(x0 = x_from, y0 = y_from, x1 = x_to, y1 = y_from, lwd = 1.5, lty = 1, col = col_delta)

    dx_lbl <- round(delta_x, 1)  # paste0("dx = ", round(delta_x, 1))

    if (show_vals){
      text(x = (x_from + (delta_x * f_1)), y = y_from, labels = dx_lbl,
           col = col_delta, cex = cex_lbl, font = 2, pos = 1)  # delta-x label
    }

    # (b) delta-y (vertical):
    segments(x0 = x_to, y0 = y_from, x1 = x_to, y1 = y_to, lwd = 1.5, lty = 1, col = col_delta)

    dy_lbl <- paste0(round(delta_y, 1), "%")  # paste0("dy = ", round(delta_y, 1), "%")

    if (show_vals){
      text(x = x_to, y = (y_from + (delta_y * f_2)), labels = dy_lbl,
           col = col_delta, cex = cex_lbl, font = 2, pos = 4)  # delta-y label
    }
  }


  # (+) Show 2nd point (x_to y_to): ------

  if (delta_x_specified & (show_aux | show_poly | show_high)){

    # Point (x_to y_to):
    points(x_to, y_to, pch = 21, cex = (cex_pts - 0.3),
           col = col_aux, bg = make_transparent(col_rem, alpha = .40), lwd = 1.5)

  }


  # (+) 2nd axis for remaining risk (y2-axis on right): ------

  if (show_rem){

    n_aiv  <- 5  # number of axis intervals

    y2_seq <- seq(y_from, y_max, length.out = (n_aiv + 1))
    y2_lbl <- seq(0, round(risk_max, 1), length.out = (n_aiv + 1))  # label all intervals
    y2_lbl <- c("0", rep(NA, n_aiv - 1), round(risk_max, 1))        # label only extrema

    axis(side = 4, pos = (x_max + 0), at = y2_seq, labels = y2_lbl,
         las = 1, cex.axis = cex_axs)  # y at right


    # # y-value of y2_lbl (as adj-value from 0 to 1):
    # if (y_max < 100){ # scale label position by risk_max:
    #   pos_y2 <- (y_max - (.50 * (y_max - y_from)))/y_max
    # } else { # scale label position by y_max (default):
    pos_y2 <- (y_max - (.35 * (y_max - y_from)))/y_max
    # }

    if (!is.na(y2_ax_lbl) && (nchar(y2_ax_lbl) > 0)){  # label y2-axis (on right):
      mtext(y2_ax_lbl, adj = pos_y2, side = 4, line = 2)
    }
  }


  # (+) Y-increments: ------

  if (show_inc){

    # lines(x = x, y = rinc_y, lwd = 2, lty = 1, col = col_rinc)
    segments(x0 = x, y0 = 0, x1 = x, y1 = rinc_y, lwd = 2, lty = 1, col = col_rinc)
    points(x = x, y = rinc_y, pch = 21, cex = (cex_pts - 0.3), col = col_rinc, bg = NA, lwd = 1.5)

  }


  # (+) Auxiliary elements: ------

  if (show_aux){

    lwd_aux <- 1
    lty_aux <- 2
    lbl_pos <- NULL   # NULL or 1: bot, 2: left, 3: top, 4: right
    x_lbl_l <- (x_min + x_adj)  # x-value of text labels (on left)

    # (a) Aux lines and label for (x_from, y_from):
    if (!is.na(x_from)){

      segments(x0 = x_from, y0 = 0, x1 = x_from, y1 = y_from, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x-from from bot (vertical)

      segments(x0 = x_min, y0 = y_from, x1 = x_from, y1 = y_from, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # y-from to left (horizontal)

      if (show_vals){
        text(x = x_lbl_l, y = y_from, labels = paste0(round(y_from, 1), "%"), pos = lbl_pos,
             cex = cex_lbl, font = 2, col = make_transparent(col_txt, alpha = 1), xpd = TRUE)  # y_from label (on left axis)
      }
    }

    # (b) Aux lines and label for (x_to, y_to):
    if (!is.na(x_to)){

      segments(x0 = x_to, y0 = 0, x1 = x_to, y1 = y_to, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x-to from bot (vertical)

      segments(x0 = x_min, y0 = y_to, x1 = x_to, y1 = y_to, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux), xpd = TRUE)  # y-to to left (horizontal)

      if (show_vals){
        text(x = x_lbl_l, y = y_to, labels = paste0(round(y_to, 1), "%"), pos = lbl_pos,
             cex = cex_lbl, font = 2, col = make_transparent(col_txt, alpha = 1), xpd = TRUE)  # y_to label (on left axis)
      }
    }

    # (d) Aux line and label for y_max (to left axis):
    if (delta_x_specified & (y_max != 100)){  # only if y_max is not 100 (trivial case):

      segments(x0 = x_min, y0 = y_max, x1 = x_max, y1 = y_max, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # y_max on top (horizontal)

      if (show_vals){
        text(x = x_lbl_l, y = y_max, labels = paste0(round(y_max, 1), "%"), pos = lbl_pos,
             cex = cex_lbl, font = 2, col = make_transparent(col_txt, alpha = 1), xpd = TRUE)  # y_max label (on left axis)
      }
    }

    # (c) Aux lines for remaining risk (to right axis):
    if (show_rem & delta_x_specified){

      segments(x0 = x_from, y0 = y_from, x1 = x_max, y1 = y_from, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x/y-from to right (horizontal)
      segments(x0 = x_to, y0 = y_to, x1 = x_max, y1 = y_to, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x/y-to to right (horizontal)
    }

    # (e) Population segments:
    if (show_popu & !is.na(x_from)){

      # lower segment (passed risk):
      x_lo <- x_min  # x_from  # x of lower population part (vertical)
      f_2  <- 1/2     # scaling factor for population label position

      # segments(x0 = x_lo, y0 = y_min, x1 = x_lo, y1 = popu_pas, lwd = 2, lty = 1,
      #          col = make_transparent(col_pass, alpha = 1))  # passed-y (vertical)
      #
      # if (show_vals){
      #   text(x = x_lo, y = (y_from * f_2), labels = paste0(round(popu_pas, 1), "%"),
      #        col = make_transparent(col_txt, alpha = alf_aux), cex = cex_lbl, font = 2, pos = 4, xpd = TRUE)  # y-from/passed risk label
      # }

      # upper segment (remaining risk):
      x_up <- x_from # x_lo  # x of upper population part (vertical)
      y_up <- y_from + (y_max - y_from) * f_2  # y of remaining population label

      segments(x0 = x_up, y0 = y_from, x1 = x_up, y1 = y_max, lwd = 2, lty = 1,
               col = make_transparent(col_popu, alpha = alf_popu))  # remaining-y (vertical)

      if (show_vals){
        text(x = x_up, y = y_up, labels = paste0(round(popu_rem, 1), "%"),
             col = make_transparent(col_popu, alpha = 1), cex = cex_lbl, font = 2, pos = 2, xpd = TRUE)  # remaining population proportion label
      }

    }

    # (e) 1st point (x_from x_to):
    if (!is.na(x_from) & !show_pas & !show_rem){  # 1st point not yet drawn:

      # Show 1st point (x_from y_from):
      points(x_from, y_from, pch = 21, cex = (cex_pts - 0.3),
             col = col_aux, bg = make_transparent(col_pass, alpha = .40), lwd = 1.5)

    }

  } # if (show_aux) end.


  # (+) Cumulative curve: ------

  if (!fit_curve){
    lines(x = x, y = y, lwd = 2, lty = 1, col = make_transparent(col_cum, alpha = alf_cum))  # actual values
  } else {
    lines(fit_spline, lwd = 2, lty = 1, col = make_transparent(col_cum, alpha = alf_cum))  # fitted values
  }

  points(x = x, y = y, pch = 20, cex = cex_pts, col = make_transparent(col_cum, alpha = alf_cum))


  # (+) Highlight remaining risk delta_risk (on right axis): ------

  if (show_high & delta_x_specified){

    segments(x0 = x_from, y0 = y_to, x1 = (x_max + x_adj), y1 = y_to, lwd = 1, lty = 2,
             col = make_transparent(col_hi, alpha = alf_hi), xpd = TRUE)  # y-to level (horizontal)

    if (show_vals){
      text(x = (x_max + x_adj/2), y = y_to, labels = paste0(round(risk_delta, 1), "%"), pos = 4,
           cex = cex_lbl, font = 2, col = make_transparent(col_hi, alpha = alf_hi), xpd = TRUE)  # risk_delta label (on right axis)
    }

  }


  ## (+) Plot other stuff: --------

  # box_else <- make_box("else_box", 9, -2, b_w, b_h)  # define some arbitrary box
  # plot(box_else, col = "firebrick1", cex = 1/2, font = 2)     # plot box


  ## (+) Title: ------

  # if (is.na(title_lbl) || (title_lbl == "")){
  #   cur_title_lbl <- ""  # empty title
  # } else {
  #   cur_title_lbl <- title_lbl
  # }

  # Plot title: # (left, not raised, normal font)
  title(title_lbl, col = col_txt, adj = 0, line = 1, font.main = 1, cex.main = 1.2)


  ## (+) Margins: ------

  if (mar_notes) {

    # Content of note:
    if (fit_curve){
      note_lbl <- "(Using fitted data.)"
    } else {
      note_lbl <- NA # "(Using data provided.)"
    }

    # parameters:
    m_col <- make_transparent(col_txt, alpha = alf_aux)
    m_cex <- (cex_lbl - .02)

    # mtext("side = 1, line = 2, adj = 0", side = 1, line = 2, adj = 0, col = m_col, cex = m_cex)  # left 2 top
    # mtext("side = 1, line = 3, adj = 0", side = 1, line = 3, adj = 0, col = m_col, cex = m_cex)  # left 3 mid
    # mtext("side = 1, line = 4, adj = 0", side = 1, line = 4, adj = 0, col = m_col, cex = m_cex)  # left 4 bot

    # mtext("side = 1, line = 2, adj = 1", side = 1, line = 2, adj = 1, col = m_col, cex = m_cex)  # right 2 top
    mtext(note_lbl, side = 1, line = 3, adj = 1, col = m_col, cex = m_cex)  # right 4 mid
    # mtext("side = 1, line = 3, adj = 1", side = 1, line = 3, adj = 1, col = m_col, cex = m_cex)  # right 3 bot

  } # if (mar_notes) etc.


  ## Finish: -----

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible()# restores par(opar)

} # plot_crisk end.


## (3) Check: ------

# # 1. Dense data:
# x <- seq(from = 0, to = 100, by = 5)
# i <- c(0, 0, 0, 0, 0, .035, .070, .145, .160, .120, .07, .03, .02, .0150, .0150, .0125, .0075, 0, 0, 0, 0)
# y <- cumsum(i) * 100  # as percentages
#
# # 2. sparse data:
# x <- seq(0, 100, by = 10)
# y <- c(0, 0, 0, 10, 24, 50, 72, 80, 83, 85, 85)
#
# plot_crisk(x, y)
# plot_crisk(x, y, x_from = 40)
# plot_crisk(x, y, x_from = 40, x_to = 60)
# plot_crisk(x, y, fit_curve = FALSE, title = "Plot title", mar_notes = TRUE)
# plot_crisk(x, y, x_from = 40, x_to = 60)  # provided points
# plot_crisk(x, y, x_from = 46, x_to = 76)  # predicted points
#
# # Small x- and y-values and linear increases:
# plot_crisk(x = 1:10, y = seq( 2, 20, by = 2), x_from = 4,   x_to = 8)    # provided points
# plot_crisk(x = 2:10, y = seq(12, 28, by = 2), x_from = 4.5, x_to = 8.5)  # predicted points
#
# # Good combinations:
# plot_crisk(x, y, x_from = 42, x_to = 62, show_rem = FALSE, show_aux = FALSE)  # show past/passed risk only
# plot_crisk(x, y, x_from = 42, x_to = 62, show_pas = FALSE, show_aux = FALSE)  # show remaining risk only
# plot_crisk(x, y, x_from = 42, x_to = 62, show_aux = FALSE) # hide auxiliary info
# plot_crisk(x, y, x_from = 42, x_to = 62, show_pas = FALSE, show_rem = FALSE, show_aux = TRUE) # show only auxiliary info
#
# # Note: Showing everything may overwhelm viewers:
# plot_crisk(x, y, x_from = 42, x_to = 62, show_inc = TRUE)
#
# # Text labels:
# plot_crisk(x, y, 40, 60, title_lbl = "Title", x_lbl = "X-label", y_lbl = "Y-label", y2_lbl = "Alternative Y")


## (+) ToDo: ------

## - etc.


## (*) Done: ----------

## - etc.

## eof. ----------
