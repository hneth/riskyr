## plot_crisk.R | riskyr
## 2021 12 15
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
#' probability density function (over cumulative risk
#' amounts represented by \code{y} as a function of \code{x}).
#'
#' A cumulative risk increment is computed for
#' an interval ranging from \code{x_from} to \code{x_to}.
#' If risk values for \code{x_from} or \code{x_to} are not provided
#' (i.e., in \code{x} and \code{y}),
#' a curve is fitted to predict \code{y} by \code{x}
#' (by \code{fit_curve = TRUE}).
#'
#' \code{plot_crisk} provides options for
#' showing/hiding various elements required for computing the
#' cumulative risk increment for instructional purposes.
#'
#' Color information is based on a vector with named
#' colors \code{col_pal = \link{pal_crisk}}.
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
#' @param show_pas Boolean: Show past/passed risk?
#' Default: \code{show_pas = FALSE}.
#'
#' @param show_rem Boolean: Show remaining risk?
#' Default: \code{show_rem = FALSE}.
#'
#' @param show_pop Boolean: Show population partitions?
#' Default: \code{show_pop = FALSE}.
#'
#' @param show_aux Boolean: Show auxiliary elements
#' (i.e., explanatory lines, points, and labels)?
#' Default: \code{show_aux = FALSE}.
#'
#' @param show_num Boolean: Show numeric values,
#' provided that \code{show_aux = TRUE}.
#' Default: \code{show_num = FALSE}.
#'
#' @param show_inc Boolean: Show risk increments?
#' Default: \code{show_inc = FALSE}.
#'
#' @param show_grid Boolean: Show grid lines?
#' Default: \code{show_grid = FALSE}.
#'
#' @param col_pal Color palette (as a named vector).
#' Default: \code{col_pal = \link{pal_crisk}}.
#'
#' @param arr_c Arrow code for symbols at ends of population links
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
#' @param mar_notes Boolean option for showing margin notes.
#' Default: \code{mar_notes = FALSE}.
#'
#' @param ... Other (graphical) parameters.
#'
#' @return Nothing (NULL).
#'
#' @examples
#' # Data:
#' x <- seq(0, 100, by = 10)
#' y <- c(0, 0, 0, 8, 24, 50, 70, 80, 83, 85, 85)
#'
#' # Basic versions:
#' plot_crisk(x, y)  # using data provided
#' plot_crisk(x, y, x_from = 40)  # use and mark 1 provided point
#' plot_crisk(x, y, x_from = 44)  # use and mark 1 provided point
#' plot_crisk(x, y, x_from = 40, x_to = 60)  # use 2 provided points
#' plot_crisk(x, y, x_from = 44, x_to = 64)  # use 2 predicted points
#' plot_crisk(x, y, fit_curve = TRUE)  # fitting curve to provided data
#'
#' # Training versions:
#' plot_crisk(x, y, 44, 64, show_pas = TRUE)  # past/passed risk only
#' plot_crisk(x, y, 44, 64, show_rem = TRUE)  # remaining risk only
#' plot_crisk(x, y, 44, 64, show_pas = TRUE, show_rem = TRUE) # both risks
#' plot_crisk(x, y, 44, 64, show_aux = TRUE)  # auxiliary lines + axis
#' plot_crisk(x, y, 44, 64, show_aux = TRUE, show_pop = TRUE)  # + population parts
#' plot_crisk(x, y, 44, 64, show_aux = TRUE, show_num = TRUE)  # + numeric values
#' plot_crisk(x, y, 44, 85, show_aux = TRUE, show_pop = TRUE, show_num = TRUE) # + aux/pop/num
#'
#' # Note: Showing ALL is likely to overplot/overwhelm:
#' plot_crisk(x, y, x_from = 47, x_to = 67, fit_curve = TRUE,
#'            show_pas = TRUE, show_rem = TRUE, show_aux = TRUE, show_pop = TRUE,
#'            show_num = TRUE, show_inc = TRUE, show_grid = TRUE, mar_notes = TRUE)
#'
#' # Small x- and y-values and linear increases:
#' plot_crisk(x = 2:10, y = seq(12, 28, by = 2), x_from = 4.5, x_to = 8.5,
#'            show_pas = TRUE, show_rem = TRUE, show_aux = TRUE, show_pop = TRUE,
#'            show_num = TRUE, show_inc = TRUE)
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics abline
#' @importFrom graphics rect
#' @importFrom graphics lines
#' @importFrom graphics segments
#' @importFrom graphics arrows
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#' @importFrom stats smooth.spline
#' @importFrom stats predict
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{pal_crisk}} corresponding color palette.
#'
#' @export

## (2) plot_crisk: Definition ------

plot_crisk <- function(x,  # x-values (as vector)
                       y,  # y-values (as vector)

                       x_from = NA, # start value of x-increment
                       x_to = NA,   # end value of x-increment

                       fit_curve = FALSE,  # fit a curve to x-y-data?

                       # Boolean plot options:
                       show_pas = FALSE,  # Boolean: Show past/passed risk?
                       show_rem = FALSE,  # Boolean: Show remaining risk?

                       show_pop = FALSE,  # Boolean: Show population partitions?
                       show_aux = FALSE,  # Boolean: Show auxiliary elements (lines and points)?
                       show_num = FALSE,  # Boolean: Show numeric values?

                       show_inc = FALSE,  # Boolean: Show risk increments?
                       show_grid = FALSE, # Boolean: Show grid lines?

                       # Colors and text labels:
                       col_pal = pal_crisk,  # color palette (as a named vector)
                       arr_c = -3,           # arrow code (-3 to +6): 0: no arrow, 1--3: V-shape, 4--6: T-shape, -1 to -3: point at ends.

                       title_lbl = "Cumulative risk", # plot title
                       x_lbl  = "Age (in years)",     # label of x-axis (at bottom)
                       y_lbl  = "Population risk",    # label of y-axis (on left)
                       y2_lbl = "",                   # label of 2nd y-axis (on right)

                       mar_notes = FALSE,  # show margin notes?
                       ...                 # other (graphical) parameters (passed to plot_line)
) {

  ## (0) Initialize: ------

  # Boolean switches:
  show_poly  <- show_aux  # show polygon of risk increment?
  show_delta <- show_aux  # show x- and y-increments?
  show_high  <- show_aux  # highlight numeric value of risk increment?

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

  # Need to fit a curve to x-y-data?

  if (!fit_curve && !is.na(x_from) && !(x_from %in% x)){  # (a) Require fit_curve for x_from:
    message("plot_crisk: x_from is not in x: Using fit_curve = TRUE.")
    fit_curve <- TRUE
  }

  if (!fit_curve && !is.na(x_to) && !(x_to %in% x)){  # (b) Require fit_curve for x_to:
    message("plot_crisk: x_to is not in x: Using fit_curve = TRUE.")
    fit_curve <- TRUE
  }


  ## (2) Compute required values: --------

  # (a) Delta-x interval: ----

  if (!is.na(x_from) && (!is.na(x_to))){ # x-interval specified:

    delta_x_specified <- TRUE

    if (x_to < x_from){
      message("plot_crisk: x_from exceeds x_to: Swapping values.")
      x_temp <- x_from
      x_from <- x_to
      x_to <- x_temp
    }

    # Compute delta-x:
    delta_x <- (x_to - x_from)

  } # if delta_x_specified end.


  # (b) Plot ranges: ----

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

  # x_range <- c(x_min, x_max)
  # y_range <- c(y_min, y_max)


  # (c) Risk increments (of y-values): ----

  rinc_y <- incsum(y)


  # (d) Fit a curve (to x/y-data provided): ----

  if (fit_curve){

    # 0. Use margin notes:
    mar_notes <- TRUE

    # 1. Fit a curve to x-y-values: ----

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


    # 2. Predict y-values (for x_from and x_to): ----

    # (2a) y_from corresponding ONLY to x_from:
    if (!is.na(x_from) & is.na(x_to)){

      y_pred <- stats::predict(fit_spline, x = x_from)

      # Correction 1a: Predicted values in x-range must be within y-range:
      if (x_from >= x_min){
        y_pred$y[y_pred$y < min(y)] <- min(y)  # reset minima
      }

    }

    # (2b) y_to corresponding ONLY to x_to:
    if (!is.na(x_to) & is.na(x_from)){

      y_pred <- stats::predict(fit_spline, x = x_to)

      # Correction 1b: Predicted values in x-range must be within y-range:
      if (x_to <= x_max){
        y_pred$y[y_pred$y > max(y)] <- max(y)  # reset maxima
      }

    }

    # (2c) Predict a SEQUENCE of y-values for risk increment (from x_from to x_to):
    if (delta_x_specified){

      # y_pred <- stats::predict(fit_spline, x = seq(x_from, x_to, by = 1))
      y_pred <- stats::predict(fit_spline, x = seq(x_from, x_to, length.out = 10))

      # Correction 1c: Predicted values in x-range must be within y-range:
      if ((x_from >= x_min) & (x_to <= x_max)){
        y_pred$y[y_pred$y < min(y)] <- min(y)  # reset minima
        y_pred$y[y_pred$y > max(y)] <- max(y)  # reset maxima
      }

      # lines(y_pred, lwd = 2, col = make_transparent(Seeblau, alpha = 2/3))
    }

    # Correction 2: Allow for new overall extremes:
    if (!is.na(x_from)) { y_min <- min(y_min, y_pred$y) }  # new predicted minimum
    if (!is.na(x_to  )) { y_max <- max(y_max, y_pred$y) }  # new predicted maximum

  } # if fit_curve etc.


  # (e) Compute y-values and increments: ----

  # 1. Compute y_from:
  if (!is.na(x_from)){

    if (x_from %in% x){

      y_from <- y[x == x_from]  # use existing data value

    } else {

      if (fit_curve) {
        y_from <- y_pred$y[1]  # use 1st predicted y-value
      } else {
        message("Either provide x_from data OR use fit_curve == TRUE.")
      }

    }

    # print(y_from)

    # Population parts:
    popu_pas <- y_from          # past/passed population proportion
    popu_rem <- (100 - y_from)  # remaining population proportion (TODO: Add population decrements)

    # Remaining risk:
    risk_max <- ((y_max - y_from)/popu_rem) * 100  # maximum remaining risk


  } # if (x_from) end.


  # 2. Compute y_to and the deltas/increments of y and risk:
  if (!is.na(x_to)){

    # a. Compute y_to:
    if (x_to %in% x){

      y_to <- y[x == x_to]  # use existing data value

    } else {

      if (fit_curve) {
        y_to <- y_pred$y[length(y_pred$y)]  # use last predicted y-value
      } else {
        message("Either provide x_to data OR use fit_curve == TRUE.")
      }

    }

    # b. Compute delta-y and risk increment (of delta_y):
    if (delta_x_specified){

      delta_y    <- (y_to - y_from)
      risk_delta <- (delta_y / popu_rem) * 100  # current risk increment (from y_from to y_to)

    }

  } # if (x_to) end.


  ## (3) Plot parameters: --------

  # (a) Generic: ----

  opar <- par(no.readonly = TRUE)  # copy of current settings
  on.exit(par(opar))  # par(opar)  # restore original settings

  # (b) Additional plot parameters (currently fixed): ----

  # Text labels:

  n_dig <- 1  # Number of digits used to display/print values

  # if (is.na(title_lbl)) {title_lbl <- ""}
  # x_unit  <- "years" # Defaults:
  x_ax_lbl  <- x_lbl   # paste0("Age (in ", x_unit, ")")
  y_ax_lbl  <- y_lbl   # "Population risk"
  y2_ax_lbl <- y2_lbl  # "Remaining risk"

  # Line parameters:
  lty_main <- 1
  lty_aux  <- 2
  lty_grid <- 1

  lwd_main <- par("lwd") * 2
  lwd_emph <- par("lwd") * 3/2
  lwd_axs  <- par("lwd")
  lwd_aux  <- par("lwd") * 2/3
  lwd_grid <- par("lwd") * 1/3

  # Sizes:
  cex_axs <- par("cex")
  cex_pts <- par("cex") * 2.0
  cex_lbl <- par("cex") * .95
  cex_tit <- par("cex") * 1.2  # main title
  cex_mar <- par("cex") * .92  # margin notes

  # Positions:
  x_adj  <- (3 * x_max/100)  # x-value adjustment (for labels next to y-axes)

  # X-values of population parts (vertical):
  x_lo <- x_from  # x_min # x_max # x of lower population part (vertical)
  x_up <- x_from  # x_lo  # x of upper population part (vertical)

  pos_aux <- NULL   # NULL or 1: bot, 2: left, 3: top, 4: right

  # Colors: Named colors from col_pal = pal_crisk:
  col_cum <- make_transparent(col_pal["cum"], alpha = 1)  # cumulative risk curve
  alf_cum <- .85

  col_rinc <- make_transparent(col_pal["rinc"], alpha = 1)  # relative risk increments

  col_txt <- make_transparent(col_pal["txt"], alpha = 1)
  alf_txt <- 1.0

  col_aux <- make_transparent(col_pal["aux"], alpha = 1)
  alf_aux <- .90

  col_hi <- make_transparent(col_pal["high"], alpha = 1)  # highlighting
  alf_hi <- .85

  col_pas <- make_transparent(col_pal["pas"], alpha = 1)
  alf_pas <- .25

  col_rem <- make_transparent(col_pal["rem"], alpha = 1)
  alf_rem <- .25

  col_poly <- make_transparent(col_pal["poly"], alpha = 1)
  alf_poly <- .25

  col_delta <- make_transparent(col_pal["delta"], alpha = 1)

  col_popu <- make_transparent(col_pal["popu"], alpha = 1)
  alf_popu <- 1

  col_mar <- make_transparent(col_txt, alpha = alf_aux)  # margin notes


  # (c) Plot and margin areas: ----

  # Define margin areas:
  n_lines_top <- 3  # if (nchar(title_lbl) > 0) { n_lines_top <- 4 } else { n_lines_top <- 3 }
  n_lines_bot <- 4  # if (mar_notes) { n_lines_bot <- 5 } else { n_lines_bot <- 4 }

  par(mar = c(n_lines_bot, 4, n_lines_top, 3.4) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(0, 0, 0, 0) + 0.2)                        # outer margins; default: par("oma") = 0 0 0 0.

  # Axis label locations:
  par(mgp = c(3, 1, 0))  # margin line of title and x/y-axes: default: c(3, 1, 0) with mgp[1] = title, mgp[2:3] = axis.

  # Orientation of the tick mark labels (and corresponding mtext captions below):
  par(las = 0)  # Options: parallel to the axis (0 = default), horizontal (1), perpendicular to axis (2), vertical (3).


  ## (4) Create basic plot: --------

  # (+) HACK: Only show remaining risk part of plat: ------
  only_remaining_risk <- FALSE # TRUE

  if (only_remaining_risk & !is.na(x_from)){

    # adjust plot origin:
    x_min <- x_from
    y_min <- y_from

    # remove some stuff:
    show_pas <- FALSE

  }


  # (A) Prepare empty plot: ------

  plot(0, 0, type = "n",      # type = "n" hides the points
       xlab = x_ax_lbl, ylab = y_ax_lbl, cex.axis = cex_axs,
       xlim = c(x_min, x_max), ylim = c(y_min, y_max),
       axes = FALSE)


  # (B) Axes: ------

  # (a) Primary X- and Y-axis: ----

  ax_pos_adj <- NA  # use NA (for default) or 0 (for no gaps)

  # 1. X-axis at bottom, horizontal labels:
  axis(side = 1, pos = (y_min - ax_pos_adj),
       las = 1, lwd = lwd_axs, cex.axis = cex_axs)

  # 2. Y-axis on left, horizontal labels:
  axis(side = 2, pos = (x_min - ax_pos_adj),
       las = 1, lwd = lwd_axs, cex.axis = cex_axs)


  # (b) Alternative Y-axis for remaining risk (2nd axis y2 on right): ------

  if ((!is.na(x_from)) & (show_aux | show_rem)){

    n_aiv  <- 5  # number of axis intervals (4 or 5)
    y2_seq <- seq(y_from, y_max, length.out = (n_aiv + 1))
    y2_lbl <- seq(0, round(risk_max, n_dig), length.out = (n_aiv + 1))  # label all intervals
    y2_lbl <- c("0", rep(NA, n_aiv - 1), round(risk_max, n_dig))        # label only extrema

    axis(side = 4, pos = (x_max + 0),        # NO gap between x_max and vertical y2-axis
         at = y2_seq, labels = y2_lbl, las = 1,
         lwd = lwd_axs, cex.axis = cex_axs)  # y at right


    # # Adjust y-value of y2_lbl (as an adj-value ranging from 0 to 1):
    # if (y_max < 100){ # scale label position by risk_max:
    #   adj_y2_lbl <- (y_max - (.50 * (y_max - y_from)))/y_max
    # } else { # scale label position by y_max (default):
    adj_y2_lbl <- (y_max - (.35 * (y_max - y_from)))/y_max
    # }

    if (!is.na(y2_ax_lbl) && (nchar(y2_ax_lbl) > 0)){  # label y2-axis (on right):
      mtext(y2_ax_lbl, adj = adj_y2_lbl, side = 4, line = 2)
    }

  }

  # (C) Grid: ------

  if (show_grid){
    grid(nx = NULL, ny = NULL,  # NA: no lines; NULL: at tick marks
         col = grey(.50, alpha = .50), lty = lty_grid, lwd = lwd_grid, equilogs = TRUE)
  }


  # (+) Draw plot points: ------

  # # Grid of points:
  # grid_x <- rep(seq(x_min, x_max, by = 10), times = length(seq(x_min, x_max, by = 10))) # x/horizontal
  # grid_y <- rep(seq(y_min, y_max, by = 10), each =  length(seq(y_min, y_max, by = 10))) # y/vertical
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)        # grid points
  #
  # points(grid_x, grid_y, pch = 3, col = grey(.66, .50), cex = 3/4)  # grid points (scaled)
  # points(0, 0, pch = 1, col = grey(.33, .50), cex = 1)  # mark origin


  ## (5) Main: Custom crisk plot: ---------

  # 1. Rectangles: ------

  x_rfin <- x_max  # right end of rectangles

  # (a) past/passed risk:
  if (!is.na(x_from) & show_pas){

    rect(xleft = x_min, ybottom = y_min, xright = x_rfin, ytop = y_from,
         border = NA, density = NA, col = make_transparent(col_pas, alpha = alf_pas))
    text(x = (x_min + x_from/2), y = y_from, labels = "Passed risk",
         col = make_transparent(col_txt, alpha = alf_aux), cex = cex_lbl, font = 1, pos = 1, xpd = TRUE)  # past/passed risk label

  } # if (show_pas) end.

  # (b) remaining risk:
  if (!is.na(x_from) & show_rem){

    rect(xleft = x_from, ybottom = y_from, xright = x_rfin, ytop = y_max,
         border = NA, density = NA, col = make_transparent(col_rem, alpha = alf_rem))
    text(x = (x_from + (x_max - x_from)/2), y = y_max, labels = "Remaining risk",
         col = make_transparent(col_txt, alpha = alf_aux), cex = cex_lbl, font = 1, pos = 1, xpd = TRUE)  # remaining risk label

  } # if (show_rem) end.


  # 2. Polygon of risk increment: ------

  if (delta_x_specified & show_poly){

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


  # 3. Delta-x and delta-y increments: ------

  if (delta_x_specified & (show_aux | show_delta)){

    f_1 <- 1/2  # scaling factor for label position

    # (a) delta-x (horizontal):
    segments(x0 = x_from, y0 = y_from, x1 = x_to, y1 = y_from,
             lty = lty_main, lwd = lwd_emph, col = col_delta)

    if (show_num){ # delta-x label:

      dx_lbl <- round(delta_x, n_dig)  # paste0("dx = ", round(delta_x, n_dig))

      # Label position:
      if (y_from > .10 * y_max){ # default:
        pos_dx <- 1  # bottom
      } else { # switch to top:
        pos_dx <- 3  # top
      }

      text(x = (x_from + (delta_x * f_1)), y = y_from, labels = dx_lbl,
           col = col_delta, cex = cex_lbl, font = 2, pos = pos_dx, xpd = TRUE)
    }

    # (b) delta-y (vertical):
    segments(x0 = x_to, y0 = y_from, x1 = x_to, y1 = y_to,
             lty = lty_main, lwd = lwd_emph, col = col_delta)

    if (show_num){ # delta-y label:

      dy_lbl <- paste0(round(delta_y, n_dig), "%")  # paste0("dy = ", round(delta_y, n_dig), "%")

      # Label position:
      if (x_to < .90 * x_max){ # default:
        pos_dy <- 4  # right
      } else { # switch to left:
        pos_dy <- 2  # left
      }

      text(x = x_to, y = (y_from + (delta_y * f_1)), labels = dy_lbl,
           col = col_delta, cex = cex_lbl, font = 2, pos = pos_dy, xpd = TRUE)
    }
  }



  # 4. Risk/y-increments (on x): ------

  if (show_inc){

    # lines(x = x, y = rinc_y, lwd = lwd_aux, lty = lty_aux, col = col_rinc)
    segments(x0 = x, y0 = 0, x1 = x, y1 = rinc_y, lwd = lwd_main, lty = lty_main, col = col_rinc)
    points(x = x, y = rinc_y, pch = 21, cex = (cex_pts - 0.3), col = col_rinc, bg = NA, lwd = 1.5)

  }


  # 5. Auxiliary elements: Lines and text labels ------

  if (show_aux){

    x_lbl_l <- (x_min + x_adj)  # x-value of text labels (on left)

    # (a) Aux lines and label for (x_from, y_from):
    if (!is.na(x_from)){

      segments(x0 = x_from, y0 = 0, x1 = x_from, y1 = y_from, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x-from from bot (vertical)

      segments(x0 = x_min, y0 = y_from, x1 = x_from, y1 = y_from, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # y-from to left (horizontal)

      if (show_num){
        text(x = x_lbl_l, y = y_from, labels = paste0(round(y_from, n_dig), "%"), pos = pos_aux,
             cex = cex_lbl, font = 2, col = make_transparent(col_txt, alpha = 1), xpd = TRUE)  # y_from label (on left axis)
      }
    }

    # (b) Aux lines and label for (x_to, y_to):
    if (!is.na(x_to)){

      segments(x0 = x_to, y0 = 0, x1 = x_to, y1 = y_to, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x-to from bot (vertical)

      segments(x0 = x_min, y0 = y_to, x1 = x_to, y1 = y_to, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux), xpd = TRUE)  # y-to to left (horizontal)

      if (show_num){
        text(x = x_lbl_l, y = y_to, labels = paste0(round(y_to, n_dig), "%"), pos = pos_aux,
             cex = cex_lbl, font = 2, col = make_transparent(col_txt, alpha = 1), xpd = TRUE)  # y_to label (on left axis)
      }
    }

    # (d) Aux line and label for y_max (to left axis):
    if (delta_x_specified & (y_max != 100)){  # only if y_max is not 100 (trivial case):

      segments(x0 = x_min, y0 = y_max, x1 = x_max, y1 = y_max, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # y_max on top (horizontal)

      if (show_num){
        text(x = x_lbl_l, y = y_max, labels = paste0(round(y_max, n_dig), "%"), pos = pos_aux,
             cex = cex_lbl, font = 2, col = make_transparent(col_txt, alpha = 1), xpd = TRUE)  # y_max label (on left axis)
      }
    }

    # (c) Aux lines for remaining risk (to right axis):
    if (delta_x_specified & (show_aux | show_rem)){

      segments(x0 = x_from, y0 = y_from, x1 = x_max, y1 = y_from, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x/y-from to right (horizontal)

      segments(x0 = x_to, y0 = y_to, x1 = x_max, y1 = y_to, lwd = lwd_aux, lty = lty_aux,
               col = make_transparent(col_aux, alpha = alf_aux))  # x/y-to to right (horizontal)
    }

    # (e) Population segments:
    if (!is.na(x_from) & show_pop){

      # 1. lower segment (passed risk):
      f_2  <- 1/2     # scaling factor for label position

      # segments(x0 = x_lo, y0 = y_min, x1 = x_lo, y1 = popu_pas, lwd = 2, lty = 1,
      #          col = make_transparent(col_pas, alpha = 1))  # passed-y (vertical)

      # Plot lower arrow/line segment:
      plot_line(x0 = x_lo, y0 = y_min, x1 = x_lo, y1 = popu_pas, lty = lty_main, lwd = lwd_emph,
                lbl = "affected", srt = 90, lbl_x = (x_lo - x_adj), cex = cex_lbl,
                arr_code = arr_c, col_brd = col_txt, col_fill = col_popu, col_txt = col_txt, ...)

      # 2. upper segment (remaining risk):
      y_up <- y_from + (y_max - y_from) * f_2  # y of remaining population label

      # segments(x0 = x_up, y0 = y_from, x1 = x_up, y1 = y_max, lty = lty_main, lwd = lwd_emph,
      #          col = make_transparent(col_popu, alpha = alf_popu))  # remaining-y (vertical)

      if (y_max < 100){ # upper arrow/line segement does not include entire population:

        # Adjust top y-value and arrow code:
        y_adj <- y_max/20
        # arr_2 <- arr_c
        # if ((arr_c == 3) | (arr_c == 6)) { arr_2 <- (arr_c - 2) }
        # if (arr_c == -3) {  arr_2 <- -2 }
        arr_2 <- 4  # 4: lower T, upper line

        # Plot upper arrow/line segment (with open top):
        plot_line(x0 = x_up, y0 = y_from, x1 = x_up, y1 = (y_max + y_adj), lty = lty_main, lwd = lwd_emph,
                  lbl = "unaffected", srt = 90, lbl_x = (x_up - x_adj), cex = cex_lbl,
                  arr_code = arr_2, col_brd = col_txt, col_fill = col_popu, col_txt = col_txt, xpd = TRUE, ...)

      } else { # default case:

        # Plot upper arrow/line segment:
        plot_line(x0 = x_up, y0 = y_from, x1 = x_up, y1 = y_max, lty = lty_main, lwd = lwd_emph,
                  lbl = "unaffected", srt = 90, lbl_x = (x_lo - x_adj), cex = cex_lbl,
                  arr_code = arr_c, col_brd = col_txt, col_fill = col_popu, col_txt = col_txt, ...)
      }

      if (show_num){ # label numeric values:

        text(x = x_lo, y = (y_from * f_2), labels = paste0(round(popu_pas, n_dig), "%"),
             col = make_transparent(col_popu, alpha = 1), cex = cex_lbl, font = 2, pos = 4, xpd = TRUE)  # y-from/passed risk label

        text(x = x_up, y = y_up, labels = paste0(round(popu_rem, n_dig), "%"),
             col = make_transparent(col_popu, alpha = 1), cex = cex_lbl, font = 2, pos = 4, xpd = TRUE)  # remaining population proportion label

      }
    }

  } # if (show_aux) end.


  # 6. Cumulative risk curve: ------

  if (!fit_curve){
    lines(x = x, y = y, lwd = lwd_main, lty = lty_main, col = make_transparent(col_cum, alpha = alf_cum))  # actual values
  } else {
    lines(fit_spline,   lwd = lwd_main, lty = lty_main, col = make_transparent(col_cum, alpha = alf_cum))  # fitted values
  }

  points(x = x, y = y, pch = 20, cex = cex_pts, col = make_transparent(col_cum, alpha = alf_cum))


  # 8. Highlight remaining risk delta_risk (on right axis): ------

  if (delta_x_specified & show_high){

    segments(x0 = x_to, y0 = y_to, x1 = (x_max + x_adj), y1 = y_to, lwd = 1, lty = lty_aux,
             col = make_transparent(col_hi, alpha = alf_hi), xpd = TRUE)  # x/y-to to right axis (horizontal)

    if (show_pop){
      segments(x0 = x_to, y0 = y_to, x1 = x_up, y1 = y_to, lwd = 1, lty = lty_aux,
               col = make_transparent(col_hi, alpha = alf_hi), xpd = TRUE)  # x/y-to to upper population partition (horizontal)
    }

    if (show_num){
      text(x = (x_max + x_adj/2), y = y_to, labels = paste0(round(risk_delta, n_dig), "%"), pos = 4,
           cex = cex_lbl, font = 2, col = make_transparent(col_hi, alpha = alf_hi), xpd = TRUE)  # risk_delta label (on right axis)
    }

  }


  # 7. Show increment points (x_from/x_to) on cum curve: ------

  # (a) Draw 1st point (x_from y_from):
  if (!is.na(x_from)){

    points(x_from, y_from, pch = 21, cex = (cex_pts - 0.3),
           col = col_popu, bg = make_transparent(col_pas, alpha = 0), lwd = lwd_emph)

  } # if (!is.na(x_from)) end.


  # (b) # Draw 2nd point (x_to y_to):
  if (!is.na(x_to)){

    points(x_to, y_to, pch = 21, cex = (cex_pts - 0.3),
           col = col_hi, bg = make_transparent(col_rem, alpha = 0), lwd = lwd_emph)

  } # if (!is.na(x_to)) end.



  ## (6) Plot other stuff: --------

  # (A) Miscellanea: ------

  # box_else <- make_box("else_box", 2, 2, 4, 3)  # define some arbitrary box
  # plot(box_else, col = "firebrick2", cex = 1/2, font = 2)  # plot box

  # (B) Title:

  # Plot title: # (left, not raised, normal font)
  title(title_lbl, col = col_txt, adj = 0, line = 1, font.main = 1, cex.main = cex_tit)


  # (C) Margin notes: ------

  if (mar_notes) {

    # Content of note:
    if (fit_curve){
      note_lbl <- "(Using fitted data.)"
    } else {
      note_lbl <- "(Using provided data.)"
    }

    mtext(note_lbl, side = 1, line = 2, adj = 1, col = col_mar, cex = cex_mar)  # right 4 mid

  } # if (mar_notes) etc.


  ## (7) Finish: --------

  # on.exit(par(opar))  # par(opar)  # restore original settings
  invisible()# restores par(opar)

} # plot_crisk end.


## (3) Check: --------

# # 1. Dense data:
# x <- seq(from = 0, to = 100, by = 5)
# i <- c(0, 0, 0, 0, 0, .035, .070, .145, .160, .120, .07, .03, .02, .0150, .0150, .0125, .0075, 0, 0, 0, 0)
# y <- cumsum(i) * 100  # as percentages
#
# 2. sparse data:
# x <- seq(0, 100, by = 10)
# y <- c(0, 0, 0, 10, 24, 50, 72, 80, 83, 85, 85)
#
# # Basic versions:
# plot_crisk(x, y)  # using data provided
# plot_crisk(x, y, x_from = 40)  # use and mark 1 provided point
# plot_crisk(x, y, x_from = 44)  # use and mark 1 provided point
# plot_crisk(x, y, x_from = 40, x_to = 60)  # use 2 provided points
# plot_crisk(x, y, x_from = 44, x_to = 74)  # use 2 predicted points
# plot_crisk(x, y, fit_curve = TRUE)  # fitting curve to provided data
#
# # Training versions:
# plot_crisk(x, y, 44, 64, show_pas = TRUE)  # past/passed risk only
# plot_crisk(x, y, 44, 64, show_rem = TRUE)  # remaining risk only
# plot_crisk(x, y, 44, 64, show_pas = TRUE, show_rem = TRUE) # both risks
# plot_crisk(x, y, 44, 64, show_aux = TRUE)  # auxiliary lines + axis
# plot_crisk(x, y, 44, 64, show_aux = TRUE, show_pop = TRUE)  # + population parts
# plot_crisk(x, y, 44, 64, show_aux = TRUE, show_num = TRUE)  # + numeric values
# plot_crisk(x, y, 44, 64, show_aux = TRUE, show_pop = TRUE, show_num = TRUE) # + aux/pop/num
#
# # Note: Showing ALL is likely to overplot/overwhelm:
# plot_crisk(x, y, x_from = 47, x_to = 67, fit_curve = TRUE,
#            show_pas = TRUE, show_rem = TRUE, show_aux = TRUE, show_pop = TRUE,
#            show_num = TRUE, show_inc = TRUE, show_grid = TRUE, mar_notes = TRUE)
#
# # Omitting risk interval prevents from showing MOST additional info:
# plot_crisk(x, y, # x_from = 44, x_to = 64, fit_curve = FALSE,
#            show_pas = TRUE, show_rem = TRUE, show_aux = TRUE, show_pop = TRUE,
#            show_num = TRUE, show_inc = TRUE, show_grid = TRUE, mar_notes = TRUE)
#
# # Omitting ONLY x_to allows showing SOME additional info:
# plot_crisk(x, y, x_from = 44, # x_to = 64, fit_curve = FALSE,
#            show_pas = TRUE, show_rem = TRUE, show_aux = TRUE, show_pop = TRUE,
#            show_num = TRUE, show_inc = TRUE, show_grid = TRUE, mar_notes = TRUE)
#
# # Small x- and y-values and linear increases:
# plot_crisk(x = 1:10, y = seq( 1, 20, by = 2), x_from = 4, x_to = 8,    # provided points
#            show_pas = TRUE, show_rem = TRUE, show_aux = TRUE, show_pop = TRUE, show_num = TRUE, show_inc = TRUE)
# plot_crisk(x = 2:10, y = seq(12, 28, by = 2), x_from = 4.5, x_to = 8.5,  # predicted points
#            show_pas = TRUE, show_rem = TRUE, show_aux = TRUE, show_pop = TRUE, show_num = TRUE, show_inc = TRUE)
#
# # Text labels:
# plot_crisk(x, y, x_from = 32, x_to = 47, show_aux = TRUE, title_lbl = "The title",
#            x_lbl = "X-lab", y_lbl = "Y-lab", y2_lbl = "Alt-Y-lab", mar_notes = TRUE)


## (+) ToDo: ------

# (a) testing function:
# - add real data
# - test function with diverse data, contents, and extreme values

# (b) adding functionality:
# - add an option for only plotting remaining risk area?
# - explore better curve fitting options (e.g., linear/cubic fits, rather than splines)
#   and modularize fitting parts (as separate functions)
# - add data/option for population decrements (on top of plot)
# - add a legend option

# (c) aesthetics/details:
# - add user-defined margin label
# - add more dynamic label positions
# - explore alternative color options

## (*) Done: ----------

## - initial function [2021-12-04]

## eof. ----------
