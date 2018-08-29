## plot_curve.R | riskyr
## 2018 08 29
## plot_curve: Plots different probabilities
## (e.g., PPV, NPV, ppod, acc) as a function
## of prevalence (for given sens and spec).
## -----------------------------------------------

## Utility function:
## add_legend() moved to comp_util.R

## Plot PV curves: PPV and NPV as functions of prevalence
## (using only necessary arguments with good defaults):

## plot_curve: Documentation ----------

#' Plot curves of selected values (e.g., PPV or NPV)
#' as a function of prevalence.
#'
#' \code{plot_curve} draws curves of selected values
#' (including \code{\link{PPV}}, \code{\link{NPV}})
#' as a function of the prevalence (\code{\link{prev}})
#' for given values of
#' sensitivity \code{\link{sens}} (or
#' miss rate \code{\link{mirt}}) and
#' specificity \code{\link{spec}} (or
#' false alarm rate \code{\link{fart}}).
#'
#' \code{plot_curve} is a generalization of
#' \code{plot_PV} (see legacy code)
#' that allows for additional dependent values.
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
#' @param spec The decision's specificity \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param what A vector of character codes that specify the
#' selection of curves to be plotted. Currently available
#' options are \code{c("prev", "PPV", "NPV", "ppod", "acc")}
#' (shortcut: \code{what = "all"}).
#' Default: \code{what = c("prev", "PPV", "NPV")}.
#'
#' @param what.col A vector of colors corresponding to the elements
#' specified in \code{what}.
#' Default: \code{what.col = pal}.
#'
#' @param unc Uncertainty range, given as a percentage of the current
#' \code{\link{prev}}, \code{\link{sens}}, and \code{\link{spec}} values
#' (added in both directions).
#' Default: \code{unc = .00} (i.e., no uncertainty).
#' Plausible ranges are \code{0 < unc < .25}.
#'
#' @param show.points Boolean option for showing the point of
#' intersection with the current prevalence \code{\link{prev}}
#' in all selected curves.
#' Default: \code{show.points = TRUE}.
#'
#' @param log.scale Boolean value for switching from a linear
#' to a logarithmic x-axis.
#' Default: \code{log.scale = FALSE}.
#'
#' @param title.lbl The title of the current plot.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param cex.lbl Scaling factor for the size of text labels
#' (e.g., on axes, legend, margin text).
#' Default: \code{cex.lbl = .85}.
#'
#' @examples
#' # Basics:
#' plot_curve()                     # => default plot: what = ("prev", "PPV", "NPV")
#' plot_curve(show.points = FALSE)  # => default plot without points
#' plot_curve(prev = .2, sens = .8, spec = .7, unc = .1)  # => 10% uncertainty range
#'
#' # All curves:
#' plot_curve(what = "all") # => all curves: what = ("prev", "PPV", "NPV", "ppod", "acc")
#' plot_curve(what = "all", show.points = FALSE)  # => all curves, no points
#'
#' # Selected curves:
#' plot_curve(what = c("PPV", "NPV"))                  # => PPV and NPV
#' plot_curve(what = c("prev", "PPV", "NPV", "acc"))   # => prev, PPV, NPV, and acc
#' plot_curve(what = c("prev", "PPV", "NPV", "ppod"))  # => prev, PPV, NPV, and acc
#'
#' # Visualizing uncertainty (unc as percentage range):
#' plot_curve(prev = .3, sens = .9, spec = .8, what = c("prev", "PPV", "NPV"),
#'            unc = .05)  # => prev, PPV and NPV with a 5% uncertainty range
#' plot_curve(prev = .2, sens = .8, spec = .7, what = "all",
#'            unc = .10)  # => all with a 10% uncertainty range
#'
#' # X-axis as linear vs. log scale:
#' plot_curve(prev = .01, sens = .9, spec = .8)                     # => linear scale
#' plot_curve(prev = .01, sens = .9, spec = .8, log.scale = TRUE)   # => log scale
#'
#' plot_curve(prev = .0001, sens = .7, spec = .6)                   # => linear scale
#' plot_curve(prev = .0001, sens = .7, spec = .6, log.scale = TRUE) # => log scale
#'
#' # Other options:
#' plot_curve(title.lbl = "Testing tiny labels", cex.lbl = .60, unc = .1)
#' plot_curve(what = "all", title.lbl = "Testing colors",
#'            what.col = c("grey", "red3", "blue3", "gold", "green3"), unc = .2)
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{txt}} for current text settings;
#' \code{\link{pal}} for current color settings.
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics abline
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#'
#' @export

## plot_curve: Definition ----------

plot_curve <- function(prev = num$prev,             # probabilities (3 essential, 2 optional)
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,
                       ## DVs:
                       what = c("prev", "PPV", "NPV"),  # what curves?  Options: "prev", "PPV", "NPV", "acc", "ppod".
                       ## Options:
                       what.col = pal,                  # colors for what.
                       unc = .00,         # Uncertainty range (as a percentage around current prev, sens, and spec values)
                       show.points = TRUE,  # show points at current prev?
                       log.scale = FALSE,   # x-axis on log scale?
                       ## Text:
                       title.lbl = txt$scen.lbl, # plot title label
                       cex.lbl = .85             # scale size of text labels (e.g., on axes, legend, margin text)
) {

  ## (0) Compute or collect current probabilities: ----------

  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (1) A provided set of probabilities is valid:

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur.prob from scratch based on current parameters (N and probabilities):
    # cur.prob <- comp_prob(prev = prev, sens = sens, spec = spec, fart = fart)  # compute prob from scratch

    ## Compute and assign current PVs:
    cur.PPV <- comp_PPV(prev, sens, spec)  # compute PPV from probabilities
    cur.NPV <- comp_NPV(prev, sens, spec)  # compute NPV from probabilities

  } else {

    ## (2) NO valid set of probabilities is provided:

    ## Use current PVs of prob:
    cur.PPV <- prob$PPV  # use PPV from prob
    cur.NPV <- prob$NPV  # use NPV from prob

  }

  ## (1) Set some (currently fixed) parameters: ----------
  x <- NULL  # "nulling out" to avoid NOTE (no visible binding for global variable ‘x’) in R CMD check!

  ## Set x-value range for plotting curves:
  eps <- 10^-6  # some very small number
  # if (log.scale) { x.min <- (0 + eps) } else { x.min <- 0 }  # different x.min values for different scales
  x.min <- (0 + eps)  # was: above
  x.max <- (1 - eps)  # was: 1

  ## Set x-value (prev) range for plotting uncertainty polygons:
  if (unc > 0) {  # plot a polygon:

    unc_dens <- NULL # OR: 20 # density of polygon lines (default = NULL: no lines)

    ## Select x-value (prev) ranges based on current type of scale (log or linear):
    if (log.scale) {

      unc_stepSize <- .01  # x-increments at which y-values of uncertainty polygons are computed (smaller values = more computation)

      x_min <- 0 + eps  # avoid 0 (to avoid extreme values)
      x_max <- 1 - eps  # avoid 1 (to avoid extreme values)

      ## Ranges for x-values (prev) of polygon:
      x_lower <- c(10^-6, 10^-5, 10^-4, (1 * 10^-3), (2 * 10^-3), (5 * 10^-3), .01, .02, .05, .10, .15, .25, .30, .50, .60, .90, x_max)  # log steps from left to right
      x_upper <- rev(x_lower)  # from right to left

    } else {  # linear scale:

      unc_stepSize <- .01  # x-increments at which y-values of uncertainty polygons are computed (smaller values = more computation)

      x_min <- 0 + unc_stepSize  # avoid 0 (to avoid extreme values)
      x_max <- 1 - unc_stepSize  # avoid 1 (to avoid extreme values)

      ## Ranges for x-values (prev) of polygon:
      x_low_1 <- seq(x_min, x_max, by = (+1 * unc_stepSize))  # main steps
      x_lower <- c(x.min, x_low_1, x.max)                    # main steps + 2 extreme points (from left to right)
      x_upper <- rev(x_lower)                                # same steps (from right to left)

    } # if (log.scale)...

  } # if (unc > 0)...

  ## Positional parameters (for raising and shifting labels):
  if (log.scale) { h.shift <- prev * 2 } else { h.shift <- .075 }
  v.shift <- .025
  low.PV  <- .15  # threshold value for judging PPV or NPV to be low
  v.raise <- min(c(cur.PPV, cur.NPV)) + .15 # vertical raise of y-prev when PPV or NPV < low.PV

  ## Point appearance:
  pt.pch <- 21    # pch symbol of points
  pt.cex <- 1.6   # cex scaling of points
  pt.lwd <- 1.6   # lwd of point borders

  ## Colors:
  unc_alpha <- .20                     # transparency of uncertainty polygons
  col.axes <- grey(.10, alpha = .99)  # axes
  col.bord <- grey(.10, alpha = .50)  # borders (also of points)

  # Text label size:
  cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .075} else {cex.lbl}  # slightly smaller than cex.lbl

  legend.lbls <- NULL  # initialize vector
  legend.cols <- NULL
  legend.ltys <- NULL


  ## (2) Define plotting area: ----------

  ## (a) Define steps and labels of x- and y-axes:

  ## x-axis:
  if (log.scale) {
    x.seq <- c(10^-5, 10^-4, 10^-3, 10^-2, .10, .25, .50, 1)  # log steps
    x.lbl <- paste0(as_pc(x.seq, n.digits = 5), "%")          # log percentages (rounded to 5 decimals)
    x.ax.lbl <- "Prevalence (on logarithmic scale)"           # log x-axis label
  } else {
    x.seq <- seq(0, 1, by = .10)        # linear steps of 10%
    x.lbl <- paste0(as_pc(x.seq), "%")  # linear percentages
    x.ax.lbl <- "Prevalence"            # linear x-axis label
  }

  ## y-axis:
  y.seq <- seq(0, 1, by = .10)        # linear steps of 10%
  y.lbl <- paste0(as_pc(y.seq), "%")  # linear percentages
  y.ax.lbl <- "Probability"           # y-axis label

  ## (b) Initialize plot:
  if (log.scale) {
    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE,
         log = "x",
         ylab = y.ax.lbl, xlab = x.ax.lbl, cex.axis = cex.lbl, type = "n")
  } else {
    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE,
         ylab = y.ax.lbl, xlab = x.ax.lbl, cex.axis = cex.lbl, type = "n")
  }

  ## (c) Axes (on 4 sides):
  axis(side = 1, at = x.seq, labels = x.lbl, cex.axis = cex.lbl, cex.lab = (cex.lbl),
       las = 1, pos = 0, tck = -.02, col.axis = col.axes, col.ticks = col.axes)       # x at bottom
  axis(side = 1, at = x.seq, labels = FALSE, cex.axis = cex.lbl, cex.lab = (cex.lbl),
       las = 1, pos = 1, tck = -.01, col.axis = col.axes, col.ticks = col.axes)       # x at top
  axis(side = 2, at = y.seq, labels = y.lbl, cex.axis = cex.lbl, cex.lab = (cex.lbl),
       las = 1, pos = x.min, tck = -.02, col.axis = col.axes, col.ticks = col.axes)   # y at left
  axis(side = 4, at = y.seq, labels = y.lbl, cex.axis = cex.lbl, cex.lab = (cex.lbl),
       las = 1, pos = 1, tck = -.02, col.axis = col.axes, col.ticks = col.axes)       # y at right

  ## (d) Grid:
  grid(col = grey(.80, .80))

  ## (3) Interpret what argument: ----------

  ## (a) shortcut to get all what options:
  if ("all" %in% what || "ALL" %in% what || "All" %in% what ) {
    what <- c("prev", "PPV", "NPV", "ppod", "acc")
  }

  ## (b) express all options in lower case:
  what <- tolower(what)

  ## (4) Plot elements of what: ----------

  ## (a) prev: ----------

  if ("prev" %in% what) {

    ## 0. parameters:
    lty.prev <- 2  # prev line type

    ## color:
    if (length(what.col) == length(what)) { # a color vector was specified:
      pos.prev <- which(what == "prev")  # find position of "prev" in what
      col.prev <- what.col[pos.prev]     # use color specified for prev
    } else {
      col.prev <- grey(.50, alpha = .99) # use default color for prev
    }

    legend.lbls <- c(legend.lbls, "prev")    # add prev label
    legend.cols <- c(legend.cols, col.prev)  # add prev color
    legend.ltys <- c(legend.ltys, lty.prev)  # add prev line type

    ## 0. Mark uncertainty about prev (as polygon/here: rectangle):

    if (unc > 0) {

      ## Color of uncertainty polygon (here: rectangle):
      unc_col  <- makeTransparent(col.prev, alpha = unc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon (here: rectangle):
      x_lower_prev <- c(max(0, (prev - unc * prev)), min((prev + unc * prev), 1)) # only 2 points (left & right)
      x_upper_prev <- rev(x_lower_prev)  # only 2 points (right & left)

      ## Compute upper and lower y-values (0 and 1) corresponding to prev values:
      y_lower_prev <- c(0, 0)  # both points on minimum y-value
      y_upper_prev <- c(1, 1)  # both points on maximum y-value

      ## Plot polygon (here: rectangle):
      xx <- c(x_lower_prev, x_upper_prev)
      yy <- c(y_lower_prev, y_upper_prev)
      polygon(xx, yy, col = unc_col, border = NA, density = unc_dens)

    }

    ## 1. curve: prev as vline
    abline(v = prev, lty = lty.prev, lwd = 1, col = col.prev)  # prev curve/line

    ## 2. point:
    if (show.points) {

      if ((cur.NPV < low.PV) | (cur.PPV < low.PV)) { # y-pos at v.raise:
        points(x = prev, y = 0 + v.raise, pch = pt.pch, cex = pt.cex, lwd = pt.lwd, col = col.bord, bg = col.prev)  # prev point
      } else { # y-pos at bottom (y = 0):
        points(x = prev, y = 0,           pch = pt.pch, cex = pt.cex, lwd = pt.lwd, col = col.bord, bg = col.prev)  # prev point
      }

      ## 3. label:
      prev.lbl <- paste0("prev = ", as_pc(prev), "%")  # prev label


      if ((cur.NPV < low.PV) | (cur.PPV < low.PV)) { # y at v.raise:
        if ((prev < .50) | !(prev > 1 - h.shift)) {
          text(x = prev + h.shift, y = 0 + v.raise,
               labels = prev.lbl, col = col.prev, cex = cex.lbl.sm) # on right
        } else {
          text(x = prev - h.shift, y = 0 + v.raise,
               labels = prev.lbl, col = col.prev, cex = cex.lbl.sm) # on left+
        }
      } else { # y at bottom (y = 0):
        if ((prev < .50) | !(prev > 1 - h.shift)) {
          text(x = prev + h.shift, y = 0 + v.shift,
               labels = prev.lbl, col = col.prev, cex = cex.lbl.sm) # on right
        } else {
          text(x = prev - h.shift, y = 0 + v.shift,
               labels = prev.lbl, col = col.prev, cex = cex.lbl.sm) # on left+
        }
      }

    } # if (show.points)...

  } # if ("prev" %in% what)...


  ## (b) PPV: ----------

  if ("ppv" %in% what) {

    ## 0. parameters:
    lty.ppv <- 1                            # PPV line type

    ## colors:
    if (length(what.col) == length(what)) { # a color vector was specified:
      pos.ppv <- which(what == "ppv")  # find position of "PPV" in what
      col.ppv <- what.col[pos.ppv]     # use color specified for PPV
    } else {
      col.ppv <- pal["ppv"]  # use default color for PPV
    }

    legend.lbls <- c(legend.lbls, "PPV")    # add PPV label
    legend.cols <- c(legend.cols, col.ppv)  # add PPV color
    legend.ltys <- c(legend.ltys, lty.ppv)  # add PPV line type

    ## 0. Mark uncertainty about PPV based on vague values of sens and spec (as polygon):

    if (unc > 0) {

      ## Color of PPV uncertainty polygon:
      unc_col  <- makeTransparent(col.ppv, alpha = unc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (PPV) corresponding to prev values:
      y_lower <- comp_PPV(prev = x_lower, sens = max(0, (sens - unc * sens)), spec = max(0, (spec - unc * spec)))  # both sens & spec DEcreased by unc
      y_upper <- comp_PPV(prev = x_upper, sens = min(1, (sens + unc * sens)), spec = min(1, (spec + unc * spec)))  # both sens & spec INcreased by unc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = unc_col, border = NA, density = unc_dens)

    }

    ## 1. PPV curve:
    curve(expr = comp_PPV(prev = x, sens, spec), from = x.min, to = x.max, add = TRUE, lty = lty.ppv, lwd = 2, col = col.ppv)  # PPV curve

    ## 2. PPV point:
    if (show.points) {

      points(x = prev, y = cur.PPV, pch = pt.pch, cex = pt.cex, lwd = pt.lwd, col = col.bord, bg = col.ppv)  # PPV point

      ## 3. label:
      PPV.lbl <- paste0("PPV = ", as_pc(cur.PPV), "%")  # PPV label

      if ((cur.PPV < .75 & !(prev > 1 - h.shift)) || (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.PPV + v.shift,
             labels = PPV.lbl, col = col.ppv, cex = cex.lbl.sm) # on right
      } else {
        text(x = prev - h.shift, y = cur.PPV + v.shift,
             labels = PPV.lbl, col = col.ppv, cex = cex.lbl.sm) # on left+
      }

    } # if (show.points)...

  } # if ("ppv" %in% what)...


  ## (c) NPV: ----------

  if ("npv" %in% what) {

    ## 0. parameters:
    lty.npv <- 1                            # PPV line type

    ## color:
    if (length(what.col) == length(what)) { # a color vector was specified:
      pos.npv <- which(what == "npv")  # find position of "NPV" in what
      col.npv <- what.col[pos.npv]     # use color specified for NPV
    } else {
      col.npv <- pal["npv"]  # use default color for NPV
    }

    legend.lbls <- c(legend.lbls, "NPV")    # add NPV label
    legend.cols <- c(legend.cols, col.npv)  # add NPV color
    legend.ltys <- c(legend.ltys, lty.npv)  # add NPV line type


    ## 0. Mark uncertainty about NPV based on vague values of sens and spec (as polygon):

    if (unc > 0) {

      ## Color of uncertainty polygon:
      unc_col  <- makeTransparent(col.npv, alpha = unc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (NPV) corresponding to prev values:
      y_lower <- comp_NPV(prev = x_lower, sens = max(0, (sens - unc * sens)), spec = max(0, (spec - unc * spec)))  # both sens & spec DEcreased by unc
      y_upper <- comp_NPV(prev = x_upper, sens = min(1, (sens + unc * sens)), spec = min(1, (spec + unc * spec)))  # both sens & spec INcreased by unc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = unc_col, border = NA, density = unc_dens)

    }

    ## 1. NPV curve:
    curve(expr = comp_NPV(prev = x, sens, spec), from = x.min, to = x.max, add = TRUE, lty = lty.npv, lwd = 2, col = col.npv)  # NPV curve

    ## 2. NPV point:
    if (show.points) {

      points(x = prev, y = cur.NPV, pch = pt.pch, cex = pt.cex, lwd = pt.lwd, col = col.bord, bg = col.npv)  # NPV point

      ## 3. label:
      NPV.lbl <- paste0("NPV = ", as_pc(cur.NPV), "%")  # NPV label

      if (cur.NPV > .75 | (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.NPV + v.shift,
             labels = NPV.lbl, col = col.npv, cex = cex.lbl.sm) # on right+
      } else {
        text(x = prev - h.shift, y = cur.NPV - v.shift,
             labels = NPV.lbl, col = col.npv, cex = cex.lbl.sm) # on left-
      }

    } # if (show.points)...

  } # if ("npv" %in% what)...


  ## (d) ppod: ----------

  if ("ppod" %in% what) {

    ## 0. parameters:
    cur.ppod <- comp_ppod(prev, sens, spec)  # compute current ppod
    lty.ppod <- 1                            # ppod line type

    ## color:
    if (length(what.col) == length(what)) { # a color vector was specified:
      pos.ppod <- which(what == "ppod")  # find position of "ppod" in what
      col.ppod <- what.col[pos.ppod]     # use color specified for ppod
    } else {
      col.ppod <- pal["pos"]  # use default color for ppod (using "pos")
    }

    legend.lbls <- c(legend.lbls, "ppod")    # add NPV label
    legend.cols <- c(legend.cols, col.ppod)  # add NPV color
    legend.ltys <- c(legend.ltys, lty.ppod)  # add NPV line type

    ## 0. Mark uncertainty about ppod based on vague values of sens and spec (as polygon):

    if (unc > 0) {

      ## Color of uncertainty polygon:
      unc_col  <- makeTransparent(col.ppod, alpha = unc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (ppod) corresponding to prev values:
      ## NOTE: ppod is INVERSELY related to spec!
      y_lower <- comp_ppod(prev = x_lower, sens = max(0, (sens - unc * sens)), spec = min(1, (spec + unc * spec)))  # NOTE: sens decrease & spec INcrease by unc
      y_upper <- comp_ppod(prev = x_upper, sens = min(1, (sens + unc * sens)), spec = max(0, (spec - unc * spec)))  # NOTE: sens increase & spec DEcrease by unc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = unc_col, border = NA, density = unc_dens)

    }

    ## 1. curve:
    curve(expr = comp_ppod(prev = x, sens, spec), from = x.min, to = x.max, add = TRUE, lty = lty.ppod, lwd = 2, col = col.ppod)  # ppod curve

    ## 2. point:
    if (show.points) {

      points(x = prev, y = cur.ppod, pch = pt.pch, cex = pt.cex, lwd = pt.lwd, col = col.bord, bg = col.ppod)  # ppod point

      ## 3. label:
      ppod.lbl <- paste0("ppod = ", as_pc(cur.ppod), "%")  # ppod label

      if (cur.ppod > .75 | (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.ppod + v.shift,
             labels = ppod.lbl, col = col.ppod, cex = cex.lbl.sm) # on right+
      } else {
        text(x = prev - h.shift, y = cur.ppod - v.shift,
             labels = ppod.lbl, col = col.ppod, cex = cex.lbl.sm) # on left-
      }

    } # if (show.points)...

  } # if ("ppod" %in% what)...


  ## (e) Overall accuracy (acc):
  if ("acc" %in% what) {

    ## 0. parameters:
    cur.acc <- comp_acc(prev, sens, spec)   # compute current acc
    lty.acc <- 1                            # acc line type

    ## color:
    if (length(what.col) == length(what)) { # a color vector was specified:
      pos.acc <- which(what == "acc")  # find position of "acc" in what
      col.acc <- what.col[pos.acc]     # use color specified for acc (using "hi")
    } else {
      col.acc <- pal["hi"]  # use default color for acc (using "hi")
    }

    legend.lbls <- c(legend.lbls, "acc")    # add acc label
    legend.cols <- c(legend.cols, col.acc)  # add acc color
    legend.ltys <- c(legend.ltys, lty.acc)  # add acc line type

    ## 0. Mark uncertainty about acc based on vague values of sens and spec (as polygon):

    if (unc > 0) {

      ## Color of uncertainty polygon:
      unc_col  <- makeTransparent(col.acc, alpha = unc_alpha)  # grey(.80, .33)

      ## Ranges for x-values (prev) of polygon:
      ## (See x_lower and x_upper above.)

      ## Compute upper and lower y-values (acc) corresponding to prev values:
      y_lower <- comp_acc(prev = x_lower, sens = max(0, (sens - unc * sens)), spec = max(0, (spec - unc * spec)))  # both sens & spec DEcreased by unc
      y_upper <- comp_acc(prev = x_upper, sens = min(1, (sens + unc * sens)), spec = min(1, (spec + unc * spec)))  # both sens & spec INcreased by unc

      ## Correction: Limit values to range from 0 to 1:
      # y_lower[y_lower < 0] <- 0  # set y-values < 0 to 0.
      # y_upper[y_upper > 1] <- 1  # set y-values > 1 to 1.

      ## Plot polygon:
      xx <- c(x_lower, x_upper)
      yy <- c(y_lower, y_upper)
      polygon(xx, yy, col = unc_col, border = NA, density = unc_dens)

    }

    ## 1. acc curve:
    curve(expr = comp_acc(prev = x, sens, spec), from = x.min, to = x.max, add = TRUE, lty = lty.acc, lwd = 2, col = col.acc)  # acc curve

    ## 2. acc point:
    if (show.points) {

      points(x = prev, y = cur.acc, pch = pt.pch, cex = pt.cex, lwd = pt.lwd, col = col.bord, bg = col.acc)  # acc point

      ## 3. label:
      acc.lbl <- paste0("acc = ", as_pc(cur.acc), "%")  # acc label

      if (cur.acc > .75 | (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.acc + v.shift,
             labels = acc.lbl, col = col.acc, cex = cex.lbl.sm) # on right+
      } else {
        text(x = prev - h.shift, y = cur.acc - v.shift,
             labels = acc.lbl, col = col.acc, cex = cex.lbl.sm) # on left-
      }

    } # if (show.points)...

  } # if ("acc" %in% what)...


  ## (5) Title: ----------

  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Probability curves by prevalence") #, "\n", cur.sens.spec.lbl)

  title(cur.title.lbl, adj = 0.0, line = 1.0, font.main = 1) # (left, raised, normal font)


  ## (6) Margin text: ----------

  ## (a) by condition: 3 basic probabilities
  cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  mtext(cur.cond.lbl, side = 1, line = 2, adj = 0, col = grey(.33, .99), cex = cex.lbl)  # print label

  if (unc > 0) {

    ## (b) Note uncertainty signal and unc value:
    note <- paste0("Shading marks an uncertainty of ", as_pc(unc), "%")

    note_lbl <- paste0("Note:  ", note, ".")
    mtext(paste0("", note_lbl, ""), side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = cex.lbl)

  }

  ## (7) Legend: ----------

  if (length(legend.lbls) > 0) { # there is a curve:
    # legend("bottom", legend = c("PPV", "NPV"),
    #       col = c(col.ppv, col.npv), lty = 1, lwd = 2, cex = 1, bty = "o", bg = "white")
    add_legend("topright",
               legend = legend.lbls, lty = legend.ltys, lwd = 2, col = legend.cols,
               cex = cex.lbl, horiz = FALSE, bty = 'n')
  }

  ## (8) Return what? : ----------
  # return(pp)     # returns plot
  # return()       # returns nothing
  # return("neat") # returns "..."

}

## Check: ----------

# ## Basics:
# plot_curve()  # => default curves (prev, PPV, NPV)
# plot_curve(what = "all")
# plot_curve(prev = .25, sens = .85, spec = .75, unc = .05)  # => parameters with a 5% uncertainty range
#
# ## Selected curves:
# plot_curve(what = c("PPV", "NPV"))                  # => PPV and NPV
# plot_curve(what = c("prev", "PPV", "NPV", "acc"))   # => prev, PPV, NPV, and acc
# plot_curve(what = c("prev", "PPV", "NPV", "ppod"))  # => prev, PPV, NPV, and acc
#
# ## Visualizing uncertainty (as ranges):
# plot_curve(what = c("prev", "PPV", "NPV"), unc = .10)  # => prev, PPV and NPV with 10% uncertainty range
# plot_curve(prev = .2, sens = .8, spec = .7, what = "all", unc = .1)  # => all metrics with 10% uncertainty range
#
# ## Other options:
# plot_curve(show.points = FALSE)  # => default without points
# plot_curve(what = c("PPV", "NPV"), show.points = TRUE)  # => prev not shown.
#
# ## linear vs. log scale:
# plot_curve(prev = .01, sens = .9, spec = .8)                     # => linear scale
# plot_curve(prev = .01, sens = .9, spec = .8, log.scale = TRUE)   # => log scale
#
# plot_curve(prev = .0001, sens = .7, spec = .6)                   # => linear scale
# plot_curve(prev = .0001, sens = .7, spec = .6, log.scale = TRUE) # => log scale
#
# plot_curve(title.lbl = "Testing smaller text labels", cex.lbl = .60)
# plot_curve(what = "all", what.col = c("grey", "red3", "green3", "blue3", "gold"))


## Check: How do PPV/NPV depend on sens and spec? ------

# comp_NPV(prev = 1, sens = 1, spec = 1) # => NaN


## increasing sens: => increases PPV (by increasing hi)

# comp_PPV(prev = 1/4, sens = 2/4, spec = 3/4) # => 0.40
# comp_PPV(prev = 1/4, sens = 3/4, spec = 3/4) # => 0.50
# comp_PPV(prev = 1/4, sens = 7/8, spec = 3/4) # => 0.54

## increasing spec: => increases PPV (by reducing fa)

# comp_PPV(prev = 1/4, sens = 3/4, spec = 2/4) # => 0.33
# comp_PPV(prev = 1/4, sens = 3/4, spec = 3/4) # => 0.50
# comp_PPV(prev = 1/4, sens = 3/4, spec = 7/8) # => 0.67


## Check: How does ppod depend on sens and spec? ------

# comp_ppod(prev = 1/4, sens = 3/4, spec = 2/4) # => 0.56
# comp_ppod(prev = 1/4, sens = 3/4, spec = 3/4) # => 0.38
# comp_ppod(prev = 1/4, sens = 3/4, spec = 7/8) # => 0.28

## increasing spec: => DEcreases ppod!


## (*) Done: ----------

## - Add option unc to show _ranges_ (polygons) to better visualize uncertainty.
## - Clean up code.  [2018 08 28]

## (+) ToDo: ----------

## - Add option to sample multiple points from given _prob_ distributions.
## - Add more options: ppod, accu, etc.
## - fine-tune positions of labels and legend (on linear vs. log scale)
## - pimp plot (titles, axes, grid, colors, transparency)

## eof. ------------------------------------------
