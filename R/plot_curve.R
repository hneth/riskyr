## plot_curve.R | riskyr
## 2018 02 11
## -----------------------------------------------
## plot_curve: A generalization of plot_PV
## that plots different DVs (e.g., PPV, NPV, acc curves)
## as functions of prevalence.

## -----------------------------------------------
## Utility function:

## add_legend() moved to comp_util.R

## -----------------------------------------------
## Plot PV curves: PPV and NPV as functions of prevalence
## (using only necessary arguments with good defaults):


#' Plot curves of selected values as a function of prevalence.
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
#' \code{plot_curve} is a generalization of \code{\link{plot_PV}}
#' that allows for additional dependent values.
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
#'
#' @param what A vector of character codes that specify the
#' selection of curves to be plotted. Currently available
#' options are \code{c("prev", "PPV", "NPV", "ppod", "acc")}
#' (shortcut: \code{what = "all"}).
#' Default: \code{what = c("prev", "PPV", "NPV")}.
#'
#' @param show.points Boolean option for showing the point of
#' intersection with the current prevalence \code{\link{prev}}
#' in all selected curves.
#' Default: \code{show.points = TRUE}.
#'
#' @param what.col A vector of colors corresponding to \code{what}.
#' Default: \code{what.col = pal}.
#'
#'
#' @param log.scale Boolean value for switching from a linear
#' to a logarithmic x-axis.
#' Default: \code{log.scale = FALSE}.
#'
#' @param title.lbl The title of the current plot.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#'
#' @examples
#' # ways to work:
#' plot_curve()                     # => default: what = ("prev", "PPV", "NPV")
#' plot_curve(show.points = FALSE)  # => default without points
#'
#' # all curves:
#' plot_curve(what = "all") # => all curves: what = ("prev", "PPV", "NPV", "ppod", "acc")
#' plot_curve(what = "all", show.points = FALSE)  # => all curves, no points
#'
#' # selected curves:
#' plot_curve(what = c("PPV", "NPV"))                  # => PPV and NPV
#' plot_curve(what = c("prev", "PPV", "NPV", "acc"))   # => prev, PPV, NPV, and acc
#' plot_curve(what = c("prev", "PPV", "NPV", "ppod"))  # => prev, PPV, NPV, and acc
#'
#' # linear vs. log scale:
#' plot_curve(prev = .01, sens = .9, spec = .8)                     # => linear scale
#' plot_curve(prev = .01, sens = .9, spec = .8, log.scale = TRUE)   # => log scale
#'
#' plot_curve(prev = .0001, sens = .7, spec = .6)                   # => linear scale
#' plot_curve(prev = .0001, sens = .7, spec = .6, log.scale = TRUE) # => log scale
#'
#'
#' @family visualization functions
#'
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
#' @export


plot_curve <- function(prev = num$prev,             # probabilities (3 essential, 2 optional)
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,
                       ## DVs:
                       what = c("prev", "PPV", "NPV"),  # what curves?  Options: "acc", "ppod"
                       what.col = pal,                  # colors for what.
                       ## Options:
                       show.points = TRUE,  # show points at current prev?
                       log.scale = FALSE,   # x-axis on log scale?
                       title.lbl = txt$scen.lbl
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

  col.axes <- grey(.10, alpha = .99)  # axes
  col.bord <- grey(.10, alpha = .50)  # borders (e.g., of points)
  cex.lbl <- .8                       # size of text labels

  if (log.scale) { x.min <- 10^-6 } else { x.min <- 0 }  # different x.min values for different scales
  if (log.scale) { h.shift <- prev * 2 } else { h.shift <- .080 }
  v.shift <- .025
  low.PV  <- .15  # threshold value for judging PPV or NPV to be low
  v.raise <- min(c(cur.PPV, cur.NPV)) + .15 # vertical raise of y-prev when PPV or NPV < low.PV

  legend.lbls <- NULL  # initialize vector
  legend.cols <- NULL
  legend.ltys <- NULL


  ## (2) Initialize plotting area: : ----------

  ## (a) Define steps and labels of x- and y-axes:
  if (log.scale) {
    x.seq <- c(10^-5, 10^-4, 10^-3, 10^-2, .10, .25, .50, 1)  # log steps
    x.lbl <- paste0(as_pc(x.seq, n.digits = 5), "%")          # log percentages (rounded to 5 decimals)
    x.ax.lbl <- "Prevalence (on logarithmic scale)"           # log x-axis label
  } else {
    x.seq <- seq(0, 1, by = .1)         # linear steps
    x.lbl <- paste0(as_pc(x.seq), "%")  # linear percentages
    x.ax.lbl <- "Prevalence"            # linear x-axis label
  }

  y.seq <- seq(0, 1, by = .1)         # linear steps
  y.lbl <- paste0(as_pc(y.seq), "%")  # linear percentages
  y.ax.lbl <- "Probability"           # y-axis label

  ## (b) Initialize plot:
  if (log.scale) {
    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE, log = "x", ylab = "Probability", xlab = x.ax.lbl, type = "n")
  } else {
    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE, ylab = "Probability", xlab = x.ax.lbl, type = "n")
  }

  ## (c) Axes (on 4 sides):
  axis(side = 1, at = x.seq, labels = x.lbl, cex.axis = cex.lbl, las = 1,
       pos = 0, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # x at bottom
  axis(side = 1, at = x.seq, labels = FALSE, cex.axis = cex.lbl, las = 1,
       pos = 1, tck = -.01, col.axis = col.axes, col.ticks = col.axes) # x at top
  axis(side = 2, at = y.seq, labels = y.lbl, cex.axis = cex.lbl, las = 1,
       pos = x.min, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # y at left
  axis(side = 4, at = y.seq, labels = y.lbl, cex.axis = cex.lbl, las = 1,
       pos = 1, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # y at right

  ## (d) Grid:
  grid(col = grey(.8, .8))


  ## (3) Plot elements of what: ----------

  ## (+) shortcut to get all what options:
  if ("all" %in% what || "ALL" %in% what || "All" %in% what ) {
    what <- c("prev", "PPV", "NPV", "ppod", "acc")
  }

  ## (a) prev:
  if ("prev" %in% what || "PREV" %in% what || "Prev" %in% what) {

    ## 0. parameters:
    lty.prev <- 2                            # prev line type
    col.prev <- grey(.50, alpha = .99)       # prev color
    legend.lbls <- c(legend.lbls, "prev")    # add prev label
    legend.cols <- c(legend.cols, col.prev)  # add prev color
    legend.ltys <- c(legend.ltys, lty.prev)  # add prev line type

    ## 1. curve: prev as vline
    abline(v = prev, lty = lty.prev, lwd = 1, col = col.prev)  # prev curve/line

    ## 2. point:
    if (show.points) {

      if ((cur.NPV < low.PV) | (cur.PPV < low.PV)) { # y-pos at v.raise:
        points(x = prev, y = 0 + v.raise, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.prev)  # prev point
      } else { # y-pos at bottom (y = 0):
        points(x = prev, y = 0,           pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.prev)  # prev point
      }

      ## 3. label:
      prev.lbl <- paste0("prev = ", as_pc(prev), "%")  # prev label

      if ((cur.NPV < low.PV) | (cur.PPV < low.PV)) { # y at v.raise:
        if ((prev < .50) | !(prev > 1 - h.shift)) {
          text(x = prev + h.shift, y = 0 + v.raise,
               labels = prev.lbl, col = col.prev, cex = cex.lbl) # on right
        } else {
          text(x = prev - h.shift, y = 0 + v.raise,
               labels = prev.lbl, col = col.prev, cex = cex.lbl) # on left+
        }
      } else { # y at bottom (y = 0):
        if ((prev < .50) | !(prev > 1 - h.shift)) {
          text(x = prev + h.shift, y = 0 + v.shift,
               labels = prev.lbl, col = col.prev, cex = cex.lbl) # on right
        } else {
          text(x = prev - h.shift, y = 0 + v.shift,
               labels = prev.lbl, col = col.prev, cex = cex.lbl) # on left+
        }
      }

    } # if (show.points)...

  } # if ("prev" %in% what)...


  ## (b) PPV:
  if ("PPV" %in% what || "ppv" %in% what || "Ppv" %in% what) {

    ## 0. parameters:
    lty.ppv <- 1                            # PPV line type
    col.ppv <- pal["ppv"]                   # PPV color
    legend.lbls <- c(legend.lbls, "PPV")    # add PPV label
    legend.cols <- c(legend.cols, col.ppv)  # add PPV color
    legend.ltys <- c(legend.ltys, lty.ppv)  # add PPV line type

    ## 1. curve:
    curve(expr = comp_PPV(x, sens, spec), from = x.min, to = 1, add = TRUE, lty = lty.ppv, lwd = 2, col = col.ppv)  # PPV curve

    ## 2. point:
    if (show.points) {

      points(x = prev, y = cur.PPV, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.ppv)  # PPV point

      ## 3. label:
      PPV.lbl <- paste0("PPV = ", as_pc(cur.PPV), "%")  # PPV label

      if ((cur.PPV < .75 & !(prev > 1 - h.shift)) || (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.PPV + v.shift,
             labels = PPV.lbl, col = col.ppv, cex = cex.lbl) # on right
      } else {
        text(x = prev - h.shift, y = cur.PPV + v.shift,
             labels = PPV.lbl, col = col.ppv, cex = cex.lbl) # on left+
      }

    } # if (show.points)...

  } # if ("PPV" %in% what)...


  ## (c) NPV:
  if ("NPV" %in% what || "npv" %in% what || "Npv" %in% what) {

    ## 0. parameters:
    lty.npv <- 1                            # PPV line type
    col.npv <- pal["npv"]                   # NPV color
    legend.lbls <- c(legend.lbls, "NPV")    # add NPV label
    legend.cols <- c(legend.cols, col.npv)  # add NPV color
    legend.ltys <- c(legend.ltys, lty.npv)  # add NPV line type

    ## 1. curve:
    curve(expr = comp_NPV(x, sens, spec), from = x.min, to = 1, add = TRUE, lty = lty.npv, lwd = 2, col = col.npv)  # NPV curve

    ## 2. point:
    if (show.points) {

      points(x = prev, y = cur.NPV, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.npv)  # NPV point

      ## 3. label:
      NPV.lbl <- paste0("NPV = ", as_pc(cur.NPV), "%")  # NPV label

      if (cur.NPV > .75 | (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.NPV + v.shift,
             labels = NPV.lbl, col = col.npv, cex = cex.lbl) # on right+
      } else {
        text(x = prev - h.shift, y = cur.NPV - v.shift,
             labels = NPV.lbl, col = col.npv, cex = cex.lbl) # on left-
      }

    } # if (show.points)...

  } # if ("NPV" %in% what)...


  ## (d) ppod:
  if ("ppod" %in% what || "PPOD" %in% what || "Ppod" %in% what) {

    ## 0. parameters:
    cur.ppod <- comp_ppod(prev, sens, spec)  # compute current ppod
    lty.ppod <- 1                            # ppod line type
    col.ppod <- pal["pos"]                   # ppod color
    legend.lbls <- c(legend.lbls, "ppod")    # add NPV label
    legend.cols <- c(legend.cols, col.ppod)  # add NPV color
    legend.ltys <- c(legend.ltys, lty.ppod)  # add NPV line type

    ## 1. curve:
    curve(expr = comp_ppod(x, sens, spec), from = x.min, to = 1, add = TRUE, lty = lty.ppod, lwd = 2, col = col.ppod)  # ppod curve

    ## 2. point:
    if (show.points) {

      points(x = prev, y = cur.ppod, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.ppod)  # ppod point

      ## 3. label:
      ppod.lbl <- paste0("ppod = ", as_pc(cur.ppod), "%")  # ppod label

      if (cur.ppod > .75 | (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.ppod + v.shift,
             labels = ppod.lbl, col = col.ppod, cex = cex.lbl) # on right+
      } else {
        text(x = prev - h.shift, y = cur.ppod - v.shift,
             labels = ppod.lbl, col = col.ppod, cex = cex.lbl) # on left-
      }

    } # if (show.points)...

  } # if ("ppod" %in% what)...


  ## (e) Overall accuracy (acc):
  if ("acc" %in% what || "ACC" %in% what || "Acc" %in% what) {

    ## 0. parameters:
    cur.acc <- comp_acc(prev, sens, spec)   # compute current acc
    lty.acc <- 1                            # acc line type
    col.acc <- pal["hi"]                    # acc color
    legend.lbls <- c(legend.lbls, "acc")    # add acc label
    legend.cols <- c(legend.cols, col.acc)  # add acc color
    legend.ltys <- c(legend.ltys, lty.acc)  # add acc line type

    ## 1. curve:
    curve(expr = comp_acc(x, sens, spec), from = x.min, to = 1, add = TRUE, lty = lty.acc, lwd = 2, col = col.acc)  # acc curve

    ## 2. point:
    if (show.points) {

      points(x = prev, y = cur.acc, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.acc)  # acc point

      ## 3. label:
      acc.lbl <- paste0("acc = ", as_pc(cur.acc), "%")  # acc label

      if (cur.acc > .75 | (prev < h.shift)) {
        text(x = prev + h.shift, y = cur.acc + v.shift,
             labels = acc.lbl, col = col.acc, cex = cex.lbl) # on right+
      } else {
        text(x = prev - h.shift, y = cur.acc - v.shift,
             labels = acc.lbl, col = col.acc, cex = cex.lbl) # on left-
      }

    } # if (show.points)...

  } # if ("acc" %in% what)...


  ## +++ here now +++


  ## (4) Title: ----------

  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Curves as a Function of Prevalence") #, "\n", cur.sens.spec.lbl)

  title(cur.title.lbl, adj = 0.0, line = 1.0, font.main = 1) # (left, raised, normal font)


  ## (5) Margin text: ----------

  ## (a) by condition: 3 basic probabilities
  cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  mtext(cur.cond.lbl, side = 1, line = 2, adj = 0, col = grey(.33, .99), cex = .85)  # print label


  ## (6) Legend: ----------

  if (length(legend.lbls) > 0) { # there is a curve:
    # legend("bottom", legend = c("PPV", "NPV"),
    #       col = c(col.ppv, col.npv), lty = 1, lwd = 2, cex = 1, bty = "o", bg = "white")
    add_legend("topright",
               legend = legend.lbls, lty = legend.ltys, lwd = 2, col = legend.cols,
               cex = .90, horiz = FALSE, bty = 'n')
  }

  ## Return what?
  # return(pp)     # returns plot
  # return()       # returns nothing
  # return("neat") # returns "..."

}

## Check:
{
  # # ways to work:
  # plot_curve()  # => default curves (prev, PPV, NPV)
  # plot_curve(show.points = FALSE)  # => default without points
  # plot_curve(what = c("PPV", "NPV"), show.points = TRUE)  # => prev not shown.
  #
  # # linear vs. log scale:
  # plot_curve(prev = .01, sens = .9, spec = .8)                     # => linear scale
  # plot_curve(prev = .01, sens = .9, spec = .8, log.scale = TRUE)   # => log scale
  #
  # plot_curve(prev = .0001, sens = .7, spec = .6)                   # => linear scale
  # plot_curve(prev = .0001, sens = .7, spec = .6, log.scale = TRUE) # => log scale
}

## -----------------------------------------------
## (+) ToDo:

## - Add more options: ppod, accu, etc.
## - fine-tune positions of labels and legend (on linear vs. log scale)
## - pimp plot (titles, axes, grid, colors, transparency)

## -----------------------------------------------
## eof.
