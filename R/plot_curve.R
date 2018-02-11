## plot_curve.R | riskyr
## 2018 02 11
## -----------------------------------------------
## plot_curve: A generalization of plot_PV that
## plots different DVs (e.g., PPV, NPV, acc curves)
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
#' New parameters:
#'
#' @param what A vector of character codes that specify the
#' selection of measures to be plotted.
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
#' Deprecated parameters:
#'
#' @param show.PVprev Option for showing current
#' prevalence \code{\link{prev}} in the plot (as a vertical line).
#' Default: \code{show.PVprev = TRUE}.
#'
#' @param show.PVpoints Option for showing current
#' predictive values (\code{\link{PPV}} and \code{\link{NPV}})
#' on the corresponding curve.
#' Default: \code{show.PVpoints = TRUE}.
#'
#'
#' @param log.scale Boolean value for switching from a linear
#' to a logarithmic x-axis.
#' Default: \code{log.scale = FALSE}.
#'
#' @param title.lbl The title of the current plot.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param col.ppv The color in which the \code{\link{PPV}} is shown.
#' Default: \code{col.ppv = pal["ppv"]}.
#'
#' @param col.npv The color in which the \code{\link{NPV}} is shown.
#' Default: \code{col.npv = pal["npv"]}.
#'
#' @examples
#' plot_PV()
#' plot_PV(prev = .0001, log.scale = TRUE)
#' plot_PV(title.lbl = "My favorite scenario", show.PVprev = FALSE)
#' plot_PV(title.lbl = "", col.ppv = "firebrick3", col.npv = "steelblue3")
#'
#'
#' @family visualization functions
#'
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


plot_curve <- function(prev = num$prev,             # probabilities (3 essential, 2 optional)
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,
                       ## DVs: ##
                       what = c("prev", "PPV", "NPV"),  # what curves?  Options: "acc", "ppod"
                       show.points = TRUE, # show points at current prev?
                       what.col = pal,     # colors for what.
                       ## Options: ##
                       show.PVprev = TRUE,          # user options [adjustable by inputs]
                       show.PVpoints = TRUE,
                       log.scale = FALSE,
                       title.lbl = txt$scen.lbl,    # custom labels and colors
                       col.ppv = pal["ppv"],
                       col.npv = pal["npv"]
) {

  ## (0) Compute or collect current probabilities:
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


  ## Labels:
  prev.lbl <- paste0("prev = ", as_pc(prev), "%")
  cur.PPV.lbl <- paste0("PPV = ", as_pc(cur.PPV), "%")
  cur.NPV.lbl <- paste0("NPV = ", as_pc(cur.NPV), "%")
  cur.PV.lbl <- paste0(cur.PPV.lbl, ", ", cur.NPV.lbl)
  cur.sens.spec.lbl <- paste0("(sens = ", as_pc(sens), "%, spec = ", as_pc(spec), "%)")
  cur.par.lbl <-  paste0("(", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%)")
  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Positive and Negative Predictive Values") #, "\n", cur.sens.spec.lbl)

  if (log.scale) {
    x.ax.lbl <- "Prevalence (on logarithmic scale)"
  } else {
    x.ax.lbl <- "Prevalence"
  }

  if (log.scale) {
    x.seq <- c(10^-5, 10^-4, 10^-3, 10^-2, .10, .25, .50, 1)
    x.lbl <- paste0(as_pc(x.seq, n.digits = 5), "%")  # log percentages (rounded to 5 decimals)
  } else {
    x.seq <- seq(0, 1, by = .1)
    x.lbl <- paste0(as_pc(x.seq), "%")  # linear percentages
  }

  y.seq <- seq(0, 1, by = .1)
  y.lbl <- paste0(as_pc(y.seq), "%")  # linear percentages


  ## Parameters:
  col.prev <- grey(.50, alpha = .99)  # prevalence # WAS: col.green.2
  col.axes <- grey(.10, alpha = .99)  # axes
  col.bord <- grey(.10, alpha = .50)  # borders (e.g., of points)
  cex.lbl <- .8 # size of text labels

  if (log.scale) { x.min <- 10^-6 } else { x.min <- 0 }  # different x.min values for different scales
  if (log.scale) { h.shift <- prev * 2 } else { h.shift <- .080 }
  v.shift <- .025
  low.PV  <- .200  # threshold value for judging PPV or NPV to be low
  v.raise <- .700  # vertical raise of y-prev when PPV or NPV < low.PV


  ## Initialize plot:
  if (log.scale) {

    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE, log = "x", ylab = "Probability", xlab = x.ax.lbl, type = "n")

  } else {

    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE, ylab = "Probability", xlab = x.ax.lbl, type = "n")

  }

  ## Axes (on 4 sides):
  axis(side = 1, at = x.seq, labels = x.lbl, cex.axis = cex.lbl, las = 1,
       pos = 0, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # x at bottom
  axis(side = 1, at = x.seq, labels = FALSE, cex.axis = cex.lbl, las = 1,
       pos = 1, tck = -.01, col.axis = col.axes, col.ticks = col.axes) # x at top
  axis(side = 2, at = y.seq, labels = y.lbl, cex.axis = cex.lbl, las = 1,
       pos = x.min, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # y at left
  axis(side = 4, at = y.seq, labels = y.lbl, cex.axis = cex.lbl, las = 1,
       pos = 1, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # y at right

  ## Grid:
  grid(col = grey(.8, .8))


  ## Plot stuff: ##

  ## Curves:
  curve(expr = comp_PPV(x, sens, spec), from = x.min, to = 1, add = TRUE, lty = 1, lwd = 2, col = col.ppv) # PPV curve
  curve(expr = comp_NPV(x, sens, spec), from = x.min, to = 1, add = TRUE, lty = 1, lwd = 2, col = col.npv) # NPV curve

  ## Lines:
  if (show.PVprev){
    abline(v = prev, lty = 2, lwd = 1, col = col.prev) # prev as vline
  }
  # abline(h = PPV, lty = 3, lwd = 1, col = col.ppv) # PPV as hline
  # abline(h = NPV, lty = 3, lwd = 1, col = col.npv) # NPV as hline
  # segments(x0 = 0, y0 = PPV, x1 = prev, y1 = PPV, lty = 2, lwd = 1.5, col = col.ppv) # PPV to y-axes
  # segments(x0 = 0, y0 = NPV, x1 = prev, y1 = NPV, lty = 2, lwd = 1.5, col = col.npv) # NPV to y-axes

  ## Points:
  ## prev:
  if (show.PVprev){
    if ((cur.NPV < low.PV) | (cur.PPV < low.PV)) { # y at v.raise:
      points(x = prev, y = 0 + v.raise, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.prev)
    } else { # y at bottom (y = 0):
      points(x = prev, y = 0, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.prev)
    }
  }

  if (show.PVpoints) {
    ## PPV:
    points(x = prev, y = cur.PPV, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.ppv)
    ## NPV:
    points(x = prev, y = cur.NPV, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.npv)
  }

  ## Labels:

  ## prev label:
  if (show.PVprev){
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
  }

  if (show.PVpoints) {
    ## PPV label:
    if ((cur.PPV < .75 & !(prev > 1 - h.shift)) | (prev < h.shift)) {
      text(x = prev + h.shift, y = cur.PPV + v.shift,
           labels = cur.PPV.lbl, col = col.ppv, cex = cex.lbl) # on right
    } else {
      text(x = prev - h.shift, y = cur.PPV + v.shift,
           labels = cur.PPV.lbl, col = col.ppv, cex = cex.lbl) # on left+
    }
    ## NPV label:
    if (cur.NPV > .75 | (prev < h.shift)) {
      text(x = prev + h.shift, y = cur.NPV + v.shift,
           labels = cur.NPV.lbl, col = col.npv, cex = cex.lbl) # on right+
    } else {
      text(x = prev - h.shift, y = cur.NPV - v.shift,
           labels = cur.NPV.lbl, col = col.npv, cex = cex.lbl) # on left-
    }
  }

  ## Title:
  title(cur.title.lbl, adj = 0.0, line = 1.0, font.main = 1) # (left, raised, normal font)

  ## Margin text:
  # if (show.PVpoints) {
  #  mtext(paste0(cur.PV.lbl), side = 1, line = 2, adj = 1, col = grey(.11, .99), cex = .90, font = 1)
  # }
  # mtext(paste0(cur.par.lbl), side = 1, line = 3, adj = 1, col = grey(.11, .99), cex = .80)
  mtext(paste0(cur.sens.spec.lbl), side = 1, line = 2, adj = 1, col = grey(.11, .99), cex = .90)


  ## Legend:
  # legend("bottom", legend = c("PPV", "NPV"),
  #       col = c(col.ppv, col.npv), lty = 1, lwd = 2, cex = 1, bty = "o", bg = "white")
  add_legend("topright", legend = c("PPV", "NPV"), lwd = 2, col = c(col.ppv, col.npv),
             horiz = FALSE, bty = 'n')


  ## Return what?
  # return(pp)    # returns plot (as diagram object)
  # return()      # returns nothing
  # return("neat")  # returns "..."

}

## Check:
{
  # plot_PV() # default
  # plot_PV(.01, sens, spec, show.PVprev = TRUE, show.PVpoints = TRUE)
  # plot_PV(.45, sens, spec, show.PVprev = TRUE, show.PVpoints = TRUE)
  # plot_PV(.55, sens, spec, show.PVprev = TRUE, show.PVpoints = TRUE)
  # plot_PV(.99, sens, spec, show.PVprev = TRUE, show.PVpoints = TRUE,
  #         col.ppv = "firebrick", col.npv = "steelblue3")
  ## log scale:
  # plot_PV(prev = .0001, sens = .95, spec = .66, log.scale = TRUE)
  # plot_PV(prev = .0010, sens = .95, spec = .66, log.scale = TRUE)
  # plot_PV(prev = .99, sens = .95, spec = .66, log.scale = TRUE)
}

## -----------------------------------------------
## (+) ToDo:

## - Add documentation.
## - fine-tune positions of labels and legend (on linear vs. log scale)
## - pimp plot (titles, axes, grid, colors, transparency)

## -----------------------------------------------
## eof.
