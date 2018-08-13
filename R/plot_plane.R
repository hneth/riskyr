## plot_plane.R | riskyr
## 2018 08 13
## -----------------------------------------------
## Plot a 3d-plane of some prob (e.g., PPV or NPV)
## as a function of both sens and spec (for given prev).
## (i.e., generalization of the former plot_PV3d.R).
## -----------------------------------------------
## Utility function:

# comp_prob_matrix() (moved to file comp_prob.R)

## -----------------------------------------------
## Plot a 3d-plane of what (e.g., PPV, NPV, ...)
## (using persp):

#' Plot a plane of selected values (e.g., PPV or NPV)
#' as a function of sensitivity and specificity.
#'
#' \code{plot_plane} draws a 3D-plane of selected values
#' (e.g., predictive values \code{\link{PPV}}
#' or \code{\link{NPV}}) as a function of
#' a decision's sensitivity \code{\link{sens}} and
#' specificity value \code{\link{spec}}
#' for a given prevalence (\code{\link{prev}}).
#'
#' \code{plot_plane} is a generalization of
#' \code{plot_PV3d} (see legacy code)
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
#' @param what A character code that specifies one metric
#' to be plotted as a plane. Currently available
#' options are \code{c("PPV", "NPV", "ppod", "acc")}.
#' Default: \code{what = "PPV"}.
#'
#' @param what.col Color for surface facets corresponding to the metric
#' specified in \code{what}.
#' Default: \code{what.col = pal}.
#'
#' @param line.col Color for lines between surface facets.
#' Default: \code{line.col = "grey85"}.
#'
#' @param show.point Boolean option for showing the current value
#' of the selected metric for the current conditions
#' (\code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}})
#' as a point on the plane.
#' Default: \code{show.point = TRUE}.
#'
#' @param step.size  Sets the granularity of the
#' \code{\link{sens}}-by-\code{\link{spec}} grid.
#' Default: \code{step.size = .05}.
#'
#'
#' @param theta Horizontal rotation angle (used by \code{\link{persp}}).
#' Default: \code{theta = -45}.
#'
#' @param phi Vertical rotation angle (used by \code{\link{persp}}).
#' Default: \code{phi = 0}.
#'
#'
#' @param title.lbl The title of the current plot.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param cex.lbl Scaling factor for the size of text labels
#' (e.g., on axes, legend, margin text).
#' Default: \code{cex.lbl = .85}.
#'
#'
#' @examples
#' # Basics:
#' plot_plane()              # => default plot (what = "PPV")
#' plot_plane(what = "PPV")  # => plane of PPV
#' plot_plane(what = "NPV")  # => plane of NPV
#' plot_plane(what = "ppod") # => plane of ppod
#' plot_plane(what = "acc")  # => plane of acc
#'
#' # Plot options:
#' plot_plane(title.lbl = "Testing smaller text labels", cex.lbl = .60)
#' plot_plane(show.point = FALSE)  # => no point shown on plane
#'
#' plot_plane(title.lbl = "Testing plot colors", what.col = "royalblue4", line.col = "sienna2")
#' plot_plane(title.lbl = "Testing plot in b/w", what.col = "white", line.col = "black")
#'
#' plot_plane(step.size = .333, what.col = "firebrick")    # => coarser granularity + color
#' plot_plane(step.size = .025, what.col = "chartreuse4")  # => finer granularity + color
#' plot_plane(what.col = "steelblue4", theta = -90, phi = 45)   # => rotated, from above
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
#' @importFrom graphics par
#' @importFrom graphics persp
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#'
#' @importFrom grDevices trans3d
#'
#' @export

plot_plane <- function(prev = num$prev,             # probabilities (3 essential, 2 optional)
                       sens = num$sens, mirt = NA,
                       spec = num$spec, fart = NA,
                       ## DVs:
                       what = "PPV",  # what metric?  Options: "PPV", "NPV", "acc", "ppod".
                       ## Options:
                       what.col = pal,       # color for facets of what (i.e., metric specified above)
                       line.col = "grey85",  # color for lines between facets
                       show.point = TRUE,    # show point on plane
                       step.size = .05,      # resolution of matrix (sens.range and spec.range)
                       ## Main persp() options [adjustable]:
                       theta = -45,
                       phi = 0,
                       ## Text:
                       title.lbl = txt$scen.lbl, # plot title label
                       cex.lbl = .85             # scale size of text labels (e.g., on axes, legend, margin text)
) {

  ## (0) Collect or compute current probabilities: ----------

  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (A) A provided set of probabilities is valid:

    ## Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

  } else {

    ## (B) NO valid set of probabilities is provided:

    ## Use current values of prob:
    prev <- prob$prev
    sens <- prob$sens
    spec <- prob$spec

  } # if (is_valid_prob_set(prev...


  ## (1) Ranges on x- and y-axes: ----------

  ## ToDo: Check that step.size is a reasonable value in [0, 1] range!
  sens.range <- seq(0, 1, by = step.size) # range of sensitivity values (x)
  spec.range <- seq(0, 1, by = step.size) # range of specificity values (y)


  ## (2) Interpret what argument: ----------

  ## (a) express what in lower case:
  what <- tolower(what)

  ## (b) shortcut to get all what options:
  if ((what %in%  c("ppv", "npv", "acc", "ppod")) == FALSE) {
    warning("Invalid what argument chosen: Using PPV instead...")
    what <- tolower("PPV")
  }


  ## (2) Determine current parameters and matrix for selected metric: ----------

  ## (a) PPV:
  if (what == "ppv") {

    ## 1. Parameters:
    cur.val <- comp_PPV(prev, sens, spec)             # cur.val (PPV)
    cur.lbl <- paste0("PPV = ", as_pc(cur.val), "%")  # cur.lbl
    sub.title.lbl <- "Plane of positive predictive values (PPV)"
    if (length(what.col) == 1) { cur.col <- what.col } else { cur.col <- pal["ppv"] }  # cur.col
    z.lbl <- "PPV"    # label of z-axis
    z.lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    ## Hack fix: Prevent values of 0 from occurring:
    eps <- 10^-9  # some very small value
    sens.range[1] <- sens.range[1] + eps  # to prevent sens = 0 case
    spec.range[1] <- spec.range[1] + eps  # to prevent spec = 0 case

    cur.mat <- comp_prob_matrix(prev = prev, sens.range, spec.range, metric = "PPV", nan.adjust = FALSE)

  } # if (what == "ppv")...

  ## (b) NPV:
  if (what == "npv") {

    ## 1. Parameters:
    cur.val <- comp_NPV(prev, sens, spec)             # cur.val (NPV)
    cur.lbl <- paste0("NPV = ", as_pc(cur.val), "%")  # cur.lbl
    sub.title.lbl <- "Plane of negative predictive values (NPV)"
    if (length(what.col) == 1) { cur.col <- what.col } else { cur.col <- pal["npv"] }  # cur.col
    z.lbl <- "NPV"    # label of z-axis
    z.lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    ## Hack fix: Prevent values of 0 from occurring:
    eps <- 10^-9  # some very small value
    sens.range[1] <- sens.range[1] + eps  # to prevent sens = 0 case
    spec.range[1] <- spec.range[1] + eps  # to prevent spec = 0 case

    cur.mat <- comp_prob_matrix(prev = prev, sens.range, spec.range, metric = "NPV", nan.adjust = FALSE)

  } # if (what == "npv")...

  ## (c) ppod:
  if (what == "ppod") {

    ## 1. Parameters:
    cur.val <- comp_ppod(prev, sens, spec)             # cur.val (ppod)
    cur.lbl <- paste0("ppod = ", as_pc(cur.val), "%")  # cur.lbl
    sub.title.lbl <- "Plane of the proportion of positive predictions (ppod)"
    if (length(what.col) == 1) { cur.col <- what.col } else { cur.col <- pal["pos"] }  # cur.col for ppod (using "pos")
    z.lbl <- "ppod"   # label of z-axis
    z.lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    # ## Hack fix: Prevent values of 0 from occurring:
    # eps <- 10^-9  # some very small value
    # sens.range[1] <- sens.range[1] + eps  # to prevent sens = 0 case
    # spec.range[1] <- spec.range[1] + eps  # to prevent spec = 0 case

    cur.mat <- comp_prob_matrix(prev = prev, sens.range, spec.range, metric = "ppod", nan.adjust = FALSE)

  } # if (what == "ppod")...

  ## (d) acc:
  if (what == "acc") {

    ## 1. Parameters:
    cur.val <- comp_acc(prev, sens, spec)             # cur.val (acc)
    cur.lbl <- paste0("acc = ", as_pc(cur.val), "%")  # cur.lbl
    sub.title.lbl <- "Plane of accuracy values (acc)"
    if (length(what.col) == 1) { cur.col <- what.col } else { cur.col <- pal["hi"] }  # cur.col for acc (using "hi")
    z.lbl <- "acc"    # label of z-axis
    z.lim <- c(0, 1)  # range of z-axis

    ## 2. Matrix:
    # ## Hack fix: Prevent values of 0 from occurring:
    # eps <- 10^-9  # some very small value
    # sens.range[1] <- sens.range[1] + eps  # to prevent sens = 0 case
    # spec.range[1] <- spec.range[1] + eps  # to prevent spec = 0 case

    cur.mat <- comp_prob_matrix(prev = prev, sens.range, spec.range, metric = "acc", nan.adjust = FALSE)

  } # if (what == "acc")...

  ## (3) Define persp parameters: ----------

  x <- sens.range
  y <- spec.range
  z <- as.matrix(cur.mat)

  ## Additional persp() parameters [currently fixed]:
  d = 1.5
  expand = 1.1
  ltheta = 200
  shade = .300  # default was .25, NULL implies no shade

  line.wd = .6  # lwd parameter (for axes and lines between surface facets); default = 1.

  ## (4) Draw 3D plane (of z) with persp: ----------

  plane <- persp(x, y, z,
                 theta = theta, phi = phi, d = d, expand = expand,  # perspective
                 col = cur.col,     # color of surface facets
                 # border = NA,     # color of line between surface facets (NA disables borders)
                 border = line.col, # color of line between surface facets (NA disables borders)
                 ltheta = ltheta, shade = shade, # illumination
                 ticktype = "detailed",
                 nticks = 6, # at 20% intervals
                 xlab = "sens",
                 ylab = "spec",
                 zlab = z.lbl,
                 zlim = z.lim,
                 cex = cex.lbl,
                 cex.axis = cex.lbl,
                 cex.lab = cex.lbl,
                 # optional:
                 lwd = line.wd  # width of border and axes lines
  )

  ## (5) Add cur.val as point to plot: ----------

  if (show.point) {

    ## Parameters:
    pt.pch <- 21        # symbol of point
    pt.cex <- 1.5       # scale point size
    pt.lwd <- 1.0       # line width of point border
    pt.col <- "yellow1" # point color
    bd.col <- grey(.01, alpha = .99) # border color

    ## Add point to plot:
    proj.pt <- trans3d(sens, spec, cur.val, plane)
    plane <- points(proj.pt, pch = pt.pch, col = bd.col, bg = pt.col, lwd = pt.lwd, cex = pt.cex)

  }


  ## (6) Title: ----------

  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, sub.title.lbl)

  title(cur.title.lbl, adj = 0.0, line = 1.0, font.main = 1) # (left, raised, normal font)


  ## (7) Margin text: ----------

  ## (a) by condition: 3 basic probabilities
  cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  mtext(cur.cond.lbl, side = 1, line = 3, adj = 1, col = grey(.33, .99), cex = cex.lbl)  # print label

  if (show.point) {
    mtext(paste0(cur.lbl), side = 1, line = 2, adj = 1, col = cur.col, cex = (cex.lbl + .05), font = 1)
  }


  ## (8) Return what?: ----------
  # return(p.pv)    # returns plot
  # return()        # returns nothing
  # return("wow!")  # returns "..."

}

## Check:

{
  # # Basics:
  # plot_plane()  # => current defaults (what = "PPV")
  # plot_plane(what = "PPV")  # => plane of PPV
  # plot_plane(what = "NPV")  # => plane of NPV
  # plot_plane(what = "ppod") # => plane of ppod
  # plot_plane(what = "ppod", theta = 45) # => plane of ppod
  # plot_plane(what = "acc")  # => plane of acc
  #
  # # Options:
  # plot_plane(show.point = FALSE)  # => no point shown on plane
  # plot_plane(step.size = .333, what.col = "firebrick")  # => coarser granularity + color
  # plot_plane(step.size = .025, what.col = "chartreuse4")  # => finer granularity + color
  # plot_plane(what.col = "steelblue4", theta = -90, phi = 45)  # => rotated, from above
  # plot_plane(title.lbl = "Testing plot options")
  # plot_plane(title.lbl = "Testing plot colors", what.col = "royalblue4", line.col = "sienna2")
  # plot_plane(title.lbl = "Testing plot in b/w", what.col = "white", line.col = "black")
}

## -----------------------------------------------
## Note:

## The following persp() parameters are currently fixed:
#
# d = 1.5
# expand = 1.1
# ltheta = 200
# shade = .25
#
# From Documentation:
#
# #' @param d Strength of perspective transformation (used by \code{\link{persp}}).
# #' Values of \code{d > 1} will weaken perspective effects,  values \code{d <= 1} exaggerate it.
# #' Default: \code{d = 1.5}.
# #'
# #' @param expand Expansion factor applied to the z coordinates (used by \code{\link{persp}}).
# #' Default: \code{expand = 1.1}.
# #'
# #' @param ltheta Azimuth direction for shading (used by \code{\link{persp}}).
# #' Default: \code{ltheta = 200}.
# #'
# #' @param shade Shading value (used by \code{\link{persp}}).
# #' Default: \code{shade = .25}.
#

## -----------------------------------------------
## OLDER function (2 in 1 plot):

{

  # ## Plot both PPV and NPV planes in 2 adjacent plots
  # ## (combined into 1 plot):
  # plot.PV.planes <- function(env, show.PVpoints = TRUE,
  #                            theta = -45, phi = 0, # persp() parameters [adjustable by user inputs]
  #                            d = 1.5, expand = 1.1, ltheta = 200, shade = .25 # persp() parameters [fixed]
  # ) {
  #
  #   ## Current environment parameters:
  #   name <- env$name
  #   N    <- env$N
  #   prev <- env$prev
  #   sens <- env$sens
  #   spec <- env$spec
  #   source <- env$source
  #
  #   ## Current PPV and NPV values and labels:
  #   ## (a) from current data:
  #   # cur.PPV <- data$PPV # get.PPV(prev, sens, spec)
  #   # cur.NPV <- data$NPV # get.NPV(prev, sens, spec)
  #   # cur.PPV.label <- data$PPV.label # paste0("PPV = ", as_pc(cur.PPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.PPV), "%)")
  #   # cur.NPV.label <- data$NPV.label # paste0("NPV = ", as_pc(cur.NPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.NPV), "%)")
  #   ## (b) Compute from scratch:
  #   cur.PPV <- comp_PPV(prev, sens, spec) # data()$PPV
  #   cur.NPV <- comp_NPV(prev, sens, spec) # data()$NPV
  #   cur.PPV.label <- paste0("PPV = ", as_pc(cur.PPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.PPV), "%)")
  #   cur.NPV.label <- paste0("NPV = ", as_pc(cur.NPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.NPV), "%)")
  #
  #   ## Ranges on x- and y-axes:
  #   sens.range <- seq(0.0, 1.0, by = .05) # range of sensitivity values
  #   spec.range <- seq(0.0, 1.0, by = .05) # range of specificity values
  #
  #   ## Compute PPV and NPV matrices:
  #   PPV.mat <- comp_prob_matrix(prev, sens.range, spec.range, metric = "PPV")
  #   NPV.mat <- comp_prob_matrix(prev, sens.range, spec.range, metric = "NPV")
  #
  #   ## Graph parameters:
  #   x <- sens.range
  #   y <- spec.range
  #   z.ppv <- as.matrix(PPV.mat)
  #   z.npv <- as.matrix(NPV.mat)
  #   z.lim <- c(0, 1) # range of z-axis
  #   # cur.par.label <- paste0("(",
  #   #                         "prev = ", as_pc(prev), "%, ",
  #   #                         "sens = ", as_pc(sens), "%, ",
  #   #                         "spec = ", as_pc(spec), "%)")
  #   cur.par.label <- paste0("(prev = ", as_pc(prev), "%)")
  #
  #   # Plot 2 plots (adjacent to each other):
  #   {
  #
  #     ## Define special graphic settings:
  #     par(mfrow = c(1, 2)) # Combine 2 plots in 1 row x 2 columns:
  #     par(bg = "white")
  #
  #     ## 3D plot for PPV:
  #     p.ppv <- persp(x, y, z.ppv,
  #                    theta = theta, phi = phi,  d = d, expand = expand,
  #                    col = col.ppv, border = NA, # col.ppv, col.orange.1,
  #                    ltheta = ltheta, shade = shade,
  #                    ticktype = "detailed", nticks = 6,
  #                    xlab = "sens", ylab = "spec", zlab = "PPV", zlim = z.lim,
  #                    main = paste0(cur.PPV.label, "\n", cur.par.label)
  #     )
  #
  #     if (show.PVpoints) { # add cur.PPV to plot:
  #       pmat <- p.ppv
  #       add.PPV <- trans3d(sens, spec, cur.PPV, pmat)
  #       points(add.PPV, pch = 21, col = "grey88", bg = col.ppv, lwd = 1.0, cex = 1.3)
  #     }
  #
  #     ## 3D plot for NPV:
  #     p.npv <- persp(x, y, z.npv,
  #                    theta = theta, phi = phi,  d = d, expand = expand,
  #                    col = col.npv, border = NA, # col.npv, col.blue.1,
  #                    ltheta = ltheta, shade = shade,
  #                    ticktype = "detailed", nticks = 6,
  #                    xlab = "sens", ylab = "spec", zlab = "NPV", zlim = z.lim,
  #                    main = paste0(cur.NPV.label, "\n", cur.par.label)
  #     )
  #
  #     if (show.PVpoints) { # add cur.NPV to plot:
  #       pmat <- p.npv
  #       add.NPV <- trans3d(sens, spec, cur.NPV, pmat)
  #       points(add.NPV, pch = 21, col = "grey88", bg = col.npv, lwd = 1.0, cex = 1.3)
  #     }
  #
  #     ## Remove special graphic settings:
  #     par(mfrow = c(1, 1))
  #   }
  #
  # }
  #
  # ## Check:
  # # plot.PV.planes(env, show.PVpoints = TRUE)

}

## -----------------------------------------------
## (+) ToDo:

## - Use ... instead re-naming arguments passed on to persp?
## - Generalize to additional metrics (e.g., wacc, mcc, etc.)
## - Change labels for all axes to percentages (as in plot_curve)
## - Pimp plot (titles, axes, grid, colors, transparency)

## -----------------------------------------------
## eof.
