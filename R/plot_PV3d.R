## plot_PV3d.R | riskyr
## 2018 02 11
## -----------------------------------------------
## Plot a 3d-plane of either PPV or NPV
## as a function of both sens and spec (given prev)

## -----------------------------------------------
## Utility function:

# comp_prob_matrix() (moved to file comp_prob.R)

## -----------------------------------------------
## Plot a 3d-plane of PPV or NPV (using persp):

#' Plot predictive values (PPV or NPV) as a function of
#' sensitivity and specificity.
#'
#' \code{plot_PV3d} draws a plane that illustrates
#' either the positive predictive value (\code{\link{PPV}})
#' or the negative predictive value (\code{\link{NPV}})
#' as a function of
#' a decision's sensitivity \code{\link{sens}} and
#' a decision's specificity value \code{\link{spec}}
#' for a given prevalence (\code{\link{prev}}).
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
#'
#' @param is.ppv Option for showing either
#' \code{\link{PPV}} or \code{\link{NPV}}.
#' Default: \code{is.ppv = TRUE}.
#'
#' @param step.size Option for the step size of the range of
#' \code{\link{sens}} and \code{\link{spec}} values shown.
#' Default: \code{step.size = .05}.
#'
#' @param show.PVpoints Option for showing current
#' \code{\link{PPV}} or \code{\link{NPV}})
#' on the corresponding plane.
#' Default: \code{show.PVpoints = TRUE}.
#'
#'
#' @param cur.theta Horizontal rotation angle (used by \code{\link{persp}}).
#' Default: \code{cur.theta = -45}.
#'
#' @param cur.phi Vertical rotation angle (used by \code{\link{persp}}).
#' Default: \code{cur.phi = 0}.
#'
#' @param cur.d Strength of perspective transformation (used by \code{\link{persp}}).
#' Values of \code{cur.d > 1} will weaken perspective effects,  values \code{cur.d <= 1} exaggerate it.
#' Default: \code{cur.d = 1.5}.
#'
#' @param cur.expand Expansion factor applied to the z coordinates (used by \code{\link{persp}}).
#' Default: \code{cur.expand = 1.1}.
#'
#' @param cur.ltheta Azimuth direction for shading (used by \code{\link{persp}}).
#' Default: \code{cur.ltheta = 200}.
#'
#' @param cur.shade Shading value (used by \code{\link{persp}}).
#' Default: \code{cur.shade = .25}.
#'
#'
#' @param title.lbl The title of the current plot.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param col.pv The color in which the selected predictive value is shown.
#' Default: \code{col.pv = pal["ppv"]}.
#'
#'
#' @examples
#' plot_PV3d()  # => shows PPV plane (using current defaults)
#' plot_PV3d(prev = .5, show.PVpoints = FALSE, step.size = .5, title.lbl = "Quick test")
#' plot_PV3d(prev = .3, is.ppv = FALSE, col.pv = pal["npv"])
#' plot_PV3d(prev = .5, is.ppv = FALSE, step.size = .20, title.lbl = "",
#'           cur.theta = -45, cur.phi = 45, cur.expand = 1.4, col.pv = "firebrick3")
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
#' @importFrom graphics grid
#' @importFrom graphics abline
#' @importFrom graphics curve
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#'
#' @importFrom grDevices trans3d
#'
#' @export

plot_PV3d <- function(prev = num$prev,             # probabilities (3 essential, 2 optional)
                      sens = num$sens, mirt = NA,
                      spec = num$spec, fart = NA,
                      ## Options: ##
                      is.ppv = TRUE,         # switch to toggle between PPV (TRUE) and NPV (FALSE)
                      step.size = .05,       # resolution of matrix (sens.range and spec.range)
                      show.PVpoints = TRUE,  # user options [adjustable by inputs]
                      cur.theta = -45, cur.phi = 0,  # user options for persp() [adjustable inputs]
                      cur.d = 1.5, cur.expand = 1.1, cur.ltheta = 200, cur.shade = .25,  # other persp() parameters [fixed]
                      title.lbl = txt$scen.lbl, col.pv = pal["ppv"]  # custom labels and colors
) {



  ## (0) Collect or compute current probabilities: ----------

  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (A) A provided set of probabilities is valid:

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur.prob from scratch based on current parameters (N and probabilities):
    # cur.prob <- comp_prob(prev = prev, sens = sens, spec = spec, fart = fart)  # compute prob from scratch

    ## Compute and assign current PVs:
    if (is.ppv) {
      cur.PV <- comp_PPV(prev, sens, spec)  # compute PPV from probabilities
      cur.PV.lbl <- paste0("PPV = ", as_pc(cur.PV), "%")
    } else {
      cur.PV <- comp_NPV(prev, sens, spec)  # compute NPV from probabilities
      cur.PV.lbl <- paste0("NPV = ", as_pc(cur.PV), "%")
    }

  } else {

    ## (B) NO valid set of probabilities is provided:
    ## Use current PVs of prob:

    if (is.ppv) {
      cur.PV <- prob$PPV  # use PPV from prob
      cur.PV.lbl <- paste0("PPV = ", as_pc(cur.PV), "%")
    } else {
      cur.PV <- prob$NPV  # use NPV from prob
      cur.PV.lbl <- paste0("NPV = ", as_pc(cur.PV), "%")
    }

  }

  ## (1) Ranges on x- and y-axes: ----------

  ## ToDo: Check that step.size is a reasonable value in [0, 1] range!
  sens.range <- seq(0, 1, by = step.size) # range of sensitivity values (x)
  spec.range <- seq(0, 1, by = step.size) # range of specificity values (y)

  ## Beware of cases in which PPV or NPV are NaN:

  ## (1) PPV is NaN if:
  ##     (a)  (prev = 1) & (sens = 0)
  ##     (b)  (prev = 0) & (spec = 1)
  ##     (c)  (sens = 0) & (spec = 1)  ==> always occurs in ranges above!

  ## (2) NPV is NaN if:
  ##     (a)  (prev = 1) & (sens = 1)
  ##     (b)  (prev = 1) & (sens = 0)
  ##     (c)  (sens = 1) & (spec = 0)  ==> always occurs in ranges above!

  ## Hack fix: Prevent values of 0 from occurring:
  eps <- 10^-9  # some very small value
  sens.range[1] <- sens.range[1] + eps  # to prevent sens = 0 case
  spec.range[1] <- spec.range[1] + eps  # to prevent spec = 0 case


  ## (2) Compute matrices of PPV/NPV values (using dedicated function): ----------

  if (is.ppv) {
    PV.mat <- comp_prob_matrix(prev = prev, sens.range, spec.range, metric = "PPV", nan.adjust = FALSE)
  } else {
    PV.mat <- comp_prob_matrix(prev = prev, sens.range, spec.range, metric = "NPV", nan.adjust = FALSE)
  }

  ## (3) Define graph parameters and labels: ----------

  x <- sens.range
  y <- spec.range
  z.pv <- as.matrix(PV.mat)
  z.lim <- c(0, 1) # range of z-axis
  z.lbl <- if (is.ppv) {"PPV"} else {"NPV"}
  cur.par.lbl <-  paste0("(", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%)")
  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  if (is.ppv) {
    cur.title.lbl <- paste0(title.lbl, "Positive predictive values (PPV)") #, " for a prevalence of ",  as_pc(prev), "%") #, "\n", cur.par.lbl)
  } else {
    cur.title.lbl <- paste0(title.lbl, "Negative predictive values (NPV)") #, " for a prevalence of ",  as_pc(prev), "%") #, "\n", cur.par.lbl)
  }
  col.bord <- grey(.01, alpha = .99) # borders (e.g., of points)
  col.pt <- if (is.ppv) {"yellow1"} else {"yellow1"} # point color should provide high contrasts to col.pv of ppv and npv
  cex.pt <- 1.5 # adjust size of points


  ## (4) Create 3D plot for PV: ----------

  p.pv <- persp(x, y, z.pv,
                theta = cur.theta, phi = cur.phi, d = cur.d, expand = cur.expand,
                col = col.pv, border = NA, # col.ppv, col.orange.1,
                ltheta = cur.ltheta,
                shade = cur.shade,
                ticktype = "detailed",
                nticks = 6, # at 20% intervals
                xlab = "sens",
                ylab = "spec",
                zlab = z.lbl,
                zlim = z.lim #,
                # main = cur.title.lbl # (defined below)
  )

  ## (5) Add points to plot: ----------

  if (show.PVpoints) { # add cur.PV to the plot:

    p.pv.pt <- trans3d(sens, spec, cur.PV, p.pv)
    p.pv <- points(p.pv.pt, pch = 21, col = col.bord, bg = col.pt, lwd = 1.0, cex = cex.pt)

  }


  ## (6) Title: ----------

  title(cur.title.lbl, adj = 0.0, line = 1.0, font.main = 1) # (left, raised, normal font)


  ## (7) Margin text: ----------

  if (show.PVpoints) {
    mtext(paste0(cur.PV.lbl), side = 1, line = 2, adj = 1, col = col.pv, cex = .90, font = 1)
  }
  mtext(paste0(cur.par.lbl), side = 1, line = 3, adj = 1, col = grey(.11, .99), cex = .80)


  ## (8) Return what?: ----------
  # return(p.pv)    # returns plot
  # return()        # returns nothing
  # return("wow!")  # returns "..."

}

## Check:
# plot_PV3d()
# plot_PV3d(prev = .5, show.PVpoints = FALSE, step.size = .5)
# plot_PV3d(prev = .5, is.ppv = FALSE, col.pv = pal["npv"])
# plot_PV3d(prev = .5, is.ppv = FALSE, step.size = .20, title.lbl = "A test",
#           cur.theta = -45, cur.phi = 45, cur.expand = 1.4, col.pv = "firebrick")

## -----------------------------------------------
## OLDER function (no longer used):

{

  ## Plot both PPV and NPV planes in 2 adjacent plots
  ## (combined into 1 plot):
  plot.PV.planes <- function(env, show.PVpoints = TRUE,
                             cur.theta = -45, cur.phi = 0, # persp() parameters [adjustable by user inputs]
                             cur.d = 1.5, cur.expand = 1.1, cur.ltheta = 200, cur.shade = .25 # persp() parameters [fixed]
  ) {

    ## Current environment parameters:
    name <- env$name
    N    <- env$N
    prev <- env$prev
    sens <- env$sens
    spec <- env$spec
    source <- env$source

    ## Current PPV and NPV values and labels:
    ## (a) from current data:
    # cur.PPV <- data$PPV # get.PPV(prev, sens, spec)
    # cur.NPV <- data$NPV # get.NPV(prev, sens, spec)
    # cur.PPV.label <- data$PPV.label # paste0("PPV = ", as_pc(cur.PPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.PPV), "%)")
    # cur.NPV.label <- data$NPV.label # paste0("NPV = ", as_pc(cur.NPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.NPV), "%)")
    ## (b) Compute from scratch:
    cur.PPV <- comp_PPV(prev, sens, spec) # data()$PPV
    cur.NPV <- comp_NPV(prev, sens, spec) # data()$NPV
    cur.PPV.label <- paste0("PPV = ", as_pc(cur.PPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.PPV), "%)")
    cur.NPV.label <- paste0("NPV = ", as_pc(cur.NPV), "%") # paste0("(", as_pc(prev), "%; ", as_pc(cur.NPV), "%)")

    ## Ranges on x- and y-axes:
    sens.range <- seq(0.0, 1.0, by = .05) # range of sensitivity values
    spec.range <- seq(0.0, 1.0, by = .05) # range of specificity values

    ## Compute PPV and NPV matrices:
    PPV.mat <- comp_prob_matrix(prev, sens.range, spec.range, metric = "PPV")
    NPV.mat <- comp_prob_matrix(prev, sens.range, spec.range, metric = "NPV")

    ## Graph parameters:
    x <- sens.range
    y <- spec.range
    z.ppv <- as.matrix(PPV.mat)
    z.npv <- as.matrix(NPV.mat)
    z.lim <- c(0, 1) # range of z-axis
    # cur.par.label <- paste0("(",
    #                         "prev = ", as_pc(prev), "%, ",
    #                         "sens = ", as_pc(sens), "%, ",
    #                         "spec = ", as_pc(spec), "%)")
    cur.par.label <- paste0("(prev = ", as_pc(prev), "%)")

    # Plot 2 plots (adjacent to each other):
    {

      ## Define special graphic settings:
      par(mfrow = c(1, 2)) # Combine 2 plots in 1 row x 2 columns:
      par(bg = "white")

      ## 3D plot for PPV:
      p.ppv <- persp(x, y, z.ppv,
                     theta = cur.theta, phi = cur.phi,  d = cur.d, expand = cur.expand,
                     col = col.ppv, border = NA, # col.ppv, col.orange.1,
                     ltheta = cur.ltheta, shade = cur.shade,
                     ticktype = "detailed", nticks = 6,
                     xlab = "sens", ylab = "spec", zlab = "PPV", zlim = z.lim,
                     main = paste0(cur.PPV.label, "\n", cur.par.label)
      )

      if (show.PVpoints) { # add cur.PPV to plot:
        pmat <- p.ppv
        add.PPV <- trans3d(sens, spec, cur.PPV, pmat)
        points(add.PPV, pch = 21, col = "grey88", bg = col.ppv, lwd = 1.0, cex = 1.3)
      }

      ## 3D plot for NPV:
      p.npv <- persp(x, y, z.npv,
                     theta = cur.theta, phi = cur.phi,  d = cur.d, expand = cur.expand,
                     col = col.npv, border = NA, # col.npv, col.blue.1,
                     ltheta = cur.ltheta, shade = cur.shade,
                     ticktype = "detailed", nticks = 6,
                     xlab = "sens", ylab = "spec", zlab = "NPV", zlim = z.lim,
                     main = paste0(cur.NPV.label, "\n", cur.par.label)
      )

      if (show.PVpoints) { # add cur.NPV to plot:
        pmat <- p.npv
        add.NPV <- trans3d(sens, spec, cur.NPV, pmat)
        points(add.NPV, pch = 21, col = "grey88", bg = col.npv, lwd = 1.0, cex = 1.3)
      }

      ## Remove special graphic settings:
      par(mfrow = c(1, 1))
    }

  }

  ## Check:
  # plot.PV.planes(env, show.PVpoints = TRUE)

}

## -----------------------------------------------
## (+) ToDo:

## - Use ... instead re-naming arguments passed on to persp?
## - Generalize to ANY dependent variable (e.g., acc, wacc, etc.)
##
## - Change labels for all axes to percentages (as in plot_PV)
## - Pimp plot (titles, axes, grid, colors, transparency)

## -----------------------------------------------
## eof.
