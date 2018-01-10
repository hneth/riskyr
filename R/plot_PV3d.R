## plot_PV3d.R | riskyR
## 2018 01 10
## -----------------------------------------------
## Plot a 3d-plane of either PPV or NPV
## as a function of both sens and spec (given prev)

## -----------------------------------------------
## Utility functions:

# comp_PV_matrix() (moved to file comp_prob.R)

## -----------------------------------------------
## Plot a 3d-plane of PPV or NPV (using persp):

plot_PV3d <- function(prev = num$prev, sens = num$sens, spec = num$spec, # key parameters
                      is.ppv = TRUE, # switch to toggle between PPV (TRUE) and NPV (FALSE)
                      step.size = .05, # detail of matrix resolution (sens.range and spec.range)
                      show.PVpoints = TRUE, # options [adjustable by user inputs]
                      cur.theta = -45, cur.phi = 0, # persp() parameters [adjustable by user inputs]
                      cur.d = 1.5, cur.expand = 1.1, cur.ltheta = 200, cur.shade = .25, # persp() parameters [fixed]
                      title.lbl = txt$scen.lbl, col.pv = pal["ppv"] # custom labels and colors
) {

  ## Current environment parameters:
  # name <- env$name
  # N    <- env$N
  # source <- env$source

  ## Current PPV/NPV value and label:
  ## (a) from current data [assumes prior call of comp_prob()]
  ## (b) compute again:
  prob <- comp_prob(prev, sens, spec)
  if (is.ppv) {
    cur.PV <- prob$ppv
    cur.PV.lbl <- paste0("PPV = ", as_pc(cur.PV), "%")
  } else {
    cur.PV <- prob$npv
    cur.PV.lbl <- paste0("NPV = ", as_pc(cur.PV), "%")
  }

  ## Ranges on x- and y-axes:
  ## ToDo: Check that step.size is a reasonable value in [0, 1] range!
  sens.range <- seq(0, 1, by = step.size) # range of sensitivity values (x)
  spec.range <- seq(0, 1, by = step.size) # range of specificity values (y)

  ## Compute matrices of PPV/NPV values (using utility function):
  if (is.ppv) {
    PV.mat <- comp_PV_matrix(prev, sens.range, spec.range, metric = "PPV")
  } else {
    PV.mat <- comp_PV_matrix(prev, sens.range, spec.range, metric = "NPV")
  }

  ## Graph parameters and labels:
  x <- sens.range
  y <- spec.range
  z.pv <- as.matrix(PV.mat)
  z.lim <- c(0, 1) # range of z-axis
  z.lbl <- if (is.ppv) {"PPV"} else {"NPV"}
  cur.par.lbl <-  paste0("(", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%)")
  if (is.ppv) {
    p.title.lbl <- paste0(title.lbl, ":\n", "Positive predictive values (PPV) for prevalence of ",  as_pc(prev), "%") #, "\n", cur.par.lbl)
  } else {
    p.title.lbl <- paste0(title.lbl, ":\n", "Positive predictive values (PPV) for prevalence of ",  as_pc(prev), "%") #, "\n", cur.par.lbl)
  }
  col.bord <- grey(.01, alpha = .99) # borders (e.g., of points)
  col.pt <- if (is.ppv) {"yellow1"} else {"yellow1"} # point color should provide high contrasts to col.pv of ppv and npv
  cex.pt <- 1.5 # adjust size of points

  ## Create 3D plot for PV:
  p.pv <- persp(x, y, z.pv,
                theta = cur.theta, phi = cur.phi, d = cur.d, expand = cur.expand,
                col = col.pv, border = NA, # col.ppv, col.orange.1,
                ltheta = cur.ltheta, shade = cur.shade,
                ticktype = "detailed", nticks = 6, # at 20% intervals
                xlab = "sens", ylab = "spec", zlab = z.lbl, zlim = z.lim #,
                # main = p.title.lbl # (defined below)
  )

  ## Title:
  title(p.title.lbl, adj = 0.0, line = 0.0, font.main = 1)

  if (show.PVpoints) { # add cur.PV to the plot:
    p.pv.pt <- trans3d(sens, spec, cur.PV, p.pv)
    p.pv <- points(p.pv.pt, pch = 21, col = col.bord, bg = col.pt, lwd = 1.0, cex = cex.pt)
  }

}

## Check:
plot_PV3d()
# plot_PV3d(prev = .5, show.PVpoints = FALSE, step.size = .5)
# plot_PV3d(prev = .5, is.ppv = FALSE, col.pv = pal["npv"])
# plot_PV3d(prev = .5, is.ppv = FALSE, step.size = .20, title.lbl = "A test",
#           cur.theta = -45, cur.phi = 45, cur.expand = 1.4, col.pv = "firebrick")

## -----------------------------------------------
## OLDER function (no longer used):

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
  PPV.mat <- comp_PV_matrix(prev, sens.range, spec.range, metric = "PPV")
  NPV.mat <- comp_PV_matrix(prev, sens.range, spec.range, metric = "NPV")

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

## -----------------------------------------------
## (+) ToDo:

## - add current PPV/NPV value and label somewhere (with all current paramters)
## - change labels for all axes to percentages (as in plot_PV)
## - pimp plot (titles, axes, grid, colors, transparency)

## -----------------------------------------------
## eof.
