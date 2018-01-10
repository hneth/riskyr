## plot_PV.R | riskyR
## 2018 01 10
## -----------------------------------------------
## plot_PV: PPV and NPV curves as functions of prevalence

## -----------------------------------------------
## Utility function:

## add_legend() moved to comp_util.R

## -----------------------------------------------
## Plot PV curves: PPV and NPV as functions of prevalence
## (using only necessary arguments with good defaults):

plot_PV <- function(prev = num$prev, sens = num$sens, spec = num$spec,           # key parameters
                    show.PVprev = TRUE, show.PVpoints = TRUE, log.scale = FALSE, # options [adjustable by user inputs]
                    scen.lbl = txt$scen.lbl,                   # custom labels
                    col.ppv = pal["ppv"], col.npv = pal["npv"] # custom colors
                    ) {

  ## Compute current PVs:
  PPV <- comp_PPV(prev, sens, spec)
  NPV <- comp_NPV(prev, sens, spec)

  ## Labels:
  prev.lbl <- paste0("prev = ", as_pc(prev), "%")
  PPV.lbl <- paste0("PPV = ", as_pc(PPV), "%")
  NPV.lbl <- paste0("NPV = ", as_pc(NPV), "%")
  sens.spec.lbl <- paste0("(sens = ", as_pc(sens), "%, spec = ", as_pc(spec), "%)")
  title.lbl <- paste0("Predictive Values of ", scen.lbl, "\n", sens.spec.lbl)
  if (log.scale) {
    x.ax.lbl <- "Prevalence (on logarithmic scale)"
    } else {
      x.ax.lbl <- "Prevalence"
  }
  if (log.scale) {
    x.seq <- c(10^-5, 10^-4, 10^-3, 10^-2, .10, .25, .50, 1)
    x.lbl <- paste0(as_pc(x.seq, n.digits = 5), "%") # log percentages (rounded to 5 decimals)
  } else {
    x.seq <- seq(0, 1, by = .1)
    x.lbl <- paste0(as_pc(x.seq), "%") # linear percentages
  }
  y.seq <- seq(0, 1, by = .1)
  y.lbl <- paste0(as_pc(y.seq), "%") # linear percentages

  ## Parameters:
  col.prev <- grey(.50, alpha = .99) # prevalence # WAS: col.green.2
  col.axes <- grey(.10, alpha = .99) # axes
  col.bord <- grey(.10, alpha = .50) # borders (e.g., of points)
  cex.lbl <- .8 # size of text labels

  if (log.scale) { x.min <- 10^-6 } else { x.min <- 0 }
  if (log.scale) { h.shift <- prev * 2 } else { h.shift <- .080 }
  v.shift <- .025
  low.PV  <- .200 # threshold value for judging PPV or NPV to be low
  v.raise <- .700 # vertical raise of y-prev when PPV or NPV < low.PV


  ## Initialize plot:
  if (log.scale) {
    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE, log = "x", ylab = "Probability", xlab = x.ax.lbl, type = "n")
  } else {
    plot(0, xlim = c(x.min, 1), ylim = c(0, 1), axes = FALSE, ylab = "Probability", xlab = x.ax.lbl, type = "n")
  }

  ## Title:
  title(title.lbl, adj = 0.0, line = 1.0, font.main = 1)

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
    if ((NPV < low.PV) | (PPV < low.PV)) { # y at v.raise:
      points(x = prev, y = 0 + v.raise, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.prev)
    } else { # y at bottom (y = 0):
      points(x = prev, y = 0, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.prev)
    }
  }

  if (show.PVpoints) {
    ## PPV:
    points(x = prev, y = PPV, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.ppv)
    ## NPV:
    points(x = prev, y = NPV, pch = 21, cex = 2, lwd = 1.5, col = col.bord, bg = col.npv)
  }

  ## Labels:
  ## prev label:
  if (show.PVprev){
    if ((NPV < low.PV) | (PPV < low.PV)) { # y at v.raise:
      if ((prev < .50) | !(prev > 1 - h.shift)) {
        text(x = prev + h.shift, y = 0 + v.raise, labels = prev.lbl, col = col.prev, cex = cex.lbl) # on right
      } else {
        text(x = prev - h.shift, y = 0 + v.raise, labels = prev.lbl, col = col.prev, cex = cex.lbl) # on left+
      }
    } else { # y at bottom (y = 0):
      if ((prev < .50) | !(prev > 1 - h.shift)) {
        text(x = prev + h.shift, y = 0 + v.shift, labels = prev.lbl, col = col.prev, cex = cex.lbl) # on right
      } else {
        text(x = prev - h.shift, y = 0 + v.shift, labels = prev.lbl, col = col.prev, cex = cex.lbl) # on left+
      }
    }
  }

  if (show.PVpoints) {
    ## PPV label:
    if ((PPV < .75 & !(prev > 1 - h.shift)) | (prev < h.shift)) {
      text(x = prev + h.shift, y = PPV + v.shift, labels = PPV.lbl, col = col.ppv, cex = cex.lbl) # on right
    } else {
      text(x = prev - h.shift, y = PPV + v.shift, labels = PPV.lbl, col = col.ppv, cex = cex.lbl) # on left+
    }
    ## NPV label:
    if (NPV > .75 | (prev < h.shift)) {
      text(x = prev + h.shift, y = NPV + v.shift, labels = NPV.lbl, col = col.npv, cex = cex.lbl) # on right+
    } else {
      text(x = prev - h.shift, y = NPV - v.shift, labels = NPV.lbl, col = col.npv, cex = cex.lbl) # on left-
    }
  }

  ## Legend:
  # legend("bottom", legend = c("PPV", "NPV"),
  #       col = c(col.ppv, col.npv), lty = 1, lwd = 2, cex = 1, bty = "o", bg = "white")
  add_legend("topright", legend = c("PPV", "NPV"), lwd = 2, col = c(col.ppv, col.npv),
             horiz = FALSE, bty = 'n')

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

## - fine-tune positions of labels and legend (on linear vs. log scale)
## - pimp plot (titles, axes, grid, colors, transparency)

## -----------------------------------------------
## eof.
