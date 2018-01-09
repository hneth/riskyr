## plot_code.R | riskyR
## 2018 01 09
## -----------------------------------------------

## Plot PV curves: PPV and NPV as functions of prevalence

## Utility function from
## https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
add_legend <- function(...) {
  opar <- par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  on.exit(par(opar))
  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend(...)
}

plot_PV <- function(prev = num$prev, sens = num$sens, spec = num$spec,
                    show.PVprev = TRUE, show.PVpoints = TRUE, log.scale = FALSE) {

  ## Compute current PVs:
  PPV <- comp_PPV(prev, sens, spec)
  NPV <- comp_NPV(prev, sens, spec)

  ## Text labels:
  prev.lbl <- paste0("prev = ", as_pc(prev), "%")
  PPV.lbl <- paste0("PPV = ", as_pc(PPV), "%")
  NPV.lbl <- paste0("NPV = ", as_pc(NPV), "%")
  sens.spec.lbl <- paste0("(sens = ", as_pc(sens), "%, spec = ", as_pc(spec), "%)")
  title.lbl <- paste0("PV plot of current scenario", "\n", sens.spec.lbl)

  ## Parameters:
  col.prev <- grey(.50, alpha = .99) # prevalence # WAS: col.green.2
  col.axes <- grey(.10, alpha = .99) # axes
  col.bord <- grey(.10, alpha = .50) # borders (e.g., of points)
  cex.lbl <- .8 # size of text labels
  h.shift <- .088
  v.shift <- .030
  low.PV  <- .200 # threshold value for judging PPV or NPV to be low
  v.raise <- .700 # vertical raise of y-prev when PPV or NPV < low.PV

  ## Plot area:
  plot(0, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ylab = "Probability", xlab = "Prevalence", type = "n")

  ## Title:
  title(title.lbl, adj = 0.0, line = 1.0)

  ## Axes:
  axis(side = 1, at = seq(0, 1, by = .1), labels = seq(0, 1, by = .1),
       pos = 0, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # x at bottom
  axis(side = 1, at = seq(0, 1, by = .1), labels = FALSE,
       pos = 1, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # x at top
  axis(side = 2, at = seq(0, 1, by = .1), labels = seq(0, 1, by = .1),
       pos = 0, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # y at left
  axis(side = 4, at = seq(0, 1, by = .1), labels = seq(0, 1, by = .1),
       pos = 1, tck = -.02, col.axis = col.axes, col.ticks = col.axes) # y at right

  ## Grid:
  grid(col = grey(.8, .8))

  ## Curves:
  ## PPV:
  curve(expr = comp_PPV(x, sens, spec), from = 0, to = 1, add = TRUE, lty = 1, lwd = 2, col = col.ppv)
  ## NPV:
  curve(expr = comp_NPV(x, sens, spec), from = 0, to = 1, add = TRUE,  lty = 1, lwd = 2, col = col.npv)

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
      text(x = prev + h.shift, y = PPV + .000, labels = PPV.lbl, col = col.ppv, cex = cex.lbl) # on right
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
             horiz = TRUE, bty = 'n')

}

## Check:
# plot_PV(.01, sens, spec, show.PVprev = TRUE, show.PVpoints = TRUE)
# plot_PV(.50, sens, spec, show.PVprev = TRUE, show.PVpoints = TRUE)
plot_PV(.99, sens, spec, show.PVprev = TRUE, show.PVpoints = TRUE)

## -----------------------------------------------
## (+) ToDo:

## - add txt info (to use in title, ...)
## - add log.scale = TRUE option
## - express axes labels as percentages as well?
## - fine-tune positions of labels and legend
## - prettify plot

## -----------------------------------------------
## eof.
