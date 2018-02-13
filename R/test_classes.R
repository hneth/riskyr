## test_classes.R | riskyr
## 2018 02 12
## -----------------------------------------------
## Define riskyr class and corresponding methods:
## -----------------------------------------------

# library(riskyr)

## -----------------------------------------------
## (0) Save some exemplary scenarios:

# scenario2 <- scenarios[2, ]
# class(scenario2) <- "riskyr"

# scenario3 <- scenarios[3, ]
# class(scenario3) <- "riskyr"

## -----------------------------------------------
## (1) plot.riskyr function:

plot.riskyr <- function(obj,
                        plottype = "iconarray",  # plottype parameter for type of plot.
                        # type = "array",  # type parameter for plot subtypes.
                        ...  # ellipsis for additional type parameters for the plotting functions.
                             ) {

  ## Testing plottype argument:
  if (!plottype %in% c("network", "tree", "curve", "iconarray", "mosaicplot", "plane")) {
    stop("Invalid plottype specified in plot.riskyr.")
  }

  # Recovering additional parameters:
    arguments <- list(...)
    arg.names <- names(arguments)

    # all:
    if ("show.accu" %in% arg.names) show.accu <- arguments$show.accu else show.accu = TRUE

    # currently network:


    # currently: icon arrays:
    if ("type" %in% arg.names) type <- arguments$type else type <- "array"

    # currently tree:

    if ("area" %in% arg.names) area <- arguments$area else area <- "no"  # tree and net.
    if ("p.lbl" %in% arg.names) p.lbl <- arguments$p.lbl else p.lbl <- "mix"

    # curretly vurve:

    if ("show.points" %in% arg.names) show.points <- arguments$show.points else show.points <- TRUE




  ## Plotting functions:
  ## A. Frequenccy net (default):
    if (plottype == "network") {

      if ("by" %in% arg.names) by <- arguments$by else by <- "cddc"

      plot_fnet(prev = obj$prev,
                sens = obj$sens, mirt = NA,
                spec = obj$spec, fart = NA,
                N = obj$N,
                round = TRUE,
                by = "cddc",
                area = "sq",
                p.lbl = "num",
                show.accu = TRUE,
                w.acc = 0.5,
                title.lbl = obj$scen.lbl,
                popu.lbl = obj$popu.lbl,
                cond.true.lbl = obj$cond.true.lbl,
                cond.false.lbl = obj$cond.false.lbl,
                dec.pos.lbl = obj$dec.pos.lbl,
                dec.neg.lbl = obj$dec.neg.lbl,
                hi.lbl = obj$hi.lbl, mi.lbl = obj$mi.lbl, fa.lbl = obj$fa.lbl,
                cr.lbl = obj$cr.lbl, col.txt = grey(0.01, alpha = 0.99), box.cex = 0.85,
                col.boxes = pal, col.border = grey(0.33, alpha = 0.99), lwd = 1.5,
                box.lwd = 1.5, col.shadow = col.sand.dark, cex.shadow = 0)
    }

  ## B. Frequency tree:
    if (plottype == "tree") {

      if ("by" %in% arg.names) by <- arguments$by else by <- "cd"

      plot_tree(prev = obj$prev,             # probabilities
                sens = obj$sens, mirt = NA,
                spec = obj$spec, fart = NA,  # was: num$fart,
                N = obj$N,    # ONLY freq used (so far)
                ## Options:
                round = TRUE,  # Boolean: round freq (if computed), default: round = TRUE.
                by = by,     # 4 perspectives: "cd" by condition, "dc" by decision.
                area = area,   # 4 area types: "no" none (default), "sq" square, "hr" horizontal rectangles, "vr" vertical rectangles.
                p.lbl = p.lbl, # 4 probability (edge) label types: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
                ## Compute and show accuracy info:
                show.accu = TRUE,  # compute and show accuracy metrics
                w.acc = .50,       # weight w for wacc (from 0 to 1)
                ## Labels:
                title.lbl = obj$scen.lbl,     # custom text labels
                popu.lbl = obj$popu.lbl,
                ## Condition labels:
                cond.true.lbl = obj$cond.true.lbl,
                cond.false.lbl = obj$cond.false.lbl,
                ## Decision labels:
                dec.pos.lbl = obj$dec.pos.lbl,
                dec.neg.lbl = obj$dec.neg.lbl,
                ## SDT combinations:
                hi.lbl = obj$hi.lbl,
                mi.lbl = obj$mi.lbl,
                fa.lbl = obj$fa.lbl,
                cr.lbl = obj$cr.lbl,
                ## Box settings:
                col.txt = grey(.01, alpha = .99),  # black
                box.cex = .90,                     # relative text size
                col.boxes = pal, # pal[c(1:9)],    # box colors (9 frequencies/boxes/colors)
                col.border = grey(.33, alpha = .99),  # grey
                ## Widths of arrows and box borders:
                lwd = 1.6,      # width of arrows
                box.lwd = 1.8,  # set to 0.001 to show boxes without borders (but =0 yields ERROR)
                ## Shadows:
                col.shadow = grey(.11, alpha = .99),  # dark grey
                cex.shadow = 0  # [values > 0 show shadows]
      )

    }

  ## C. Curve:
    if (plottype == "curve") {
      if ("what" %in% arg.names) what <- arguments$what else what <- c("prev", "PPV", "NPV")

      plot_curve(prev = obj$prev,             # probabilities (3 essential, 2 optional)
                  sens = obj$sens, mirt = NA,
                  spec = obj$spec, fart = NA,
                  ## DVs:
                  what = what,  # what curves?  Options: "acc", "ppod"
                  what.col = pal,                  # colors for what.
                  ## Options:
                  show.points = show.points,  # show points at current prev?
                  log.scale = FALSE,   # x-axis on log scale?
                  title.lbl = obj$scen.lbl
      )
    }

  ## D. Iconarrays
    if (plottype == "iconarray") {

      plot_icons(prev = obj$prev,             # probabilities
                 sens = obj$sens, mirt = NA,
                 spec = obj$spec, fart = NA,  # was: num$fart,
                 N = obj$N,    # ONLY freq used (so far)
                 ## Key options: ##
                 type = type,  # needs to be given if random position but nonrandom ident.
                 # Types include: array, shuffled array, mosaic, equal, fillleft, filltop, scatter.
                 ident.order = c("hi", "mi", "fa", "cr"),
                 icon.colors = pal[c("hi", "mi", "fa", "cr")],  # use one color for each usual type.
                 icon.types = 22,  # plotting characters; default square with border
                 icon.border.col = grey(.10, .50),  # border color of icons
                 icon.border.lwd = 1.5, # line width of icons
                 transparency = .50,
                 icon.size = NULL,
                 block.d = NULL,  # distance between blocks (where applicable).
                 border.d = 0.1,  # distance of icons to border.

                 # for classic icon arrays only:
                 block.size.row = 10,
                 block.size.col = 10,
                 nblocks.row = NULL,
                 nblocks.col = NULL,

                 fill.array = "left",
                 fill.blocks = "rowwise",

                 show.accu = TRUE, # Option for showing current accuracy metrics.
                 w.acc = 0.50,

                 # labelling:
                 title.lbl = obj$scen.lbl,
                 type.lbls = obj[c("hi.lbl", "mi.lbl", "fa.lbl", "cr.lbl")])

    }

  ## E. Mosaicplot:
    if (plottype == "mosaicplot") {
      plot_mosaic(prev = obj$prev,
                  sens = obj$sens, mirt = NA,
                  spec = obj$spec, fart = NA,
                  N = obj$N,
                  vsplit = TRUE,
                  show.accu = TRUE, w.acc = 0.5,
                  title.lbl = obj$scen.lbl,
                  col.sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"]))
    }

  ## F. Plane:
    if (plottype == "plane") {

      if ("what" %in% arg.names) what <- arguments$what else what <- "PPV"

      plot_plane(prev = obj$prev,             # probabilities (3 essential, 2 optional)
                 sens = obj$sens, mirt = NA,
                 spec = obj$spec, fart = NA,
                 ## DVs:
                 what = "PPV", # what plane?  Options: "PPV", "NPV", "acc", "ppod".
                 ## Options:
                 what.col = pal,     # color for what.
                 step.size = .05,    # resolution of matrix (sens.range and spec.range)
                 show.point = TRUE,  # show point on plane?
                 ## Main persp() options [adjustable]:
                 theta = -45,
                 phi = 0,
                 ## Text:
                 title.lbl = txt$scen.lbl, # plot title label
                 cex.lbl = .85 # scale size of text labels (e.g., on axes, legend, margin text)
                 )
    }

  # Include other plottypes here.


}


## Check:

# plot(scenario2, plottype = "iconarray")
# plot(scenario3, plottype = "tree")


## -----------------------------------------------
## (2) summary.riskyr function:

summary.riskyr <- function(obj, summarize = "all", ...) {

  cat("Scenario: ", obj$scen.lbl, "\n\n")  # Always show scenario name.

  cat("N = ", obj$N, "\n\n")  # N should be always given.

  ## If probabilities or all is to be shown:
  if (summarize %in% c("all", "probs")) {
    cat("Probabilities:\n")
    probs <- unlist(obj[c("prev", "sens", "spec", "fart")])
    names(probs) <- c("Prevalence", "Sensitivity", "Specificity", "False alarm rate")
    print(probs)
  }


  ## If frequency information is to be shown:
  if (summarize %in% c("all", "freqs")) {
    freqs <- comp_freq(prev = obj$prev, sens = obj$sens, spec = obj$spec,
                       N = obj$N)
    cat("\nFrequencies:\n")
    sdt.freqs <- unlist(freqs[c("hi", "mi", "fa", "cr")])
    names(sdt.freqs) <- c("Hits", "Misses", "False alarms", "Correct rejections")
    print(sdt.freqs)
  }

  # TODO: Extend to accuracy.



}

## Check:
# summary(scenario3)
# summary(scenario2, summarize = "probs")


## -----------------------------------------------
## (3) Define an object with a list of riskyr objects:

scenarios.lst <- vector("list", nrow(scenarios))
names(scenarios.lst) <- paste0("scen", 1:nrow(scenarios))

for (i in 1:nrow(scenarios)) {

  cur.scen <- scenarios[i, ]
  class(cur.scen) <- "riskyr"
  scenarios.lst[[i]] <- cur.scen

}


## Check:
## Calling list elements:

## (a) by number:
# summary(scenarios.lst[[6]])
# plot(scenarios.lst[[6]])

## (b) by names:
# summary(scenarios.lst$scen6)
# plot(scenarios.lst$scen6)


## -----------------------------------------------
## (+) ToDo:

## - consider defining an object class riskyr
##   that binds all elements of the current scenario
##   similar to the (now deprecated) env list.

## -----------------------------------------------
## eof.
