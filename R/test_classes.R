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

plot.riskyr <- function(obj, plottype = "iconarray", ...) {

  # Plotting functions:
  if (plottype == "iconarray") {

    plot_icons(prev = obj$prev,             # probabilities
               sens = obj$sens, mirt = NA,
               spec = obj$spec, fart = NA,  # was: num$fart,
               N = obj$N,    # ONLY freq used (so far)
               ## Key options: ##
               type = "array",  # needs to be given if random position but nonrandom ident.
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

  if (plottype == "tree") {

    plot_tree(prev = obj$prev,             # probabilities
              sens = obj$sens, mirt = NA,
              spec = obj$spec, fart = NA,  # was: num$fart,
              N = obj$N,    # ONLY freq used (so far)
              ## Options:
              round = TRUE,  # Boolean: round freq (if computed), default: round = TRUE.
              by = "cd",     # 4 perspectives: "cd" by condition, "dc" by decision.
              area = "no",   # 4 area types: "no" none (default), "sq" square, "hr" horizontal rectangles, "vr" vertical rectangles.
              p.lbl = "mix", # 4 probability (edge) label types: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
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
