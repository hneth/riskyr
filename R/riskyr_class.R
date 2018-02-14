## riskyr_class.R | riskyr
## 2018 02 14
## -----------------------------------------------
## Define riskyr class and corresponding methods
## and re-define scenarios.df as a list of
## riskyr objects scenarios:
## -----------------------------------------------


## -----------------------------------------------
## (0) Get some exemplary scenarios and
##     save them with "riskyr" class attribute:

# scenario2 <- scenarios.df[2, ]  # get scenario 2 of scenarios.df
# class(scenario2) <- "riskyr"

# scenario3 <- scenarios.df[3, ]  # get scenario 3 of scenarios.df
# class(scenario3) <- "riskyr"


## -----------------------------------------------
## Dealing with riskyr objects:
## -----------------------------------------------
## (1) plot.riskyr function:

# TODO:  Document plot.riskyr and summary riskyr!
# Follow e.g., ?stats:::print.summary.lm()

plot.riskyr <- function(obj,
                        plottype = "network",  # plottype parameter for type of plot.
                        # type = "array",  # type parameter for plot subtypes.
                        ...  # ellipsis for additional type parameters for the plotting functions.
) {

  ## Test plottype argument:
  if (!plottype %in% c("fnet", "network",
                       "tree", "ftree",
                       "icons", "iconarray",
                       "mosaic", "mosaicplot",
                       "curve", "curves",
                       "plane", "planes")) {
    stop("Invalid plottype specified in plot.riskyr.")
  }

  # Recovering additional parameters:
  arguments <- list(...)
  arg.names <- names(arguments)

  # all:
  if ("show.accu" %in% arg.names) show.accu <- arguments$show.accu else show.accu = TRUE
  if ("area" %in% arg.names) area <- arguments$area else area <- "no"  # tree and net.


  ## Plotting functions: ----------


  ## A. Frequency net (default):
  if ((plottype == "fnet") || (plottype == "network")) {

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
              box.lwd = 1.5, col.shadow = grey(0.11, alpha = 0.99), cex.shadow = 0)

  } # if (plottype == "network")...


  ## B. Frequency tree:
  if ((plottype == "tree") || (plottype == "ftree")) {

    if ("by" %in% arg.names) by <- arguments$by else by <- "cd"
    if ("p.lbl" %in% arg.names) p.lbl <- arguments$p.lbl else p.lbl <- "mix"

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

  } #  if (plottype == "tree")...


  ## C. Mosaicplot:
  if ((plottype == "mosaic") || (plottype == "mosaicplot")) {
    plot_mosaic(prev = obj$prev,
                sens = obj$sens, mirt = NA,
                spec = obj$spec, fart = NA,
                N = obj$N,
                vsplit = TRUE,
                show.accu = TRUE, w.acc = 0.5,
                title.lbl = obj$scen.lbl,
                col.sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"]))

  } # if (plottype == "mosaicplot")...


  ## D. Iconarrays
  if ((plottype == "icons") || (plottype == "iconarray")) {

    if ("type" %in% arg.names) type <- arguments$type else type <- "array"

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

  } #  if (plottype == "iconarray")...


  ## E. Curve:
  if ((plottype == "curve") || (plottype == "curves")) {

    if ("what" %in% arg.names) what <- arguments$what else what <- c("prev", "PPV", "NPV")
    if ("show.points" %in% arg.names) show.points <- arguments$show.points else show.points <- TRUE

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
  } # if (plottype == "curve")...


  ## F. Plane:
  if ((plottype == "plane") || (plottype == "planes")) {

    if ("what" %in% arg.names) what <- arguments$what else what <- "PPV"
    if ("theta" %in% arg.names) theta <- arguments$theta else theta <- -45
    if ("phi" %in% arg.names) phi <- arguments$phi else phi <- 0

    plot_plane(prev = obj$prev,             # probabilities (3 essential, 2 optional)
               sens = obj$sens, mirt = NA,
               spec = obj$spec, fart = NA,
               ## DVs:
               what = what, # what plane?  Options: "PPV", "NPV", "acc", "ppod".
               ## Options:
               what.col = pal,     # color for what.
               step.size = .05,    # resolution of matrix (sens.range and spec.range)
               show.point = TRUE,  # show point on plane?
               ## Main persp() options [adjustable]:
               theta = theta,
               phi = phi,
               ## Text:
               title.lbl = obj$scen.lbl, # plot title label
               cex.lbl = .85 # scale size of text labels (e.g., on axes, legend, margin text)
    )
  } # if (plottype == "plane")...

  ## Add other plottypes:
  ## (...)

}


## Check:

{
  ## (A) with example scenarios (defined above):
  # plot(scenario2, plottype = "icons")
  # plot(scenario3, plottype = "tree")

  ## (B) with scenarios from scenarios (defined BELOW):
  #
  # s25 <- scenarios$n25  # select scenario 25 from scenarios
  #
  # plot(s25)  # => default plot (fnet)
  # plot(s25, plottype = "fnet")  # => network diagram (same as default)
  # plot(s25, plottype = "tree", area = "vr") # => tree diagram (with vertical rectangles)
  # plot(s25, plottype = "curve", what = "all")
  # plot(s25, plottype = "icons")
  # plot(s25, plottype = "icons", type = "mosaic")  # passing on additional parameters.
  # plot(s25, plottype = "mosaic")
  # plot(s25, plottype = "plane", what = "NPV")
  # # plot(s25, plottype = "wetwork")
}


## -----------------------------------------------
## (2) summary.riskyr function: ------------------

## (A) Create a summary object: ------------------

summary.riskyr <- function(obj, summarize = "all", ...) {

  obj.sum <- list()  # initialize as list

  obj.sum$scen.lbl <- obj$scen.lbl

  obj.sum$cond.lbl <- obj$cond.lbl  # condition
  obj.sum$dec.lbl <- obj$dec.lbl    # decision
  obj.sum$popu.lbl <- obj$popu.lbl  # population
  obj.sum$N <- obj$N                # N
  obj.sum$scen.src <- obj$scen.src  # source (short)

  ## (0) If all should be summarized: ----------

  if (summarize == "all") summarize <- c("prob", "freq", "accu")



  ## (A) Probability information: ----------

  if (("prob" %in% summarize) || ("probs" %in% summarize) || ("probabilities" %in% summarize)) {

    # calculate all probabilities:
    probs <- comp_prob_prob(prev = obj$prev, sens = obj$sens, spec = obj$spec)

    probs.ess <- unlist(probs[c("prev", "sens", "mirt", "spec", "fart")])  # essential probabilities.

    probs.ness <- unlist(probs[c("ppod", "PPV", "NPV", "FDR", "FOR")])  # non-essential probabilities.

    obj.sum$probs <- list(probs.ess = probs.ess, probs.ness = probs.ness)

  } # if "prob"...



  ## (B) Frequency information: ----------

  if (("freq" %in% summarize) || ("freqs" %in% summarize) || ("frequencies" %in% summarize)) {

    # calculate frequencies:
    freqs <- comp_freq(prev = obj$prev, sens = obj$sens, spec = obj$spec,
                       N = obj$N)

    ## (a) Frequencies by condition:
    cond.freqs <- unlist(freqs[c("cond.true", "cond.false")])

    ## (b) Frequencies by decision:
    dec.freqs <- unlist(freqs[c("dec.pos", "dec.neg")])

    ## (c) Frequencies by accuracy (i.e., correspondence of decision to condition):
    acc.freqs <- unlist(freqs[c("dec.cor", "dec.err")])

    ## (d) SDT frequencies:
    sdt.freqs <- unlist(freqs[c("hi", "mi", "fa", "cr")])  # == "essential" frequencies.

    ## (+) Add to summary object:
    obj.sum$freqs <- list(cond.freqs = cond.freqs,
                          dec.freqs = dec.freqs,
                          acc.freqs = acc.freqs,
                          sdt.freqs = sdt.freqs)

  } # if "freq"...


  ## (C) Accuracy information: ----------

  if (("acc" %in% summarize) || ("accu" %in% summarize) || ("accuracy" %in% summarize)) {

    ## Overall accuracy acc:
    obj.sum$acc <- comp_acc(prev = obj$prev, sens = obj$sens, spec = obj$spec)

    ## ToDo: ALL accuracy metrics:
    # accu <- comp_accu(prev = obj$prev, sens = obj$sens, spec = obj$spec)

    } # if "acc"...



  ## Add class to summary object: ----------

  class(obj.sum) <- c("summary.riskyr")

  return(obj.sum)

}


## (B) Create corresponding print function: ----------

print.summary.riskyr <- function(obj) {

  ## 1. Always print header: ----------

  cat("Scenario: ",   obj$scen.lbl, "\n\n")  # always show scenario name.

  cat("Condition: ",  obj$cond.lbl, "\n")  # always show current condition.
  cat("Decision: ",   obj$dec.lbl,  "\n")  # always show current decision.
  cat("Population: ", obj$popu.lbl, "\n")  # always show current condition.
  cat("N = ", obj$N, "\n")                 # always show population size N.
  cat("Source: ", obj$scen.src, "\n")      # always show (short) source info

  ## 2. Print only on demand: ----------

  n <- names(obj)  # save names.

  ## (A) Probabilities: ----------

  if ("probs" %in% n) {

    cat("\nProbabilities:\n\n")

    cat(" Essential probabilities:\n")
    # names(obj$probs$probs.ess) <- c("Prevalence", "Sensitivity", "Miss rate", "Specificity", "False alarm rate")  # explicit
    names(obj$probs$probs.ess) <- c("prev", "sens", "mirt", "spec", "fart")  # shorter
    print(obj$probs$probs.ess)

    cat("\n Other probabilities:\n")
    print(round(obj$probs$probs.ness, 3))  # no naming for non-essential probs.
  }

  ## (B) Frequencies: ----------

  if ("freqs" %in% n) {

    cat("\nFrequencies:\n")

    cat("\n by conditions:\n")
    # names(obj$freqs$cond.freqs) <- c("True", "False")  # explicit
    names(obj$freqs$cond.freqs) <- c("cond.true", "cond.false")  # more explicit
    print(obj$freqs$cond.freqs)


    cat("\n by decision:\n")
    names(obj$freqs$dec.freqs) <- c("Positive", "Negative")  # explicit
    names(obj$freqs$dec.freqs) <- c("dec.pos", "dec.neg")  # more explicit
    print(obj$freqs$dec.freqs)

    cat("\n by correspondence (of decision to condition):\n")
    # names(obj$freqs$acc.freqs) <- c("Correct cases", "Incorrect cases")  # explicit
    names(obj$freqs$acc.freqs) <- c("dec.cor", "dec.err")  # implicit
    print(obj$freqs$acc.freqs)

    cat("\n 4 essential (SDT) frequencies:\n")
    # names(obj$freqs$sdt.freqs) <- c("Hits", "Misses", "False alarms", "Correct rejections")  # explicit
    names(obj$freqs$sdt.freqs) <- c("hi", "mi", "fa", "cr")  # implicit
    print(obj$freqs$sdt.freqs)

  }

  ## (C) Accuracy: ----------

  if (("acc" %in% n) || ("accu" %in% n)) {

    cat("\nAccuracy:\n\n")

    cat(" acc:\n")
    cat(obj$acc)  # overall accuracy acc only!

    ## ToDo: Include ALL other accuracy metrics (accu).

  }

}


## Check:
{
  # summary(scenario2)  # => all summaries
  # summary(scenario2, summarize = "freq")
  # summary(scenario2, summarize = "prob")
  # summary(scenario2, summarize = "accu")
}


## -----------------------------------------------
## (3) Function to create riskyr scenarios: ------

riskyr <- function(scen.lbl = txt$scen.lbl, scen.lng = txt$scen.lng,
                   scen.txt = txt$scen.txt, popu.lbl = txt$popu.lbl,
                   cond.lbl = txt$cond.lbl,
                   cond.true.lbl = txt$cond.true.lbl, cond.false.lbl = txt$cond.false.lbl,
                   dec.lbl = txt$dec.lbl,
                   dec.pos.lbl = txt$dec.pos.lbl, dec.neg.lbl = txt$dec.neg.lbl,
                   hi.lbl = txt$hi.lbl, mi.lbl = txt$mi.lbl,
                   fa.lbl = txt$fa.lbl, cr.lbl = txt$cr.lbl,
                   prev = num$prev,
                   sens = num$sens,
                   spec = num$spec, fart = NA,
                   N = freq$N,
                   scen.src = txt$scen.src, scen.apa = txt$scen.apa) {

  # Create object (scenario):
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = NA, spec = spec, fart = fart,
                        tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt = NA, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

  }

  object <- list(scen.lbl = scen.lbl, scen.lng = scen.lng, scen.txt = scen.txt,
                 popu.lbl = popu.lbl, cond.lbl = cond.lbl,
                 cond.true.lbl = cond.true.lbl, cond.false.lbl = cond.false.lbl,
                 dec.lbl = dec.lbl, dec.pos.lbl = dec.pos.lbl, dec.neg.lbl = dec.neg.lbl,
                 hi.lbl = hi.lbl, mi.lbl = mi.lbl, fa.lbl = fa.lbl, cr.lbl = cr.lbl,
                 prev = prev,
                 sens = sens,
                 spec = spec, fart = fart,
                 N = N,
                 scen.src = scen.src, scen.apa = scen.apa)

  # add class riskyr:
  class(object) <- "riskyr"

  return(object)
}


## Check:
{
  # test.obj <- riskyr()  # initialize with default parameters
  # names(test.obj)

  ## Compare with scenarios.df:
  # names(scenarios.df)
  # all.equal(names(test.obj), names(scenarios.df))

  # # cat(
  # #   paste0(
  # #     paste0(names(scenarios$scen1), " = ", names(scenarios$scen1)),
  # #     collapse = ", "))

}

## -----------------------------------------------
## (4) Define an object with a list of riskyr objects:
##     - Convert the data frame scenarios.df into
##       a list "scenarios" of riskyr objects:

scenarios <- vector("list", nrow(scenarios.df))  # initialize scenarios as a list
names(scenarios) <- paste0("n", 1:nrow(scenarios.df))

for (i in 1:nrow(scenarios.df)) {  # for each scenario i in scenarios.df:

  ## (1) define scenario s:
  s <- scenarios.df[i, ]

  ## (2) pass scenario s to riskyr function:
  cur.scen <- riskyr(scen.lbl = s$scen.lbl, scen.lng = s$scen.lng, scen.txt = s$scen.txt,
                     popu.lbl = s$popu.lbl, cond.lbl = s$cond.lbl,
                     cond.true.lbl = s$cond.true.lbl, cond.false.lbl = s$cond.false.lbl,
                     dec.lbl = s$dec.lbl, dec.pos.lbl = s$dec.pos.lbl, dec.neg.lbl = s$dec.neg.lbl,
                     hi.lbl = s$hi.lbl, mi.lbl = s$mi.lbl, fa.lbl = s$fa.lbl, cr.lbl = s$cr.lbl,
                     prev = s$prev,
                     sens = s$sens,
                     spec = s$spec, fart = s$fart,
                     N = s$N,
                     scen.src = s$scen.src, scen.apa = s$scen.apa)  # use initialization function.

  # (3) Add cur.scen (riskyr object) as i-th element of scenarios
  scenarios[[i]] <- cur.scen

  } # end for...

## -----------------------------------------------
## (5) Define scenarios as the list scenarios.lst
##     (of riskyr scenario objects):

# scenarios <- NULL
# scenarios <- scenarios.lst

## Check:
# length(scenarios)
# scenarios$n25  # => shows elements of a scenario


## -----------------------------------------------
## (6) Typical user interaction / session:
## -----------------------------------------------
## (A) Defining and viewing your own scenario:

## ToDo: (...)

## -----------------------------------------------
## (B) Exporing pre-defined scenarios
## -----------------------------------------------
## Standard example: Mammography screening
## Source: Hoffrage et al. (2015), p. 3

# ## (a) Choosing a scenario
# s25 <- scenarios[[25]] # select by number: [[dd]]
# s25 <- scenarios$n25   # select by name:   $ndd

# ## (b) Summary info:
# summary(s25)

# ## (c) Visualization:
# plot(s25)  # => default plot (fnet)
# plot(s25, plottype = "icons")
# plot(s25, plottype = "curve")

## -----------------------------------------------
## Example 2: PSA screening
## Source: Arkes & Gaissmaier (2012), p. 550

## Overview:
# summary(scenarios$n21)

## Visualization:
# plot(scenarios$n21, plottype = "tree", area = "sq")
# plot(scenarios$n21, plottype = "icons")
# plot(scenarios$n21, plottype = "curves", what = "all")
# plot(scenarios$n21, plottype = "planes", what = "PPV")

## Contrast with lower prevalence version:

## Overview:
# summary(scenarios$n22)

## Visualization:
# plot(scenarios$n22, plottype = "tree", area = "sq")
# plot(scenarios$n22, plottype = "icons")
# plot(scenarios$n22, plottype = "curves", what = "all")
# plot(scenarios$n22, plottype = "planes", what = "PPV")

## -----------------------------------------------
## Example 3: Bowel cancer (FOB screening)
## Source: https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Worked_example

# s20 <- scenarios$n20   # select by name:   $ndd

## Overview:
# summary(s20)
# summary(s20, summarize = "freq")

## Visualization:
# plot(s20, plottype = "tree", area = "vr") # => tree diagram (with vertical rectangles)
# plot(s20, plottype = "curve", what = "all")
# plot(s20, plottype = "icons")
# plot(s20, plottype = "icons", type = "mosaic")  # passing on additional parameters.
# plot(s20, plottype = "mosaic")
# plot(s20, plottype = "plane", what = "NPV")
## plot(s20, plottype = "wetwork")


## -----------------------------------------------
## (+) ToDo:

## - Summary: Extend to other types of accuracy in accu
##
## - allow riskyr() to take all kinds of inputs,
##   so that a full object is created.

## -----------------------------------------------
## eof.
