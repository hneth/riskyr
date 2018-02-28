## plot_mosaic_2.R | riskyr
## 2018 02 27
## -----------------------------------------------
## Plot mosaicplot that expresses freq as area
## (size and proportion)
## from 3 essential probabilities (prev, sens, spec)
## or current population data.frame popu.

## -----------------------------------------------
## Own version without Dependencies:

# library("vcd")   # moved to "Imports:" in in DESCRIPTION!
# library("grid")

## -----------------------------------------------
## plot_mosaic_2: Plot mosaic plot (without "vcd" and "grid")
## using only necessary arguments with good defaults:
## -----------------------------------------------

## Helper function: Draw an (arrow) line between 2 points and label it:
help_line <- function(x0, y0, x1, y1,  # coordinates of p1 and p2
                      col, col.bord,   # colors (for line, points, and labels)
                      lty, lwd,        # lines
                      pt.pch, pt.cex, pt.lwd,  # points
                      lbl.x, lbl.y, lbl.txt, lbl.pos, lbl.cex  # text label
) {

  arrow <- FALSE

  if (arrow) {

    ## Draw an arrow:
    arrows(x0, y0, x1, y1,
           length = .06, angle = 33, code = 3,  # V shape (small)
           # length = .08, angle = 90, code = 3,    # T shape
           lty = lty, lwd = lwd, col = col)  # arrow

  } else {

    ## Normal line with 2 points at line ends:
    arrows(x0, y0, x1, y1,
           length = 0, angle = 0, code = 3,  # no arrows
           lty = lty, lwd = lwd, col = col)
    points(x0, y0, pch = pt.pch, cex = pt.cex,
           lwd = pt.lwd, col = col.bord, bg = col)  # point 1
    points(x1, y1, pch = pt.pch, cex = pt.cex,
           lwd = pt.lwd, col = col.bord, bg = col)  # point 2

  }

  ## Text label:
  text(lbl.x, lbl.y, labels = lbl.txt, pos = lbl.pos, col = col, cex = lbl.cex)  # label

}


#' Plot a mosaic plot of population frequencies.
#'
#' \code{plot_mosaic_2} draws a mosaic plot that
#' represents the proportions of frequencies in the current
#' population \code{\link{popu}} as relatives sizes of
#' rectangular areas.
#'
#' If a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' is provided, new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch.  Otherwise, the existing
#' population \code{\link{popu}} is shown.
#'
#' Rectangles corresponding to the areas of the mosaic plot
#' can be visualized byopting for vertical rectangles (by selecting
#' the option \code{box = "vr"}) in \code{\link{plot_tree}}
#' and \code{\link{plot_fnet}}.
#'
#' \code{plot_mosaic} requires and uses the R packages "vcd" and
#' "grid" (\code{library("vcd", "grid")}).
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
#' @param N The number of individuals in the population.
#' (This value is not represented in the plot,
#' but used when new frequency information \code{\link{freq}}
#' and a new population table \code{\link{popu}}
#' are computed from scratch from current probabilities.)
#'
#' @param by A character code specifying the perspective
#' (or 1st category by which the population is split into subsets)
#' with 2 options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition (vertical split first);
#'   \item \code{"dc"} ... by decision (horizontal split first).
#'   }
#'
#' @param show.freq Boolean option for showing frequencies
#' (i.e., of \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, and
#' \code{\link{cr}}) in the plot.
#' Default: \code{show.freq = FALSE}.
#'
#' @param show.prob Boolean option for showing visual help lines to mark
#' generating metrics (e.g., \code{\link{prev}}, \code{\link{sens}}, and
#' \code{\link{spec}}) in the plot.
#' Default: \code{show.prob = FALSE}.
#'
#' @param show.accu Option for showing current
#' accuracy metrics \code{\link{accu}} in the plot.
#' Default: \code{show.accu = TRUE}.
#'
#' @param w.acc Weigthing parameter \code{w} used to compute
#' weighted accuracy \code{w.acc} in \code{\link{comp_accu}}.
#' Default: \code{w.acc = .50}.
#'
#' @param title.lbl Text label for current plot title.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param col.sdt Colors for cases of 4 essential frequencies.
#' Default: \code{col.sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])}.
#'
#' @examples
#' # Basics:
#' plot_mosaic()                # => default options
#' plot_mosaic(title.lbl = "")  # => no title
#' plot_mosaic(by = "dc")       # => by decision (horizontal split 1st)
#' plot_mosaic(title.lbl = "My favorite scenario", col.sdt = "goldenrod")
#'
#' # Accuracy:
#' plot_mosaic(show.accu = TRUE)               # => default w = .5 (balanced accuracy "bacc")
#' plot_mosaic(show.accu = TRUE, w.acc = 1/3)  # => (weighted accuracy "wacc")
#' plot_mosaic(show.accu = FALSE)              # => no accuracy info.
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics box
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics abline
#' @importFrom graphics rect
#' @importFrom graphics arrows
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom graphics title
#' @importFrom graphics mtext
#' @importFrom graphics legend
#'
#' @family visualization functions
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

plot_mosaic_2 <- function(prev = num$prev,             # probabilities
                          sens = num$sens, mirt = NA,
                          spec = num$spec, fart = NA,
                          N = num$N,  # not needed in Mosaic plot (but used in comp_freq below)
                          ## Options:
                          by = "cd",  # "cd"...condition 1st vs. "dc"...decision 1st
                          show.freq = FALSE,  # show labels of 4 frequencies in plot
                          show.prob = FALSE,  # show help_line (for metrics, e.g., prev, sens, spec)?
                          show.accu = TRUE,  # compute and show accuracy metrics
                          w.acc = .50,       # weight w for wacc (from 0 to 1)
                          ## Text and color options: ##
                          title.lbl = txt$scen.lbl,
                          col.sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])
) {

  ## (0) Handle arguments and deprecated arguments: ----------

  # ## (0) Get probabilities from global numeric parameters (num):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec

  ## Currently fixed parameters:
  gap <- 0.025  # width of gap between 2 main subgroups (set via "by" argument)
  show.prob.comp <- TRUE  # show help_line for complements of prob (e.g, prev, sens, spec)?

  ## (1) Compute or use current popu: ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur.freq and popu based on current parameters (N and probabilities):
    cur.freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = TRUE)  # compute cur.freq (with round = TRUE)

    ## Assign (only needed) elements of cur.freq:
    n.hi  <- cur.freq$hi
    n.mi  <- cur.freq$mi
    n.fa  <- cur.freq$fa
    n.cr  <- cur.freq$cr

    ## (c) Compute cur.popu from computed frequencies:
    cur.popu <- comp_popu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr)  # compute cur.popu (from 4 essential frequencies)

    # warning("Generated new population (cur.popu) to draw mosaic plot...")


  } else {  # (B) NO valid set of probabilities was provided:

    ## Use the current popu:
    cur.popu <- popu

    # warning("Using existing population (popu) to draw mosaic plot...")

  } # if (is_valid_prob_set...)

  ## (2) Text labels: ----------

  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, "Mosaic plot") # , "(N = ", N, ")")

  cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  # cur.dec.lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
  cur.par.lbl <- cur.cond.lbl

  ## (3) Accuracy: ----------

  if (show.accu) {
    cur.accu <- comp_accu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr, w = w.acc)  # compute accuracy info
    cur.accu.lbl <- make_accu_lbl(acc = cur.accu$acc, w = w.acc, wacc = cur.accu$wacc, mcc = cur.accu$mcc)  # use utility function

    # mtext(cur.accu.lbl, side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = .85)
    cur.par.lbl <- paste0(cur.par.lbl, "\n", cur.accu.lbl, "\n")  # add accuracy lbl to existing cur.par.lbl
  }

  ## (4) Define plot and margin areas: ----------

  ## Margin areas:
  par(oma = c(3, 0, 0, 0) + 0.1)  # outer margins: bottom has 3 lines of space
  par(mar = c(4, 2, 4, 2) + 0.1)  # margin: default: c(5.1, 4.1, 4.1, 2.1)

  ## Plot dimensions:
  xlim = c(0, (1 + gap))
  ylim = c(0, 1)

  ## Plot area setup:
  plot(x = 1,
       xlim = xlim, ylim = ylim,
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "n",
       fg = grey(.50, alpha = .99))

  ## Mark plot and margin area:
  col.plot <- "firebrick3"
  box("plot", col = col.plot)
  mar.col <- "forestgreen"
  box("figure", col = mar.col)

  ## Graphical parameters:
  col.prev <- prev.li  # prev help line
  col.sens <- sens.li  # sens help line
  col.spec <- spec.li  # spec help line
  col.bord <- grey(.11, .99)  # borders and labels

  ## Point appearance:
  pt.pch <- 21    # pch symbol of points
  pt.cex <- 1.4   # cex scaling of points
  pt.lwd <- 1.6   # lwd of point borders

  ## Text labels:
  col.lbl <- col.bord
  cex.lbl <- .90   # scaling factor for text labels
  cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  h.shift <- .05   # horizontal shifting of labels
  v.shift <- .05   # vertical shifting of labels

  # help line properties (main metrics):
  lty.help <- 1    # line type
  lwd.help <- 2.5  # line width

  ## (5) Mosaic plot: ----------

  if (by == "cd") {

    ## (a) by condition (vertical split 1st): ----------

    ## 0. Determine 2 key coordinates:
    x.prev.sens <- prev     # point 1
    y.prev.sens <- (1 - sens)
    x.prev.spec <- (prev + gap) # point 2
    y.prev.spec <- spec

    ## 1. Draw 4 rectangles:
    rect(0, y.prev.sens, prev, 1, col = pal["hi"], border = col.bord) # hi
    rect(0, 0, prev, y.prev.sens, col = pal["mi"], border = col.bord) # mi
    rect((prev + gap), y.prev.spec, (1 + gap), 1, col = pal["fa"], border = col.bord) # fa
    rect((prev + gap), 0, (1 + gap), y.prev.spec, col = pal["cr"], border = col.bord) # cr

    ## 2. Mark 2 key points:
    points(x.prev.sens, y.prev.sens, pch = pt.pch, cex = pt.cex,
           lwd = pt.lwd, col = col.bord, bg = col.sens)  # intersect prev x sens
    points(x.prev.spec, y.prev.spec, pch = pt.pch, cex = pt.cex,
           lwd = pt.lwd, col = col.bord, bg = col.spec)  # intersect prev x spec


    ## 3. Label the 2 categories on all 4 sides: ----------

    ## top:
    text(x = (prev/2), y = 1, labels = "cond.true", pos = 3, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # top left
    text(x = (prev + gap + (1 - prev)/2), y = 1, labels = "cond.false", pos = 3, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # top right
    ## bottom:
    text(x = (prev/2), y = 0, labels = "cond.true", pos = 1, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # bottom left
    text(x = (prev + gap + (1 - prev)/2), y = 0, labels = "cond.false", pos = 1, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # bottom right
    ## left:
    text(x = 0, y = (1 - (sens/2) + v.shift), labels = "dec.pos", srt = 90, pos = 2, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # left top
    text(x = 0, y = ((1 - sens)/2 + v.shift), labels = "dec.neg", srt = 90, pos = 2, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # left bottom
    ## right:
    text(x = (1 + gap), y = (spec + (1 - spec)/2 + v.shift), labels = "dec.pos", srt = -90, pos = 4, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # right top
    text(x = (1 + gap), y = (spec/2 + v.shift), labels = "dec.neg", srt = -90, pos = 4, xpd = TRUE, col = col.lbl, cex = cex.lbl.sm)  # right bottom


    ## 4. Label the 4 freq in 4 rectangles: ----------

    y.hi <- ((1 - sens) + sens/2)
    y.fa <- (spec + (1 - spec)/2)

    if (show.freq) {

      col.lbl <- my.whitish

      hi.lbl <- paste0("hi = ", n.hi, "")  # label for hi
      mi.lbl <- paste0("mi = ", n.mi, "")  # label for mi
      fa.lbl <- paste0("fa = ", n.fa, "")  # label for fa
      cr.lbl <- paste0("cr = ", n.cr, "")  # label for cr

      text(x = prev/2, y = y.hi, labels = hi.lbl, srt = 0, pos = NULL, col = col.lbl, cex = cex.lbl.sm)  # hi
      text(x = prev/2, y = ((1 - sens)/2), labels = mi.lbl, srt = 0, pos = NULL, col = col.lbl, cex = cex.lbl.sm)  # mi
      text(x = (prev + gap + (1 - prev)/2), y = y.fa, labels = fa.lbl, srt = 0, pos = NULL, col = col.lbl, cex = cex.lbl.sm)  # fa
      text(x = (prev + gap + (1 - prev)/2), y = (spec/2), labels = cr.lbl, srt = 0, pos = NULL, col = col.lbl, cex = cex.lbl.sm)  # cr

    }

    ## 1. Show (sens) in plot: ----------

    if (show.prob || show.prob.comp) { # set arbitrary values of help lines:

      x.sens <- (0 + (prev * 1/5))  # x-value of vertical line (on left)
      x.spec <- (prev + gap + ((1 - prev) * 4/5))  # x-value of vertical lines: on right

      ## Define y.prev:
      if ((sens > spec)) {
        y.prev <- spec * 5/6  # high line
      }
      else if ((spec > sens)) {
        y.prev <- spec * 5/6  # high line
      }
      else {
        y.prev <- 1/2  # middle line
      }

    }

    if (show.prob) {

      ## sens parameters:
      sens.lbl <- "sens" # paste0("sens = ", as_pc(sens, n.digits = 1), "%")  # label for sens
      lty.sens <- 1  # sens line type
      y.lbl.sens <- ((1 - sens) + (sens * 1/4))  # y-value of label

      ## Draw sens (as horizontal line):
      # abline(h = y.sens, lty = lty.sens, lwd = 1, col = col.sens)

      help_line(x.sens, y.prev.sens, x.sens, 1,
                col = col.sens, col.bord = col.bord,
                lty = lty.sens, lwd = lwd.help,
                pt.pch = pt.pch, pt.cex = pt.cex, pt.lwd = pt.lwd,
                lbl.txt = sens.lbl, lbl.x = x.sens, lbl.y = y.lbl.sens, lbl.pos = 4, lbl.cex = cex.lbl.sm)

    }

    if (show.prob.comp) {

      ## Show complement (1 - sens) as vertical help_line:
      sens.lbl <- "(1 - sens)" # paste0("(1 - sens) = ", as_pc((1 - sens), n.digits = 1), "%")  # label for (1 - sens)
      lty.sens <- 3  # sens line type

      y.lbl.sens <- ((1 - sens) * 1/4)  # y-value of label

      help_line(x.sens, 0, x.sens, y.prev.sens,
                col = col.sens, col.bord = col.bord,
                lty = lty.sens, lwd = 1.5,
                pt.pch = pt.pch, pt.cex = pt.cex, pt.lwd = pt.lwd,
                lbl.txt = sens.lbl, lbl.x = x.sens, lbl.y = y.lbl.sens, lbl.pos = 4, lbl.cex = cex.lbl.sm)
    }

    ## 2. Show (spec) in plot: ----------

    if (show.prob) {

      ## sens parameters:
      spec.lbl <- "spec" # paste0("spec = ", as_pc(spec, n.digits = 1), "%")  # label for spec
      lty.spec <- 1                    # spec line type

      ## Show spec as vertical help_line:
      y.lbl.spec <- (spec * 1/4)

      help_line(x.spec, 0, x.spec, y.prev.spec,
                col = col.spec, col.bord = col.bord,
                lty = lty.spec, lwd = lwd.help,
                pt.pch = pt.pch, pt.cex = pt.cex, pt.lwd = pt.lwd,
                lbl.txt = spec.lbl, lbl.x = x.spec, lbl.y = y.lbl.spec, lbl.pos = 2, lbl.cex = cex.lbl.sm)
    }

    if (show.prob.comp) {
      ## Show complement (1 - spec) as vertical help_line:
      spec.lbl <- "(1 - spec)" # paste0("(1 - spec) = ", as_pc((1 - spec), n.digits = 1), "%")  # label for (1 - spec)
      lty.spec <- 3
      y.lbl.spec.comp <- (spec + (1 - spec) * 1/4)

      help_line(x.spec, spec, x.spec, 1,
                col = col.spec, col.bord = col.bord,
                lty = lty.spec, lwd = 1.5,
                pt.pch = pt.pch, pt.cex = pt.cex, pt.lwd = pt.lwd,
                lbl.txt = spec.lbl, lbl.x = x.spec, lbl.y = y.lbl.spec.comp, lbl.pos = 2, lbl.cex = cex.lbl.sm)
    }

    ## 3. Show (prev) in plot: ----------

    if (show.prob) {

      ## prev parameters:
      prev.lbl <- "prev" # paste0("prev = ", as_pc(prev, n.digits = 1), "%")  # label for prev
      lty.prev <- 1  # prev line type



      ## Show prev as horizontal help_line:
      help_line(0, y.prev, prev, y.prev,
                col = col.prev, col.bord = col.bord,
                lty = lty.prev, lwd = lwd.help,
                pt.pch = pt.pch, pt.cex = pt.cex, pt.lwd = pt.lwd,
                lbl.txt = prev.lbl, lbl.x = (prev/2), lbl.y = (y.prev), lbl.pos = 3, lbl.cex = cex.lbl.sm)
    }

    ## (b) prev complement (1 - prev):

    if (show.prob.comp) {

      ## Show complement (1 - prev) as horizontal help_line:
      prev.lbl <- "(1 - prev)" # paste0("(1 - prev) = ", as_pc((1 - prev), n.digits = 1), "%")  # prev label
      lty.prev <- 3

      help_line((prev + gap), y.prev, (1 + gap), y.prev,
                col = col.prev, col.bord = col.bord,
                lty = lty.prev, lwd = 1.5,
                pt.pch = pt.pch, pt.cex = pt.cex, pt.lwd = pt.lwd,
                lbl.txt = prev.lbl, lbl.x = (prev + gap + (1 - prev)/2), lbl.y = (y.prev), lbl.pos = 3, lbl.cex = cex.lbl.sm)
    }

    ## WAS:
    # vcd::mosaic(Decision ~ Truth, data = cur.popu,
    #             shade = TRUE, colorize = TRUE,
    #             split_vertical = TRUE,
    #             gp = grid::gpar(fill = matrix(data = col.sdt, nrow = 2, ncol = 2, byrow = TRUE)),
    #             main_gp = grid::gpar(fontsize = 12, fontface = 1, adj = 0),
    #             sub_gp = grid::gpar(fontsize = 10, fontface = 1, adj = 1),
    #             main = paste0(cur.title.lbl), #, "\n", cur.par.lbl),
    #             sub = paste0(cur.par.lbl)  # print label
    #             )

  }

  else if (by == "dc") {

    ## +++ here now +++ ##

    ## (b) by decision (horizontal split 1st):
    vcd::mosaic(Truth ~ Decision, data = cur.popu,
                shade = TRUE, colorize = TRUE,
                split_vertical = FALSE,
                gp = grid::gpar(fill = matrix(data = col.sdt, nrow = 2, ncol = 2, byrow = FALSE)),
                main_gp = grid::gpar(fontsize = 12, fontface = 1),
                sub_gp = grid::gpar(fontsize = 10, fontface = 1),
                main = paste0(cur.title.lbl), #, "\n", cur.par.lbl),
                sub = paste0(cur.par.lbl)
    )
  } # if (by == ...)

  ## Title and margin text:
  title(cur.title.lbl, adj = 0.5, line = 1.5, font.main = 1) # (centered, raised, normal font)

}

## Check:
{
  # plot_mosaic_2()
  # plot_mosaic_2(title.lbl = "")
  # plot_mosaic_2(by = "dc")
  # plot_mosaic_2(title.lbl = "Just testing", col.sdt = "goldenrod")
}


## -----------------------------------------------
## (+) ToDo:

## - add labels for (prev, sens, spec), (ppod, PPV, NPV), bacc.
## - add Decisions panel.
## - consider adding 3rd perspective: (c) by correspondence (accu)
## - make mosaic plot dependent on basic parameters
##   (i.e., compute comp_popu() from probabilities and N,
##    rather than providing it as input)?
## - add a simpler version that only shows cond.true vs. cond.false
## - adjust parameters (zero size and gap width)
## - add labels (frequencies) to plot?

## -----------------------------------------------
## eof.
