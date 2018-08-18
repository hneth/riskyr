## plot_bar.R | riskyr
## 2018 08 18
## -----------------------------------------------
## Plot bar (a family of) charts that express freq as area
## (size and proportion)
## from 3 essential probabilities (prev, sens, spec)
## or current population data.frame popu.

## (Using only base graphics and custom helper functions
##  defined in plot_util.R.)
## -----------------------------------------------

## Helper functions (now moved to plot_util.R): -----------
{
  # ## Helper function: Plot a box and its text labels ----------
  # plot_vbox <- function(box.x,  box.y,    # coordinates x (center) and y (bottom)
  #                      box.lx, box.ly,   # lengths of box (width and height)
  #                      ## Text labels:
  #                      type = NA,        # type of box (printed as title below box)
  #                      show.freq = TRUE, # option to show/hide frequency labels
  #                      fname = NA,        # box name (corresponding to a color in pal, as character)
  #                      freq,             # frequency (as number).  ToDo: Derive freq from type and/OR name!
  #                      ## Color options:
  #                      col.fill = grey(.95, .75),  # default color (but derived from name below)
  #                      col.brd = pal["brd"],
  #                      col.txt = pal["txt"],
  #                      ...               # other graphical parameters: lwd, cex, ...
  #                      ) {
  #
  #
  #   ## (0) Parameters (currently fixed):
  #
  #   ## Box parameters:
  #   # box.lwd <- 1  # line width of border around rect (default = 1)
  #
  #   if (name %in% names(pal)) { # if name corresponds to a color name in pal
  #     col.fill <- pal[name]     # use this color to fill box
  #   }
  #
  #   ## Text parameters:
  #   # col.lbl <- pal["txt"]  # defined in pal
  #   # cex.lbl <- .90   # scaling factor for text labels
  #   # cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  #   # h.shift <- .05   # horizontal shifting of labels
  #   # v.shift <- .05   # vertical shifting of labels
  #
  #   ## (1) Plot rect:
  #   rect(xleft  = (box.x - box.lx/2), ybottom = box.y,
  #        xright = (box.x + box.lx/2), ytop    = (box.y + box.ly),
  #        col = col.fill,
  #        border = col.brd,
  #        # lwd = box.lwd,
  #        ...)
  #
  #   ## (2) Print type as box title (below box, optional):
  #   if (!is.na(type)) {  # type is specified:
  #
  #   text(x = box.x, y = box.y,
  #        labels = type,
  #        pos = 1,  # 1...below, 3...above
  #        xpd = TRUE,
  #        col = col.txt,
  #        # cex = cex.lbl.sm,
  #        ...)
  #
  #   }
  #
  #   ## (3) Plot box label (centered in box, optional):
  #   if (show.freq, ...) {
  #
  #     # y-coordinate of label:
  #     mid.y <- box.y + box.ly/2  # y-value of mid point
  #
  #     # Compose box label:
  #     if (!is.na(name)) {  # name is specified:
  #
  #       # ToDo: Derive freq from type and/or name.
  #
  #       box.lbl <- paste0(name, " = ", freq)
  #
  #     } else { # no name specified:
  #
  #       box.lbl <- paste0(freq)
  #
  #     }
  #
  #     # Plot text:
  #     text(x = box.x, y = mid.y,
  #          labels = box.lbl,
  #          # pos = 3,
  #          xpd = TRUE,
  #          col = col.txt,
  #          # cex = cex.lbl.sm,
  #          ...)
  #   }
  #
  # }
  #
  # ## Check:
  # {
  #   # plot(c(0, 100), c(0, 100)) # 2 points
  #   #
  #   # plot_vbox(10, 80, 20, 20, freq = 111) # no name => default fill color
  #   #
  #   # plot_vbox(50, 80, 20, 20, fname = "N", freq = 222) # no type label
  #   #
  #   # plot_vbox(10, 50, 30, 20, type = "type as box title", fname = "hi", freq = 333)
  #   #
  #   # plot_vbox(40, 50, 20, 20, fname = "mi", freq = 123, lwd = 3, cex = .7)
  #
  # }
  #
  #
  #
  # ## ToDo:
  # ## - Distinguish 2 separate functions:
  # ##   1. generic plot_vbox vs.
  # ##   2. plot_freq (that automatically determines current freq value and fill color).
  #
  #
  # ## Helper function: Add text with background box to a plot ------
  # ## from https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r
  # {
  # ## Add text with background box to a plot
  #
  # # \code{boxtext} places a text given in the vector \code{labels}
  # # onto a plot in the base graphics system and places a coloured box behind
  # # it to make it stand out from the background.
  #
  # # @param x numeric vector of x-coordinates where the text labels should be
  # # written. If the length of \code{x} and \code{y} differs, the shorter one
  # # is recycled.
  # # @param y numeric vector of y-coordinates where the text labels should be
  # # written.
  # # @param labels a character vector specifying the text to be written.
  # # @param col.text the colour of the text
  # # @param col.bg color(s) to fill or shade the rectangle(s) with. The default
  # # \code{NA} means do not fill, i.e., draw transparent rectangles.
  # # @param border.bg color(s) for rectangle border(s). The default \code{NA}
  # # omits borders.
  # # @param adj one or two values in [0, 1] which specify the x (and optionally
  # # y) adjustment of the labels.
  # # @param pos a position specifier for the text. If specified this overrides
  # # any adj value given. Values of 1, 2, 3 and 4, respectively indicate
  # # positions below, to the left of, above and to the right of the specified
  # # coordinates.
  # # @param offset when \code{pos} is specified, this value gives the offset of
  # # the label from the specified coordinate in fractions of a character width.
  # # @param padding factor used for the padding of the box around
  # # the text. Padding is specified in fractions of a character width. If a
  # # vector of length two is specified then different factors are used for the
  # # padding in x- and y-direction.
  # # @param cex numeric character expansion factor; multiplied by
  # # code{par("cex")} yields the final character size.
  # # @param font the font to be used
  # #
  # # @return Returns the coordinates of the background rectangle(s). If
  # # multiple labels are placed in a vactor then the coordinates are returned
  # # as a matrix with columns corresponding to xleft, xright, ybottom, ytop.
  # # If just one label is placed, the coordinates are returned as a vector.
  # #
  # # @author Ian Kopacka
  # #
  # # @examples
  # # ## Create noisy background
  # # plot(x = runif(1000), y = runif(1000), type = "p", pch = 16,
  # # col = "#40404060")
  # # boxtext(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480",
  # #    pos = 4, font = 2, cex = 1.3, padding = 1)
  # #
  #
  # boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
  #                     border.bg = NA, adj = NULL, pos = NULL, offset = 0.5,
  #                     padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){
  #
  #   ## The Character expansion factro to be used:
  #   theCex <- graphics::par('cex')*cex
  #
  #   ## Is y provided:
  #   if (missing(y)) y <- x
  #
  #   ## Recycle coords if necessary:
  #   if (length(x) != length(y)){
  #     lx <- length(x)
  #     ly <- length(y)
  #     if (lx > ly){
  #       y <- rep(y, ceiling(lx/ly))[1:lx]
  #     } else {
  #       x <- rep(x, ceiling(ly/lx))[1:ly]
  #     }
  #   }
  #
  #   ## Width and height of text
  #   textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  #   textWidth <- graphics::strwidth(labels, cex = theCex, font = font)
  #
  #   ## Width of one character:
  #   charWidth <- graphics::strwidth("e", cex = theCex, font = font)
  #
  #   ## Is 'adj' of length 1 or 2?
  #   if (!is.null(adj)){
  #     if (length(adj == 1)){
  #       adj <- c(adj[1], 0.5)
  #     }
  #   } else {
  #     adj <- c(0.5, 0.5)
  #   }
  #
  #   ## Is 'pos' specified?
  #   if (!is.null(pos)){
  #     if (pos == 1){
  #       adj <- c(0.5, 1)
  #       offsetVec <- c(0, -offset*charWidth)
  #     } else if (pos == 2){
  #       adj <- c(1, 0.5)
  #       offsetVec <- c(-offset*charWidth, 0)
  #     } else if (pos == 3){
  #       adj <- c(0.5, 0)
  #       offsetVec <- c(0, offset*charWidth)
  #     } else if (pos == 4){
  #       adj <- c(0, 0.5)
  #       offsetVec <- c(offset*charWidth, 0)
  #     } else {
  #       stop('Invalid argument pos')
  #     }
  #   } else {
  #     offsetVec <- c(0, 0)
  #   }
  #
  #   ## Padding for boxes:
  #   if (length(padding) == 1){
  #     padding <- c(padding[1], padding[1])
  #   }
  #
  #   ## Midpoints for text:
  #   xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  #   yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]
  #
  #   ## Draw rectangles:
  #   rectWidth <- textWidth + 2*padding[1]*charWidth
  #   rectHeight <- textHeight + 2*padding[2]*charWidth
  #   graphics::rect(xleft = xMid - rectWidth/2,
  #                  ybottom = yMid - rectHeight/2,
  #                  xright = xMid + rectWidth/2,
  #                  ytop = yMid + rectHeight/2,
  #                  col = col.bg, border = border.bg)
  #
  #   ## Place the text:
  #   graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
  #                  adj = c(0.5, 0.5))
  #
  #   ## Return value:
  #   if (length(xMid) == 1){
  #     invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
  #                 yMid + rectHeight/2))
  #   } else {
  #     invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
  #                     yMid + rectHeight/2))
  #   }
  # }
  #
  # ## Check:
  # {
  #   # ## Create noisy background:
  #   # plot(x = runif(1000), y = runif(1000), type = "p", pch = 16, col = "#40404060")
  #   #
  #   # ## Vector of labels, using argument 'pos' to position right of coordinates:
  #   # boxtext(x = c(0.1, 0.8), y = c(0.1, 0.7), labels = c("some Text", "something else"),
  #   #         col.bg = "gold", pos = 4, padding = 0.2)
  #   #
  #   # ## Tweak cex, font and adj:
  #   # boxtext(x = 0.2, y = 0.4, labels = "some big and bold text",
  #   #         col.bg = "skyblue", adj = c(0, 0.6), font = 2, cex = 1.8)
  # }
  #
  # }
  #
  # ## Helper function: Plot an (arrow) line between 2 points with an optional text label: ------
  # plot_line <- function(x0, y0, x1, y1,     # coordinates of p1 and p2
  #                       col = "grey",       # colors (for line, point fill, and labels)
  #                       col.bord = "black", # color of point border
  #                       lty = 1, lwd = 1,                     # line options
  #                       pt.pch = 21, pt.cex = 1, pt.lwd = 1,  # point options
  #                       ## Optional text label:
  #                       lbl.txt = NA,         # string for text label
  #                       lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
  #                       lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
  #                       lbl.cex = 1,          # size of text label
  #                       ...                   # pos (1 = bottom, 3 = top), offset, etc.
  # ) {
  #
  #   ## (1) Draw an arrow or line between 2 points: ------
  #
  #   arrow <- FALSE
  #
  #   if (arrow) {
  #
  #     ## Draw an arrow:
  #     arrows(x0, y0, x1, y1,
  #            length = .06, angle = 33, code = 3,    # V shape (small)
  #            # length = .08, angle = 90, code = 3,  # T shape
  #            lty = lty, lwd = lwd, col = col)       # arrow
  #
  #   } else {
  #
  #     ## Normal line with 2 points at line ends:
  #     arrows(x0, y0, x1, y1,
  #            length = 0, angle = 0, code = 3,  # no arrows
  #            lty = lty, lwd = lwd, col = col)
  #     points(x0, y0, pch = pt.pch, cex = pt.cex,
  #            lwd = pt.lwd, col = col.bord, bg = col)  # point 1
  #     points(x1, y1, pch = pt.pch, cex = pt.cex,
  #            lwd = pt.lwd, col = col.bord, bg = col)  # point 2
  #
  #   }
  #
  #   ## (2) Optional text label: ------
  #
  #   if (!is.na(lbl.x)) { # if lbl.x exists:
  #
  #     ## Text label:
  #     text(lbl.x, lbl.y,
  #          labels = lbl.txt,
  #          col = col, cex = lbl.cex,
  #          # pos, offeset, ...
  #          ...)
  #   }
  #
  # }
  #
  # ## Check:
  # {
  #   # plot(0:1, 0:1) # 2 points
  #   #
  #   # plot_line(0, .1, 1, .1)  # basic line (without label)
  #   #
  #   # plot_line(0, .2, 1, .2, lbl.txt = "Label 1")  # basic with text label (on line)
  #   #
  #   # plot_line(0, 0, 1, 1, lbl.txt = "Label 2", pos = 3, offset = 2)  # basic with raised text label
  #   #
  #   # plot_line(0, 1, 1, 0,  # coordinates
  #   #           col = "firebrick1", col.bord = "black",   # colors (for line, points, and labels)
  #   #           lty = 1, lwd = 2,                         # line
  #   #           pt.pch = 21, pt.cex = 2, pt.lwd = 2,      # points
  #   #           # Text label (with options):
  #   #           lbl.x = 1/3, lbl.y = 2/3,
  #   #           lbl.txt = "Some label\nthat takes\nmultiple (3) lines",
  #   #           pos = 3, offset = 2, lbl.cex = .8)
  #
  # }
  #
  # ## Helper function: Plot multiple (nArr) arrows along a line: ------
  # plot_arrows <- function(x0, y0, x1, y1,       # coordinates
  #                          nArr = 2,             # number of arrows to draw
  #                          ## Optional label:
  #                          lbl.txt = NA,         # string for text label
  #                          lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
  #                          lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
  #                          pos = 3, offset = 1,  # pos (1 = bottom, 3 = top), offset, etc.
  #                          ...                   # other graphical parameters
  #                          )
  # {
  #   ## (0) Draw line from p1 to p2: ----
  #
  #   # lines(c(x0, x1), c(y0, y1), ...)
  #
  #
  #   ## (1) Draw nArr arrows: ----
  #
  #   # Split line into nArr + 1 segments:
  #   Ax = seq(x0, x1, length = nArr + 1)
  #   Ay = seq(y0, y1, length = nArr + 1)
  #
  #   # Loop to draw all arrows:
  #   for (i in 1:nArr)
  #   {
  #     arrows(Ax[i], Ay[i], Ax[i + 1], Ay[i + 1],
  #            length = .20, angle = 33, code = 2, # arrow type: V or T?
  #            ...)
  #   }
  #
  #   ## (3) Optional text label: ------
  #
  #   if (!is.na(lbl.x)) { # if lbl.x exists:
  #
  #     # Parameters:
  #     # lbl.cex = 1          # size of text label
  #
  #     ## Text label:
  #     text(lbl.x, lbl.y,
  #          labels = lbl.txt,
  #          # col = col,
  #          # cex = lbl.cex,
  #          pos = pos, offset = offset,
  #          ...)
  #   }
  #
  # }
  #
  # ## Check:
  # {
  #   # plot(0:1, 0:1) # 2 points
  #   #
  #   # plot_arrows(0, 0, 1, 0, col = "red3")  # 2 arrows, no text
  #   #
  #   # plot_arrows(0, .2, 1, .2, col = "green3", lbl.txt = "Label 1", pos = 3)
  #   #
  #   # plot_arrows(0, .3, 1, .5, col = "blue3", nArr = 3, lbl.txt = "Label 2", pos = 3, lwd = 2)
  #   #
  #   # plot_arrows(0, .4, 1, .9, col = "black", lbl.txt = "Label 3\nis a longer\nand wider label\nin smaller font", pos = 3, offset = 2, cex = .8)
  # }

}

## Main function: ---------

#' Plot bar charts of population frequencies.
#'
#' \code{plot_bar} draws bar charts that
#' represent the proportions of frequencies in the current
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
#' (or the dimension by which the population is split into 2 subsets)
#' with the following options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition;
#'   \item \code{"dc"} ... by decision;
#'   \item \code{"ac"} ... by accuracy;
#'   \item \code{"all"} ... shows all subdivisions.
#'   }
#'
#' @param dir Number of directions of bars. Options:
#' \enumerate{
#'   \item \code{1} ... uni-directional bars;
#'   \item \code{2} ... bi-directional (positive vs. negative) bars;
#'   }
#'
#' @param show.freq Boolean option for showing frequencies
#' (i.e., of \code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, and
#' \code{\link{cr}}) in the plot.
#' Default: \code{show.freq = TRUE}.
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
#' @param col.pal Color palette.
#' Default: \code{col.pal = pal} (see \code{\link{pal}} and \code{\link{init_pal}}).
#'
#' @examples
#' # Basics:
#' plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test 1")
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60,
#'          title.lbl = "Test 2")  # by "all" (default)
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd",
#'          title.lbl = "Test 3a")  # by condition
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", dir = 2,
#'          title.lbl = "Test 3b")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc",
#'          title.lbl = "Test 4a")  # by decision
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", dir = 2,
#'          title.lbl = "Test 4b")  # bi-directional
#'
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac",
#'          title.lbl = "Test 5a")  # by accuracy
#' plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", dir = 2,
#'          title.lbl = "Test 5b")  # bi-directional
#'
#' # Note:
#' plot_bar(N = 3, prev = .33, sens = .75, spec = .66,
#'          title.lbl = "Test rounding effects",
#'          show.freq = TRUE)  # => Rounding of freq, but not of proportions shown in plot.
#'
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
#' @importFrom graphics barplot
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

plot_bar <- function(prev = num$prev,             # probabilities
                     sens = num$sens, mirt = NA,
                     spec = num$spec, fart = NA,
                     N = num$N,  # not needed in Mosaic plot (but used in comp_freq below)
                     ## Options:
                     by = "all",  # "cd"...condition, "dc"...decision; "all".
                     dir = 1,     # directions: 1 vs. 2
                     show.freq = TRUE,   # show labels of frequencies in plot
                     show.prob = FALSE,  # show help_line (for metrics, e.g., prev, sens, spec)?
                     show.accu = TRUE,   # compute and show accuracy metrics
                     w.acc = .50,        # weight w for wacc (from 0 to 1)
                     ## Text and color options: ##
                     title.lbl = txt$scen.lbl,
                     col.pal = pal,
                     ...  # other graphical parameters: lwd, cex, ...
) {

  ## (0) Handle arguments and deprecated arguments: ----------

  # ## (0) Get probabilities from global numeric parameters (num):
  # prev <- num$prev
  # sens <- num$sens
  # spec <- num$spec

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
  cur.title.lbl <- paste0(title.lbl, "Custom bar plot") # , "(N = ", N, ")")

  cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  # cur.dec.lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
  cur.par.lbl <- cur.cond.lbl

  type_label <- "freq type"  # to be derived below.

  ## (3) Accuracy: ----------

  if (show.accu) {
    cur.accu <- comp_accu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr, w = w.acc)  # compute accuracy info
    cur.accu.lbl <- make_accu_lbl(acc = cur.accu$acc, w = w.acc, wacc = cur.accu$wacc, mcc = cur.accu$mcc)  # use utility function

    # mtext(cur.accu.lbl, side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = .85)
    cur.par.lbl <- paste0(cur.par.lbl, "\n", cur.accu.lbl, "\n")  # add accuracy lbl to existing cur.par.lbl
  }

  ## (4) Define plot and margin areas: ----------

  ## Margin areas:
  par(oma = c(3, 2, 1, 1) + 0.1)  # outer margins: bottom has 3 lines of space
  par(mar = c(4, 2, 4, 2) + 0.1)  # margin: default: c(5.1, 4.1, 4.1, 2.1)

  ## Graphical parameters: ----

  # Offset from base line:
  x.base <- 0  # offset x
  y.base <- 0  # offset y

  ## Color info (NOW defined in init_pal):
  # col.prev <- prev.li  # prev help line
  # col.sens <- sens.li  # sens help line
  # col.spec <- spec.li  # spec help line
  # col.bord <- grey(.20, .99) # NA # grey(.11, .99)  # borders and labels (NA removes borders)

  ## Currently fixed parameters:
  # gap <- 2.0/100  # width of gap between 2 main subgroups (direction set via "by" argument)
  # show.prob.comp <- TRUE  # show help_line for complements of prob (e.g, prev, sens, spec)?

  ## Box appearance:
  # box.lwd <- 1  # line width of border around rect box (default = 1)

  ## Point appearance:
  # pt.pch <- 21    # pch symbol of points
  # pt.cex <- 1.4   # cex scaling of points
  # pt.lwd <- 1.6   # lwd of point borders

  ## Text label appearance:
  # col.lbl <- pal["txt"]  # defined in pal
  # cex.lbl <- .90   # scaling factor for text labels
  # cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  # h.shift <- .05   # horizontal shifting of labels
  # v.shift <- .05   # vertical shifting of labels

  ## help line properties (main metrics):
  # lty.help <- 1    # line type
  # lwd.help <- 2.5  # line width


  ## Define plot area: ----------

  ## Plot dimensions:
  xlim = c(0, 1)

  if (dir == 1) {
    y.min <- 0
  } else if (dir == 2) {
    y.min <- -N
  }
  ylim = c(y.min, N)

  ## Plot area setup:
  plot(x = 1,
       xlim = xlim, ylim = ylim,
       type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       bty = "n",
       fg = grey(.50, alpha = .99)
  )

  ## Mark plot and margin area:
  # col.plot <- "grey80"
  # box("plot", col = col.plot)
  # mar.col <- "grey60"
  # box("figure", col = mar.col)

  ## Axes:
  # axis(side = 1, las = 1) # x-axis, horizontal labels
  axis(side = 2, las = 2) # y-axis, horizontal labels

  ## Grid:
  grid(nx = NA, ny = NULL,  # y-axes only (at tick marks)
       col = grey(.75, .99), lty = 1,
       lwd = par("lwd"), equilogs = TRUE)


  ## Custom bar plot: ----------

  if (by == "all") {

    ## (1): 5 vertical bars: ----------

    # Number and basic width of columns:
    nr.col <- 5         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate
    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", freq = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # # Column name: freq type
    # type_label <- paste0(comp_freq_type("N"))  # determine freq type
    # text(x = n.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)

    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"], # col = comp_freq_col("N"),
                     ...)

    # +++ here now +++


    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      fa.ly <- -1 * fa.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      cr.y <- y.base
      fa.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", freq = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", freq = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", freq = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", freq = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("hi"))  # determine freq type
    # text(x = hi.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Condition column: ----

    # x-coordinates:
    col.nr <- 2
    cond.true.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    cond.false.x <- cond.true.x

    # heights:
    cond.true.ly  <- (n.ly * prev)        # re-computes cond.true (without rounding)
    cond.false.ly <- (n.ly * (1 - prev))  # re-computes cond.false (without rounding)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.ly <- -1 * cond.false.ly
    }

    # y-coordinates (given heights):
    cond.true.y <- y.base
    cond.false.y <- cond.true.y + cond.true.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "cond.true", freq = (n.hi + n.mi),
              box.x  = cond.true.x,
              box.y  = cond.true.y,
              box.lx = b.lx,
              box.ly = cond.true.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cond.false", freq = (n.fa + n.cr),
              box.x  = cond.false.x,
              box.y  = cond.false.y,
              box.lx = b.lx,
              box.ly = cond.false.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("cond.true"))  # determine freq type
    # text(x = cond.true.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)
    plot_ftype_label("cond.true", cond.true.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond.true"),
                     ...)

    ## (d) Decision column: ----

    # x-coordinates:
    col.nr <- 4
    dec.pos.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.neg.x <- dec.pos.x

    # heights:
    dec.pos.ly <- abs(hi.ly) + abs(fa.ly)
    dec.neg.ly <- abs(mi.ly) + abs(cr.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.ly <- -1 * dec.neg.ly
    }

    # y-coordinates (given heights):
    dec.pos.y <- y.base
    dec.neg.y <- dec.pos.y + dec.pos.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.pos", freq = (n.hi + n.fa),
              box.x  = dec.pos.x,
              box.y  = dec.pos.y,
              box.lx = b.lx,
              box.ly = dec.pos.ly,
              show.freq, ...)
    plot_vbox(type = NA, fname = "dec.neg", freq = (n.mi + n.cr),
              box.x  = dec.neg.x,
              box.y  = dec.neg.y,
              box.lx = b.lx,
              box.ly = dec.neg.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("dec.pos"))  # determine freq type
    # text(x = dec.pos.x, y = y.min,
    #     labels = type_label, pos = 1,
    #     xpd = TRUE, col = pal["txt"],
    #     ...)
    plot_ftype_label("dec.pos", dec.pos.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.pos"),
                     ...)

    ## (e) Accuracy column: ----

    # x-coordinates:
    col.nr <- 5
    dec.cor.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.err.x <- dec.cor.x

    # heights:
    dec.cor.ly <- abs(hi.ly) + abs(cr.ly)
    dec.err.ly <- abs(mi.ly) + abs(fa.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.ly <- -1 * dec.err.ly
    }

    # y-coordinates (given heights):
    dec.cor.y <- y.base
    dec.err.y <- dec.cor.y + dec.cor.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.cor", freq = (n.hi + n.cr),
              box.x  = dec.cor.x,
              box.y  = dec.cor.y,
              box.lx = b.lx,
              box.ly = dec.cor.ly,
              show.freq, ...)
    plot_vbox(type = NA, fname = "dec.err", freq = (n.mi + n.fa),
              box.x  = dec.err.x,
              box.y  = dec.err.y,
              box.lx = b.lx,
              box.ly = dec.err.ly,
              show.freq, ...)

    ## Column name: freq type
    # type_label <- paste0(comp_freq_type("dec.cor"))  # determine freq type
    # text(x = dec.cor.x, y = y.min,
    #      labels = type_label, pos = 1,
    #      xpd = TRUE, col = pal["txt"],
    #      ...)
    plot_ftype_label("dec.cor", dec.cor.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.cor"),
                     ...)

  } # if (by == "all")

  else if (by == "cd") {

    ## (2): 3 vertical bars (condition in middle): ----------

    # Number and basic width of columns:
    nr.col <- 3         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate

    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", freq = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # Column name: freq type
    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("N"),
                     ...)

    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      fa.ly <- -1 * fa.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    mi.y <- hi.y + hi.ly
    fa.y <- mi.y + mi.ly
    cr.y <- fa.y + fa.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      cr.y <- y.base
      fa.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", freq = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", freq = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", freq = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", freq = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    # Column name: freq type
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Condition column: ----

    # x-coordinates:
    col.nr <- 2
    cond.true.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    cond.false.x <- cond.true.x

    # heights:
    cond.true.ly  <- (n.ly * prev)        # re-computes cond.true (without rounding)
    cond.false.ly <- (n.ly * (1 - prev))  # re-computes cond.false (without rounding)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.ly <- -1 * cond.false.ly
    }

    # y-coordinates (given heights):
    cond.true.y <- y.base
    cond.false.y <- cond.true.y + cond.true.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      cond.false.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "cond.true", freq = (n.hi + n.mi),
              box.x  = cond.true.x,
              box.y  = cond.true.y,
              box.lx = b.lx,
              box.ly = cond.true.ly,
              show.freq, ...)
    plot_vbox(type = NA, fname = "cond.false", freq = (n.fa + n.cr),
              box.x  = cond.false.x,
              box.y  = cond.false.y,
              box.lx = b.lx,
              box.ly = cond.false.ly,
              show.freq, ...)

    # Column name: freq type
    plot_ftype_label("cond.true", cond.true.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("cond.true"),
                     ...)

  } # if (by == "cd")

  else if (by == "dc") {

    ## (3): 3 vertical bars (decision in middle): ----------

    # Number and basic width of columns:
    nr.col <- 3         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate

    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", freq = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("N"),
                     ...)


    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi.ly <- -1 * mi.ly
      cr.ly <- -1 * cr.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    fa.y <- hi.y + hi.ly
    cr.y <- fa.y + fa.ly
    mi.y <- cr.y + cr.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      cr.y <- y.base
      mi.y <- cr.y + cr.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", freq = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", freq = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", freq = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", freq = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Decision column: ----

    # x-coordinates:
    col.nr <- 2
    dec.pos.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.neg.x <- dec.pos.x

    # heights:
    dec.pos.ly <- abs(hi.ly) + abs(fa.ly)
    dec.neg.ly <- abs(mi.ly) + abs(cr.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.ly <- -1 * dec.neg.ly
    }

    # y-coordinates (given heights):
    dec.pos.y <- y.base
    dec.neg.y <- dec.pos.y + dec.pos.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.neg.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.pos", freq = (n.hi + n.fa),
              box.x  = dec.pos.x,
              box.y  = dec.pos.y,
              box.lx = b.lx,
              box.ly = dec.pos.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "dec.neg", freq = (n.mi + n.cr),
              box.x  = dec.neg.x,
              box.y  = dec.neg.y,
              box.lx = b.lx,
              box.ly = dec.neg.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("dec.pos", dec.pos.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.pos"),
                     ...)

  } # if (by == "dc")

  if (by == "ac") {

    ## (4): 3 vertical bars (accuracy in middle): ----------

    # Number and basic width of columns:
    nr.col <- 3         # number of (vertical) columns
    col.x  <- 1/nr.col  # corresponding column width

    # Length parameters:
    lbase <- N    # length of base side (vertical: y)
    lelse <- 1/(2 * nr.col)  # length of other side (horizontal: x)
    scale <- 1.0  # scaling factor (0-1)

    # Basic height and width:
    b.ly <- lbase * scale  # basic height (scaled constant)
    b.lx <- lelse * scale  # basic width (scaled constant)

    ## Draw rectangles: ------

    ## (a) N column: ----

    # Dimensions and coordinates:
    n.ly <- b.ly    # height (y)
    col.nr <- 1     # column number (out of nr.col)
    n.x  <- (x.base + (col.nr * col.x) - (col.x/2))  # x-coordinate: mid point of column col.nr
    n.y  <- y.base  # y-coordinate

    if (dir == 2) {
      ## center N bar around 0:
      n.y  <- y.base - N/2
    }

    # Plot 1 box:
    plot_vbox(type = NA, fname = "N", freq = N,
              box.x  = n.x,
              box.y  = n.y,
              box.lx = b.lx,
              box.ly = n.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("N", n.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("N"),
                     ...)


    ## (b) SDT column: ----

    # x-coordinates:
    col.nr <- 3
    hi.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    mi.x <- hi.x
    fa.x <- hi.x
    cr.x <- hi.x

    # heights (in y-direction):
    hi.ly <- (n.ly * prev) * sens              # re-computes n.hi (without rounding)
    mi.ly <- (n.ly * prev) * (1 - sens)        # re-computes n.mi (without rounding)
    cr.ly <- (n.ly * (1 - prev)) * spec        # re-computes n.cr (without rounding)
    fa.ly <- (n.ly * (1 - prev)) * (1 - spec)  # re-computes n.fa (without rounding)

    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi.ly <- -1 * mi.ly
      fa.ly <- -1 * fa.ly
    }

    # y-coordinates (given heights):
    hi.y <- y.base
    cr.y <- hi.y + hi.ly
    mi.y <- cr.y + cr.ly
    fa.y <- mi.y + mi.ly

    if (dir == 2) {
      ## reverse direction of 2 bars:
      mi.y <- y.base
      fa.y <- mi.y + mi.ly
    }

    # Plot 4 boxes:
    plot_vbox(type = NA, fname = "hi", freq = n.hi,
              box.x  = hi.x,
              box.y  = hi.y,
              box.lx = b.lx,
              box.ly = hi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "mi", freq = n.mi,
              box.x  = mi.x,
              box.y  = mi.y,
              box.lx = b.lx,
              box.ly = mi.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "fa", freq = n.fa,
              box.x  = fa.x,
              box.y  = fa.y,
              box.lx = b.lx,
              box.ly = fa.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "cr", freq = n.cr,
              box.x  = cr.x,
              box.y  = cr.y,
              box.lx = b.lx,
              box.ly = cr.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("hi", hi.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("hi"),
                     ...)


    ## (c) Accuracy column: ----

    # x-coordinates:
    col.nr <- 2
    dec.cor.x <- (x.base + (col.nr * col.x) - (col.x/2))  # mid point of column col.nr
    dec.err.x <- dec.cor.x

    # heights:
    dec.cor.ly <- abs(hi.ly) + abs(cr.ly)
    dec.err.ly <- abs(mi.ly) + abs(fa.ly)

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.ly <- -1 * dec.err.ly
    }

    # y-coordinates (given heights):
    dec.cor.y <- y.base
    dec.err.y <- dec.cor.y + dec.cor.ly

    if (dir == 2) {
      ## reverse direction of 1 bar:
      dec.err.y <- y.base
    }

    # Plot 2 boxes:
    plot_vbox(type = NA, fname = "dec.cor", freq = (n.hi + n.cr),
              box.x  = dec.cor.x,
              box.y  = dec.cor.y,
              box.lx = b.lx,
              box.ly = dec.cor.ly,
              show.freq, ...)

    plot_vbox(type = NA, fname = "dec.err", freq = (n.mi + n.fa),
              box.x  = dec.err.x,
              box.y  = dec.err.y,
              box.lx = b.lx,
              box.ly = dec.err.ly,
              show.freq, ...)

    # Column name:
    plot_ftype_label("dec.cor", dec.cor.x, y.min, pos = 1,
                     col = pal["txt"],
                     # col = comp_freq_col("dec.cor"),
                     ...)

  } # if (by == "ac")

  ## +++ here now +++ ##

  else if (by == "xxx") {

    ## Using bar plot: ----------

    gap <- 0

    ## Plot dimensions:
    xlim = c(0, (5 + gap))
    ylim = c(0, N)

    ftab <- cbind(c(N, 0, 0, 0), # N
                  c(n.hi + n.mi, 0, n.fa + n.cr, 0), # by condition
                  c(n.hi,  n.mi, n.fa,  n.cr),       # 4 sdt categories
                  c(n.hi + n.fa, 0, n.mi + n.cr, 0)  # by decision
    )
    colnames(ftab) <- c("N", "by cd", "sdt", "by dc")
    rownames(ftab) <- c("hi", "mi", "fa", "cr")

    barplot(ftab,
            # main = "Plot title",
            xlab = "x-axis label",
            ylab = "y-axis label",
            col = rev(c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])),
            legend = rev(rownames(ftab)),
            add = FALSE
    )

  } # if (by == "xxx")


  ## Title and margin text: --------

  # title(cur.title.lbl, adj = 0.5, line = 1.5, font.main = 1) # (centered, raised, normal font)
  title(cur.title.lbl, adj = 0.0, line = 1.5, font.main = 1) # (left, raised, normal font)

}


## Check:
{
  ## Basics:
  # plot_bar(prev = .33, sens = .75, spec = .66, title.lbl = "Test 1")
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, title.lbl = "Test 2")  # by = "all" (default)
  #
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", title.lbl = "Test 3a")  # by condition
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", title.lbl = "Test 3b", dir = 2) # bi-directional
  #
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", title.lbl = "Test 4a")  # by decision
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", title.lbl = "Test 4b", dir = 2) # bi-directional
  #
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", title.lbl = "Test 5a")  # by accuracy
  # plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", title.lbl = "Test 5a", dir = 2) # bi-directional

  ## Note:
  # plot_bar(N = 3, prev = .33, sens = .75, spec = .66,
  #          title.lbl = "Test rounding effects",
  #          show.freq = TRUE)  # => Rounding of freq, but not of proportions shown in plot.

}

## -----------------------------------------------
## (*) Done:

## - Scale 1 dimension by N (and add axis) [2018 08 13]
## - Add area labels (in center of area)   [2018 08 14]
## - Add options for by ("all", "cd", "dc", "ac") and
##                   dir (1 vs. 2)         [2018 08 15]

## -----------------------------------------------
## (+) ToDo:

## - Use text labels defined in txt.def and init_txt (incl. accuracy).
## - Add probabilitiy indicators (arrows and labels)
## - Allow alternative arrangements: horizontal (flip coord), dodged bars, ...
## - ...

## -----------------------------------------------
## eof.
