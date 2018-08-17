## plot_util.R | riskyr
## 2018 08 17
## -----------------------------------------------
## Collection of helper functions for plotting
## -----------------------------------------------

## Helper function: Plot a box and its text labels ----------
plot_box <- function(box.x,  box.y,    # coordinates x (center) and y (bottom)
                     box.lx, box.ly,   # lengths of box (width and height)
                     ## Text labels:
                     type = NA,        # type of box (printed as title below box)
                     show.freq = TRUE, # option to show/hide frequency labels
                     name = NA,        # box name (corresponding to a color in pal, as character)
                     freq,             # frequency (as number).  ToDo: Derive freq from type and/OR name!
                     ## Color options:
                     col.fill = grey(.95, .75),  # default color (but derived from name below)
                     col.brd = pal["brd"],
                     col.txt = pal["txt"],
                     ...               # other graphical parameters: lwd, cex, ...
) {


  ## (0) Parameters (currently fixed):

  ## Box parameters:
  # box.lwd <- 1  # line width of border around rect (default = 1)

  if (name %in% names(pal)) { # if name corresponds to a color name in pal
    col.fill <- pal[name]     # use this color to fill box
  }

  ## Text parameters:
  # col.lbl <- pal["txt"]  # defined in pal
  # cex.lbl <- .90   # scaling factor for text labels
  # cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  # h.shift <- .05   # horizontal shifting of labels
  # v.shift <- .05   # vertical shifting of labels

  ## (1) Plot rect:
  rect(xleft  = (box.x - box.lx/2), ybottom = box.y,
       xright = (box.x + box.lx/2), ytop    = (box.y + box.ly),
       col = col.fill,
       border = col.brd,
       # lwd = box.lwd,
       ...)

  ## (2) Print type as box title (below box, optional):
  if (!is.na(type)) {  # type is specified:

    text(x = box.x, y = box.y,
         labels = type,
         pos = 1,  # 1...below, 3...above
         xpd = TRUE,
         col = col.txt,
         # cex = cex.lbl.sm,
         ...)

  }

  ## (3) Plot box label (centered in box, optional):
  if (show.freq) {

    # y-coordinate of label:
    mid.y <- box.y + box.ly/2  # y-value of mid point

    # Compose box label:
    if (!is.na(name)) {  # name is specified:

      # ToDo: Derive freq from type and/or name.

      box.lbl <- paste0(name, " = ", freq)

    } else { # no name specified:

      box.lbl <- paste0(freq)

    }

    # Plot text:
    text(x = box.x, y = mid.y,
         labels = box.lbl,
         # pos = 3,
         xpd = TRUE,
         col = col.txt,
         # cex = cex.lbl.sm,
         ...)
  }

}

## Check:
{
  # plot(c(0, 100), c(0, 100)) # 2 points
  #
  # plot_box(10, 80, 20, 20, freq = 111) # no name => default fill color
  #
  # plot_box(50, 80, 20, 20, name = "N", freq = 222) # no type label
  #
  # plot_box(10, 50, 30, 20, type = "type as box title", name = "hi", freq = 333)
  #
  # plot_box(40, 50, 20, 20, name = "mi", freq = 123, lwd = 3, cex = .7)

}

## +++ here now +++

## ToDo:
## - Distinguish 2 separate functions:
##   1. generic plot_box vs.
##   2. plot_freq (that automatically determines current freq value and fill color).




## Goal: ---------------------------
## Distinguish 2 separate functions:
##   1. generic plot_cbox (that plots a box given its coordinates and format) vs.
##   2. plot_freq (that determines current freq value and fill color for know freq).

## Helper function: Plot a centered box with text labels ----------
plot_cbox <- function(x,  y,   # coordinates of box CENTER (x and y)
                      lx, ly,   # lengths of box (width and height)
                      ## Text labels:
                      lbl     = NA,       # main label (in middle)
                      lbl.top = NA,       # title (at top)
                      lbl.bot = NA,       # caption (at bottom)
                      ## Color options:
                      col.fill = grey(.95, .50),
                      col.brd = pal["brd"],
                      col.txt = pal["txt"],
                      ...  # other graphical parameters: lwd, cex, ...
) {


  ## (0) Parameters (currently fixed):

  ## Box coordinates:
  x_left = (x - lx/2)
  x_right = x_left + lx
  y_bottom = (y - ly/2)
  y_top = y_bottom + ly

  ## (1) Plot rect:
  rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top,
       col = col.fill,
       border = col.brd,
       ...)

  ## (2) Print optional text labels:
  if (!is.na(lbl)) {

    text(x = x, y = y,
         labels = paste0(lbl),
         pos = NULL,  # center (default), 1...below, 3...above
         xpd = TRUE,
         col = col.txt,
         ...)

  }

  if (!is.na(lbl.top)) {

    text(x = x, y = y_top,
         labels = paste0(lbl.top),
         pos = 3,  # 1...below, 3...above
         xpd = TRUE,
         col = col.txt,
         ...)

  }

  if (!is.na(lbl.bot)) {

    text(x = x, y = y_bottom,
         labels = paste0(lbl.bot),
         pos = 1,  # 1...below, 3...above
         xpd = TRUE,
         col = col.txt,
         ...)

  }


}

## Check:

# plot(0:1, 0:1, type = "n", xlab = "x-axis", ylab = "y-axis",
#     xlim = c(0, 10), ylim = c(0, 6))

# plot_cbox(1, 5, 1, 1)  # default color, no text labels
# plot_cbox(2, 5, 1, 1, lbl = "Label", lbl.top = "Title:", lbl.bot = "Caption.")  # add text labels
# plot_cbox(3, 5, 1, 1, lbl = "Label", lbl.top = "Title:", lbl.bot = "Caption.",
#           cex = .75, font = 2,                                # add text options
#           col.fill = "gold", col.brd = "steelblue", lwd = 3)  # add color options


## Helper function: Plot a known frequency (freq) as a box  ----------
plot_fbox <- function(x,  y,   # coordinates of box CENTER (x and y)
                      lx, ly,  # lengths of box (width and height)
                      fname,   # name of a known frequency (freq)
                      ## Text labels:
                      # lbl     = NA,       # label (in middle)
                      # lbl.top = NA,       # title (at top)
                      # lbl.bot = NA,       # caption (at bottom)
                      ## Color options:
                      # col.fill = grey(.95, .50),
                      # col.brd = pal["brd"],
                      # col.txt = pal["txt"],
                      ...  # other graphical parameters: lwd, cex, ...
) {

  # Initialize:
  fval <- NA
  colname <- NA
  ftype <- NA
  fcol <- NA

  if (fname %in% names(freq)) { # if freq corresponds to named frequency in freq:

    ## Derive current values corresponding to freq:
    ix <- which(names(freq) == fname)  # index in freq

    # (a) Value of frequency in freq:
    fval <- freq[ix]

    # (b) Type of frequency:
    ftype <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    # (c) Color of frequency:
    # Note that names(freq) are sometimes longer than names(pal):
    if (any(grep(pattern = "\\.", x = fname))) {  # if fname contains a dot (.):

      nameparts <- unlist(strsplit(fname, split = "\\."))

      fname_1st <- nameparts[1]  # 1st part of fname
      fname_2nd <- nameparts[2]  # 2nd part of fname

      colname <- fname_2nd  # 2nd part corresponds to name of color

      # if (colname == "true") { colname <- "cor" }

    } else {
      colname <- fname
    }

    # print(colname)

    # Find color value of colname in color pal:
    if (colname %in% names(pal)) { # if colname corresponds to a color name in pal
      fcol <- pal[colname]     # use this color to fill box
    } else {
      fcol <- grey(.95, .50) # use some default color (e.g., "white")
    }

    # print(fcol)

  }


  ## Plot corresponding cbox:
  plot_cbox(x, y, lx, ly,
            lbl = paste0(fname, " = ", fval),
            lbl.bot = paste0(ftype),
            col.fill = fcol,
            ...)

}

## Check:

# plot(0:1, 0:1, type = "n", xlab = "x-axis", ylab = "y-axis",
#     xlim = c(0, 10), ylim = c(0, 6))

# plot_fbox(5, 5, 3, 2/3, fname = "N")
# plot_fbox(3, 4, 2, 2/3, fname = "cond.true")
# plot_fbox(7, 4, 2, 2/3, fname = "cond.false")
# plot_fbox(2, 3, 1, 2/3, fname = "hi")
# plot_fbox(4, 3, 1, 2/3, fname = "mi")
# plot_fbox(6, 3, 1, 2/3, fname = "fa")
# plot_fbox(8, 3, 1, 2/3, fname = "cr")
# plot_fbox(3, 2, 2, 2/3, fname = "dec.pos")
# plot_fbox(7, 2, 2, 2/3, fname = "dec.neg")
# # plot_fbox(3, 1, 2, 2/3, fname = "dec.cor")
# # plot_fbox(7, 1, 2, 2/3, fname = "dec.err")
# plot_fbox(5, 1, 3, 2/3, fname = "N")


## Helper function: Plot an (arrow) line between 2 points with an optional text label: ------
plot_line <- function(x0, y0, x1, y1,     # coordinates of p1 and p2
                      col = "grey",       # colors (for line, point fill, and labels)
                      col.bord = "black", # color of point border
                      lty = 1, lwd = 1,                     # line options
                      pt.pch = 21, pt.cex = 1, pt.lwd = 1,  # point options
                      ## Optional text label:
                      lbl.txt = NA,         # string for text label
                      lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
                      lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
                      lbl.cex = 1,          # size of text label
                      ...                   # pos (1 = bottom, 3 = top), offset, etc.
) {

  ## (1) Draw an arrow or line between 2 points: ------

  arrow <- FALSE

  if (arrow) {

    ## Draw an arrow:
    arrows(x0, y0, x1, y1,
           length = .06, angle = 33, code = 3,    # V shape (small)
           # length = .08, angle = 90, code = 3,  # T shape
           lty = lty, lwd = lwd, col = col)       # arrow

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

  ## (2) Optional text label: ------

  if (!is.na(lbl.x)) { # if lbl.x exists:

    ## Text label:
    text(lbl.x, lbl.y,
         labels = lbl.txt,
         col = col, cex = lbl.cex,
         # pos, offeset, ...
         ...)
  }

}

## Check:

# plot(0:1, 0:1) # 2 points
#
# plot_line(0, .1, 1, .1)  # basic line (without label)
#
# plot_line(0, .2, 1, .2, lbl.txt = "Label 1")  # basic with text label (on line)
#
# plot_line(0, 0, 1, 1, lbl.txt = "Label 2", pos = 3, offset = 2)  # basic with raised text label
#
# plot_line(0, 1, 1, 0,  # coordinates
#           col = "firebrick1", col.bord = "black",   # colors (for line, points, and labels)
#           lty = 1, lwd = 2,                         # line
#           pt.pch = 21, pt.cex = 2, pt.lwd = 2,      # points
#           # Text label (with options):
#           lbl.x = 1/3, lbl.y = 2/3,
#           lbl.txt = "Some label\nthat takes\nmultiple (3) lines",
#           pos = 3, offset = 2, lbl.cex = .8)




## Helper function: Plot multiple (nArr) arrows along a line: ------
plot_arrows <- function(x0, y0, x1, y1,       # coordinates
                        nArr = 2,             # number of arrows to draw
                        ## Optional label:
                        lbl.txt = NA,         # string for text label
                        lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
                        lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
                        pos = 3, offset = 1,  # pos (1 = bottom, 3 = top), offset, etc.
                        ...                   # other graphical parameters
)
{
  ## (0) Draw line from p1 to p2: ----

  # lines(c(x0, x1), c(y0, y1), ...)


  ## (1) Draw nArr arrows: ----

  # Split line into nArr + 1 segments:
  Ax = seq(x0, x1, length = nArr + 1)
  Ay = seq(y0, y1, length = nArr + 1)

  # Loop to draw all arrows:
  for (i in 1:nArr)
  {
    arrows(Ax[i], Ay[i], Ax[i + 1], Ay[i + 1],
           length = .20, angle = 33, code = 2, # arrow type: V or T?
           ...)
  }

  ## (3) Optional text label: ------

  if (!is.na(lbl.x)) { # if lbl.x exists:

    # Parameters:
    # lbl.cex = 1          # size of text label

    ## Text label:
    text(lbl.x, lbl.y,
         labels = lbl.txt,
         # col = col,
         # cex = lbl.cex,
         pos = pos, offset = offset,
         ...)
  }

}

## Check:

# plot(0:1, 0:1) # 2 points
#
# plot_arrows(0, 0, 1, 0, col = "red3")  # 2 arrows, no text
#
# plot_arrows(0, .2, 1, .2, col = "green3", lbl.txt = "Label 1", pos = 3)
#
# plot_arrows(0, .3, 1, .5, col = "blue3", nArr = 3, lbl.txt = "Label 2", pos = 3, lwd = 2)
#
# plot_arrows(0, .4, 1, .9, col = "black", lbl.txt = "Label 3\nis a longer\nand wider label\nin smaller font", pos = 3, offset = 2, cex = .8)



## Helper function: Add text with background box to a plot ------
## from https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r

## Add text with background box to a plot

# \code{boxtext} places a text given in the vector \code{labels}
# onto a plot in the base graphics system and places a coloured box behind
# it to make it stand out from the background.

# @param x numeric vector of x-coordinates where the text labels should be
# written. If the length of \code{x} and \code{y} differs, the shorter one
# is recycled.
# @param y numeric vector of y-coordinates where the text labels should be
# written.
# @param labels a character vector specifying the text to be written.
# @param col.text the colour of the text
# @param col.bg color(s) to fill or shade the rectangle(s) with. The default
# \code{NA} means do not fill, i.e., draw transparent rectangles.
# @param border.bg color(s) for rectangle border(s). The default \code{NA}
# omits borders.
# @param adj one or two values in [0, 1] which specify the x (and optionally
# y) adjustment of the labels.
# @param pos a position specifier for the text. If specified this overrides
# any adj value given. Values of 1, 2, 3 and 4, respectively indicate
# positions below, to the left of, above and to the right of the specified
# coordinates.
# @param offset when \code{pos} is specified, this value gives the offset of
# the label from the specified coordinate in fractions of a character width.
# @param padding factor used for the padding of the box around
# the text. Padding is specified in fractions of a character width. If a
# vector of length two is specified then different factors are used for the
# padding in x- and y-direction.
# @param cex numeric character expansion factor; multiplied by
# code{par("cex")} yields the final character size.
# @param font the font to be used
#
# @return Returns the coordinates of the background rectangle(s). If
# multiple labels are placed in a vactor then the coordinates are returned
# as a matrix with columns corresponding to xleft, xright, ybottom, ytop.
# If just one label is placed, the coordinates are returned as a vector.
#
# @author Ian Kopacka
#
# @examples
# ## Create noisy background
# plot(x = runif(1000), y = runif(1000), type = "p", pch = 16,
# col = "#40404060")
# boxtext(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480",
#    pos = 4, font = 2, cex = 1.3, padding = 1)
#

boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
                    border.bg = NA, adj = NULL, pos = NULL, offset = 0.5,
                    padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){

  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }
  }

  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)
    }
  } else {
    adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]

  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  graphics::rect(xleft = xMid - rectWidth/2,
                 ybottom = yMid - rectHeight/2,
                 xright = xMid + rectWidth/2,
                 ytop = yMid + rectHeight/2,
                 col = col.bg, border = border.bg)

  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
                 adj = c(0.5, 0.5))

  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }
}

## Check:
{
  # ## Create a noisy background:
  # plot(x = runif(1000), y = runif(1000), type = "p", pch = 16, col = "#40404060")
  #
  # ## Vector of labels, using argument 'pos' to position right of coordinates:
  # boxtext(x = c(0.1, 0.8), y = c(0.1, 0.7), labels = c("some Text", "something else"),
  #         col.bg = "gold", pos = 4, padding = 0.2)
  #
  # ## Tweak cex, font and adj:
  # boxtext(x = 0.2, y = 0.4, labels = "some big and bold text",
  #         col.bg = "skyblue", adj = c(0, 0.6), font = 2, cex = 1.8)
}




## -----------------------------------------------
## (*) Done:

## - started this collection [2018 08 16]

## -----------------------------------------------
## (+) ToDo:

## - ...

## -----------------------------------------------
## eof.
