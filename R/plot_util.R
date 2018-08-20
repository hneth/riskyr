## plot_util.R | riskyr
## 2018 08 20
## Helper functions for plotting objects (freq and prob).
## -----------------------------------------------

## plot_vbox: Plot a vertical box and its text labels ----------
plot_vbox <- function(box.x,  box.y,    # coordinates x (center) and y (bottom)
                      box.lx, box.ly,   # lengths of box (width and height)
                      ## Text labels:
                      type = NA,        # type of box (printed as title below box)
                      show.freq = TRUE, # option to show/hide frequency labels
                      fname = NA,       # frequency name (corresponding to a color in pal, as character)
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

  ## Fill color:
  col.fill <- comp_freq_col(fname)
  # if (fname %in% names(pal)) { # if name corresponds to a color name in pal
  #  col.fill <- pal[fname]     # use this color to fill box
  #}


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
    if (!is.na(fname)) {  # frequency name is specified:

      # ToDo: Derive freq from type and/or fname.

      box.lbl <- paste0(fname, " = ", freq)

    } else { # no fname specified:

      box.lbl <- paste0(freq)

    }

    # Plot freq label:
    text(x = box.x, y = mid.y,
         labels = box.lbl,
         # pos = NULL,  # default
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
  # plot_vbox(10, 80, 20, 20, freq = 111) # no name => default fill color
  # plot_vbox(50, 80, 20, 20, fname = "N", freq = 222) # no type label
  # plot_vbox(80, 80, 20, 20, fname = "cond.true", freq = 232, type = comp_freq_type("cond.true"))
  # plot_vbox(10, 50, 30, 20, type = "type as box title", name = "hi", freq = 333)
  # plot_vbox(40, 50, 20, 20, fname = "mi", freq = 444, lwd = 3, cex = .7, type = comp_freq_type("mi"))
  #
  # plot_vbox(70, 50, 20, 20, fname = "asdf", freq = 555, type = comp_freq_type("asdf"))
}


## Distinguish between 2 separate functions:
#   1. generic plot_cbox (that plots a box given its CENTER coordinates and format) vs.
#   2. plot_freq (that determines current freq value and fill color for know freq).

## plot_cbox: Plot a CENTERED box with text labels ----------
plot_cbox <- function(x,  y,    # coordinates of box CENTER (x and y)
                      lx, ly,   # lengths of box (width and height)
                      ## Text labels:
                      lbl     = NA,       # main label (in middle)
                      lbl.top = NA,       # title (at top)
                      lbl.bot = NA,       # caption (at bottom)
                      ## Color options:
                      col.fill = grey(.95, .50),  # default fill color
                      col.brd = pal["brd"],       # default border color
                      col.txt = pal["txt"],       # default label color
                      ...  # other graphical parameters: lwd, cex, ...
) {

  ## (0) Parameters (currently fixed):

  # Compute box coordinates:
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
#
# plot_cbox(1, 5, 1, 1)  # default color, no text labels
# plot_cbox(2, 5, 1, 1, lbl = "Label", lbl.top = "Title:", lbl.bot = "Caption.")  # add text labels
# plot_cbox(3, 5, 1, 1, lbl = "Label", lbl.top = "Title:", lbl.bot = "Caption.",
#           cex = .75, font = 2,                                # add text options
#           col.fill = "gold", col.brd = "steelblue", lwd = 3)  # add color options


## plot_fbox: Plot a known frequency (freq) as a box ----------
plot_fbox <- function(fname,   # name of a known frequency (freq)
                      x,  y,   # coordinates of box CENTER (x and y)
                      lx, ly,  # lengths of box (width and height)
                      ## Text labels:
                      # lbl     = NA,       # label (in middle): freq
                      # lbl.top = NA,       # title (at top)
                      # lbl.bot = NA,       # caption (at bottom)
                      ltype = "default",    # type of freq label
                      ## Color options:
                      col.fill = col,  # if missing, color of fname freq is derived below
                      # col.brd = pal["brd"],
                      # col.txt = pal["txt"],
                      ...  # other graphical parameters: lwd, cex, pos, ...
) {

  # Initialize:
  fval <- NA
  ftype <- NA
  fcol <- NA
  flbl <- NA

  if (fname %in% names(freq)) { # if freq corresponds to named frequency in freq:

    # Derive current values corresponding to freq:
    ix <- which(names(freq) == fname)  # index in freq

    # (a) Value of frequency in freq:
    fval <- freq[ix]

    # (b) Type of frequency:
    ftype <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    # (c) Color of frequency box:
    if (missing(col.fill)) {  # no col.fill has been specified:
      # if (is.na(col)) {  # no col has been specified:
      fcol <- comp_freq_col(fname)  # determine default fcol corresponding to fname in freq and pal
    } else {
      fcol <- col.fill  # use the color specified in function call
      # fcol <- col  # use the color specified in function call
    }
    # print(fcol)

    # (d) Label of frequency:
    flbl <- label_freq(fname, ltype = ltype)
    # print(flbl)

    # (e) Plot corresponding cbox with values of fname freq:
    plot_cbox(x = x, y = y, lx = lx, ly = ly,
              lbl = paste0(flbl, " = ", fval),
              lbl.bot = paste0(ftype),
              col.fill = fcol,
              ...)

  } else {  # fname is NOT a known freq:

    # (a) Fill color of frequency box:
    if (missing(col.fill)) {  # no col.fill has been specified:
      fcol <- grey(.95, .50)  # default fill color
    } else {
      fcol <- col.fill  # use the color specified in function call
    }
    # print(fcol)

    # (b) Plot cbox with default settings:
    plot_cbox(x = x, y = y, lx = lx, ly = ly,
              lbl = paste0(fname),
              # lbl.bot = paste0(ftype),
              col.fill = fcol,
              ...)

  }

}

## Check:
# plot(0:1, 0:1, type = "n", xlab = "x-axis", ylab = "y-axis",
#      xlim = c(0, 10), ylim = c(0, 6))
# ## Basics:
# plot_fbox(fname = "N", 5, 5, 1, 2/3)
# plot_fbox(fname = "cond.true", 3, 4, 2, 2/3)
# plot_fbox(fname = "cond.false", 7, 4, 2, 2/3, ltype = "nam")
# plot_fbox(fname = "hi", 2, 3, 1, 2/3, ltype = "nam")
# plot_fbox(fname = "mi", 4, 3, 1, 2/3, ltype = "nix")
# plot_fbox(fname = "fa", 6, 3, 1, 2/3)
# plot_fbox(fname = "cr", 8, 3, 1, 2/3)
# plot_fbox(fname = "dec.pos", 3, 2, 2, 2/3)
# plot_fbox(fname = "dec.neg", 7, 2, 2, 2/3)
# # plot_fbox(fname = "dec.cor", 3, 1, 2, 2/3)
# # plot_fbox(fname = "dec.err", 7, 1, 2, 2/3)
# plot_fbox(fname = "N", 5, 1, 1, 2/3, col = "green2", col.brd = "red3", cex = .6, lwd = 3)
# ## arbitrary boxes (with unknown freq): ###
# plot_fbox(fname = "unknown_freq", 9, 2, 1, 2/3)  # unknown fname (freq) with defaults
# plot_fbox(fname = "other_freq", 9, 1, 1, 2/3, col = "gold", cex = .7, font = 2)


## plot_line: Plot an (arrow) line between 2 points (with optional text label): ------
plot_line <- function(x0, y0, x1, y1,      # coordinates of p1 and p2
                      # lty = 1, lwd = 1,                   # line options
                      pt.pch = 21, pt.cex = 1, pt.lwd = 1,  # point options
                      arr.code = 0,         # 0...none, 1+2...arrows, 3...double arrow
                      ## Optional text label:
                      lbl.txt = NA,         # string for text label
                      lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
                      lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
                      lbl.pos = NULL,       # pos (NULL = default, 1 = left, 2 = top, etc.)
                      lbl.off = .5,         # offset of text label
                      ## Colors:
                      col.fill = pal["brd"], # if missing, color of fname freq is derived below
                      col.brd =  pal["brd"],
                      col.txt =  pal["txt"],
                      # lbl.cex = 1,        # size of text label
                      ...                   # other graphical parameters: lwd, cex, pos, ...
) {

  ## (0) Preparations:

  # ## Determine col.fill:
  # if (missing(col.fill)) {  # no color was specified:
  #   col.fill <- pal["brd"] # default fill color
  # } else {
  #   col.fill <- col  # use the color specified in function call
  # }
  #
  # print(col.fill)

  arrow <- TRUE # FALSE # initialize

  ## (1) Draw an arrow between both points:

  if (arr.code > 0) {

    # Draw an arrow between both points:
    arrows(x0, y0, x1, y1,
           length = .10, angle = 33, code = arr.code,    # V shape (small)
           # length = .10, angle = 90, code = arr.code,  # T shape
           col = col.fill,
           ...)  # lty, lwd, ...

  } else { # no arrow heads:

    ## Draw a line with 2 points at line ends:
    arrows(x0, y0, x1, y1,
           length = 0, angle = 0, code = 0,       # no arrows
           col = col.fill,
           ...)  # lty, lwd, ...

    if (arr.code < 0) {  # draw points:

      if (arr.code == -1 || arr.code == -3) {
        points(x0, y0, pch = pt.pch, cex = pt.cex,    # 1st point
               lwd = pt.lwd, col = col.brd, bg = col.fill)
      }

      if (arr.code == -2 || arr.code == -3) {
        points(x1, y1, pch = pt.pch, cex = pt.cex,    # 2nd point
               lwd = pt.lwd, col = col.brd, bg = col.fill)
      }

    }

  } # if (arr.code ...

  ## (2) Optional text label: ------

  if (!is.na(lbl.x)) { # if lbl.x exists:

    ## Text label:
    text(lbl.x, lbl.y,
         labels = lbl.txt,
         col = col.txt,  # text color
         pos = lbl.pos,
         offset = lbl.off,
         ...)  # cex, ...
  }

}

## Check:
# plot(0:10, 0:10, type = "n") # 2 points
# # (1) without labels:
# plot_line(0, 10, 9, 10)  # basic line (without label)
# plot_line(0, 9, 9, 9, arr.code = 1)  # basic arrow (without label)
# plot_line(0, 8, 9, 8, arr.code = 2)  # basic arrow (without label)
# plot_line(0, 7, 9, 7, arr.code = 3)  # double arrow (without label)
# plot_line(0, 6, 9, 6, arr.code = -1) # arrow with points (without label)
# plot_line(0, 5, 9, 5, arr.code = -2) # arrow with points (without label)
# plot_line(0, 4, 9, 4, arr.code = -3) # arrow with points (without label)
# # (2) with labels:
# plot_line(0, 3, 9, 3, arr.code = 3,
#           lbl.txt = "Label 1", cex = .8, lty = 2, lwd = .5)  # text label (on line) and options
# plot_line(0, 2, 9, 2, lbl.txt = "Label 2", arr.code = -3,
#           lbl.pos = 4, lbl.off = 1,
#           col.fill = "firebrick", col.txt = "forestgreen",
#           font = 2, cex = .8)  # basic with raised text label
# plot_line(0, 1, 9, 9,  arr.code = -3,
#           pt.pch = 22, pt.cex = 2, pt.lwd = 2,  # point paramters
#           # Text label (with options):
#           lbl.x = 10, lbl.y = 9,
#           lbl.txt = "Some label\nthat takes\nmultiple (3) lines",
#           lbl.pos = NULL, lbl.off = 0,
#           # Colors:
#           col = "gold", col.brd = "steelblue", col.txt = "steelblue",
#           cex = .7, lty = 2, lwd = 2               # grapical parameters
# )

## plot_arrs: Plot multiple (n.arr) arrows along a line: ------
plot_arrs <- function(x0, y0, x1, y1,       # coordinates
                      n.arr = 2,            # number of arrows to draw
                      l.arr = .10,          # length of arrows to draw
                      a.arr = 35,           # angle of arrows to draw
                      ## Optional label:
                      lbl.txt = NA,         # string for text label
                      lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
                      lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
                      pos = NULL,           # pos (NULL = default; 1 = bottom, 2 = left, 3 = top)
                      offset = 1,           # offset, etc.
                      ...                   # other graphical parameters
)
{
  ## (0) Draw line from p1 to p2: ----

  # lines(c(x0, x1), c(y0, y1), ...)


  ## (1) Draw n.arr arrows: ----

  # Split line into n.arr + 1 segments:
  Ax = seq(x0, x1, length = n.arr + 1)
  Ay = seq(y0, y1, length = n.arr + 1)

  # Loop to draw all arrows:
  for (i in 1:n.arr)
  {
    arrows(Ax[i], Ay[i], Ax[i + 1], Ay[i + 1],
           length = l.arr, angle = a.arr, code = 2, # arrow type: V or T?
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
# plot(0:1, 0:1, type = "n") # 2 points
# plot_arrs(0, 0, 1, 0, col = "red3")  # 2 arrows, no text
# plot_arrs(0, .1, 1, .1, col = "grey", lbl.txt = "Label 0")
# plot_arrs(0, .2, 1, .2, col = "green3", lbl.txt = "Label 1", pos = 3)
# plot_arrs(0, .3, 1, .5, col = "blue3",
#             n.arr = 3, l.arr = .25, a.arr = 20,
#             lbl.txt = "3 arrows", pos = 3, lwd = 2)
# plot_arrs(0, .4, 1, .9, col = "black", lbl.txt = "Label 3\nis a longer\nand wider label\nin smaller font", pos = 3, offset = 2, cex = .8)


## box_text: Add text with background box to a plot ------
## from https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r

## Add text with background box to a plot

# \code{box_text} places a text given in the vector \code{labels}
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
# box_text(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480",
#    pos = 4, font = 2, cex = 1.3, padding = 1)
#

box_text <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
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
  # box_text(x = c(0.1, 0.8), y = c(0.1, 0.7), labels = c("some Text", "something else"),
  #         col.bg = "gold", pos = 4, padding = 0.2)
  #
  # ## Tweak cex, font and adj:
  # box_text(x = 0.2, y = 0.4, labels = "some big and bold text",
  #         col.bg = "skyblue", adj = c(0, 0.6), font = 2, cex = 1.8)
}

## plot_ftype_label: Label the type corresponding to fname at (x, y): ----------

plot_ftype_label <- function(fname,  # name of a known freq
                             x, y,       # coordinates
                             # pos = NULL,      # pos (NULL = default; 1 = bottom, 2 = left, 3 = top)
                             # offset = 0.5,    # offset, etc.
                             # col = pal["txt"],  # default color
                             ...                # other graphical parameters
){

  ftype_lbl <- ""  # initialize
  ftype_lbl <- paste0(comp_freq_type(fname))  # determine freq_type corresponding to fname


  # Plot text label:
  text(x, y,
       labels = ftype_lbl,
       # pos = pos,
       # offset = offset,
       xpd = TRUE,
       # col = col, # pass on parameter
       ...)

  # # Return type (as name):
  # return(ftype_lbl)

}

## Check:
# plot(0:1, 0:1) # 2 points
# plot_ftype_label("N", .1, .9)
# plot_ftype_label("cond.false", .2, .8, cex = .8)
# plot_ftype_label("dec.pos", .3, .7, col = "red3")
# plot_ftype_label("dec.cor", .7, .3, col = "gold")
# plot_ftype_label("hi", .5, .5, col = "green3")
# plot_ftype_label("hi", .5, .5, col = "steelblue1", pos = 1)
# plot_ftype_label("mi", .5, .5, col = "steelblue2", pos = 2)
# plot_ftype_label("fa", .5, .5, col = "steelblue3", pos = 3)
# plot_ftype_label("cr", .5, .5, col = "steelblue4", pos = 4)


## Define objects: Create an object of type "box" as a list: ----------
box0 <- list(name = "box0_name", x = .5, y = .5, lx = 1, ly = 1)  # object as list
class(box0) <- "box"  # name class

## Check:
# box0 # shows object (list)

## box class: Create constructor function for the "box" class: ----------
make_box <- function(name, x, y, lx, ly) {

  # Note: It is good practice to give the constructor function
  #       the same name as the class (here: box).  However, as
  #       the function box exists (in graphics), we use make_box here.

  # Check integrity of arguments:
  if (!is.character(name)) stop("name must be a character.")
  if (!is.numeric(x)) stop("x must be numeric.")
  if (!is.numeric(y)) stop("y must be numeric.")
  if (!is.numeric(lx)) stop("lx must be numeric.")
  if (!is.numeric(ly)) stop("ly must be numeric.")
  # if (x < x_min || x > x_max)  stop("x must be in valid range.")
  # if (y < y_min || y > y_max)  stop("y must be in valid range.")

  # Create object as a list:
  obj <- list(name = name, x = x, y = y, lx = lx, ly = ly)

  # Set class by using class() or attr() function:
  class(obj) <- "box"          # name class
  attr(obj, "class") <- "box"  # set attr

  # Return object:
  obj
}

## Check:
# box1 <- make_box(1, 0, 0, 1, 1)  # => Error due to stop; no object created.
# box1 <- make_box("box1_name", .1, .1, 1, 1) # use constructor function to create new objects.
# box1

## box methods: Create generic print and plot methods for box objects: ---------

print.box <- function(obj) {
  cat("box name:", obj$name, "\n")
  cat("position: x =", obj$x, "; y =", obj$y, "\n")
  cat("width:   lx =", obj$lx, "\n")
  cat("height:  ly =", obj$ly, "\n")
}

plot.box <- function(obj, ...) {

  ## Call plot_fbox helper function:
  plot_fbox(fname = obj$name,
            x = obj$x, y = obj$y, lx = obj$lx, ly = obj$ly,
            ...)
}

# ## Check:
# # Create some box objects:
# box_b1 <- make_box("1st_box", 3, 9, 2, 2)  # 1st box with an arbitrary label
# box_b2 <- make_box("2nd_box", 3, 6, 2, 2)  # 2nd box with an arbitrary label
# box_hi <- make_box("hi", 3, 3, 2, 2)       # box with known freq label
# box_mi <- make_box("mi", 6, 3, 2, 2)       # box with known freq label
# print(box_b1)
# print(box_hi)
# # Plot boxes:
# plot(c(0, 10), c(0, 10), type = "n") # 2 points, empty canvas
# plot(box_b1)  # plot box with arbitrary label (and default color)
# plot(box_b2, col = "skyblue", cex = 2/3, font = 2)  # plot box with arbitrary label (and specific color)
# plot(box_hi)  # plot box with known freq label (and type, color, etc.)
# plot(box_mi, ltype = "nam",
#      cex = 2/3, lwd = 4, col = "gold", font = 2) # overwrite default parameters


## plot_link: Plot link between 2 boxes ----------

plot_link <- function(box1, box2, pos1, pos2, ...) {

  # (1) Determine link coordinates:

  # 1st point:
  if (is.null(pos1) | pos1 == 0) {
    x1 <- box1$x  # x in center of box1
    y1 <- box1$y  # y in center of box1
  } else if (pos1 == 1) {
    x1 <- box1$x              # x in center of box1
    y1 <- box1$y - box1$ly/2  # y at bottom of box1
  } else if (pos1 == 2) {
    x1 <- box1$x - box1$lx/2  # x at left of box1
    y1 <- box1$y              # y in center of box1
  } else if (pos1 == 3) {
    x1 <- box1$x              # x in center of box1
    y1 <- box1$y + box1$ly/2  # y at top of box1
  } else if (pos1 == 4) {
    x1 <- box1$x + box1$lx/2  # x at right of box1
    y1 <- box1$y              # y in center of box1
  } else { # default:
    x1 <- box1$x  # x in center of box1
    y1 <- box1$y  # y in center of box1
  }

  # 2nd point:
  if (is.null(pos2) | pos1 == 0) {
    x2 <- box2$x  # x in center of box2
    y2 <- box2$y  # y in center of box2
  } else if (pos2 == 1) {
    x2 <- box2$x              # x in center of box2
    y2 <- box2$y - box2$ly/2  # y at bottom of box2
  } else if (pos2 == 2) {
    x2 <- box2$x - box2$lx/2  # x at left of box2
    y2 <- box2$y              # y in center of box2
  } else if (pos2 == 3) {
    x2 <- box2$x              # x in center of box2
    y2 <- box2$y + box2$ly/2  # y at top of box2
  } else if (pos2 == 4) {
    x2 <- box2$x + box2$lx/2  # x at right of box2
    y2 <- box2$y              # y in center of box2
  } else { # default:
    x2 <- box2$x  # x in center of box1
    y2 <- box2$y  # y in center of box1
  }

  # (2) Plot link:
  plot_line(x1, y1, x2, y2, ...)

}

## Check:
# box_b1 <- make_box("1st_box", 5, 9, 2, 2)  # 1st box with an arbitrary label
# box_b2 <- make_box("2nd_box", 3, 6, 2, 2)  # 2nd box with an arbitrary label
# box_hi <- make_box("hi", 7, 3, 2, 2)       # box with known freq label
## Prepare canvas:
# plot(c(0, 10), c(0, 10), type = "n") # 2 points, empty canvas
# plot(box_b1)  # plot box with arbitrary label (and default color)
# plot(box_b2, col = "skyblue", cex = 2/3, font = 2)  # plot box with arbitrary label (and specific color)
# plot(box_hi)  # plot box with known freq label (and type, color, etc.)
## Link positions:
# plot_link(box_b1, box_b2, 0, 0)  # 0-0: link from center to center
# plot_link(box_b1, box_b2, 2, 2)  # 2-2: link from left to left
# plot_link(box_b1, box_b2, 1, 3)  # 1-3: link from bottom to top
# plot_link(box_b1, box_b2, 3, 1)  # 3-1: link from top to bottom
# plot_link(box_b1, box_b2, 4, 4)  # 1-3: link from right to right
## Link options:
# plot_link(box_b2, box_hi, 0, 0, arr.code = 0,
#           lbl.txt = "some label", lbl.pos = NULL, cex = .8,
#           col.txt = "steelblue", col.fill = "grey", lwd = 20)

## +++ here now +++

## plot_plink: Link boxes of 2 known frequencies (and label link by probability)


## (*) Done: ----------

## - Defined box class and print.box and plot.box methods [2018 08 19].
## - Started this collection [2018 08 16].

## (+) ToDo: ----------

## - new link_boxes function that draws lines or arrows between 2 boxes
##   (arrow position per box = 1 to 4 (side of arrow: NULL vs. bltr).)
## - Link 2 boxes (with optional arrows, labels, and colors, etc.)
## - ...

## eof. ----------
