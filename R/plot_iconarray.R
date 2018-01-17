## temp_code.R | riskyR
## 2018 01 08
## -----------------------------------------------

##  This function plots an iconarray flexibly, dependent on population size

## Notes:
  ## if the number of colums diverges largely from the number of rows, icons become distorted.
  ## maybe drawing rectangles is inefficient.

## -----------------------------------------------

dev.new(width = 5, height = 15)  # create device with known aspect ratio.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Preparation:------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# define parameters:
pop <- 111 # define population size.
ncols <- 10  # define number of columns (to be filled by row)
nrows_fix <- 10  # fix number of rows (for each block) to some value.

nrows <- ceiling(pop / ncols)  # calculate the required number of rows (for one block).

source("R/init_num.R")
source("R/init_pal.R")
source("R/comp_freq.R")

sdt.colors

freq <- comp_freq(N = pop)

## (c) SDT (status decision/truth):

n.hi <- freq$hi
n.mi <- freq$mi
n.fa <- freq$fa
n.cr <- freq$cr


sdt <- c(rep("hi", n.hi), rep("mi", n.mi),
  rep("fa", n.fa), rep("cr", n.cr))

icon_colors <- c(rep(sdt.colors["hi"], n.hi), rep(sdt.colors["mi"], n.mi),
                 rep(sdt.colors["fa"], n.fa), rep(sdt.colors["cr"], n.cr))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Plotting symbols:-------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Important insight: an icon array is equivalent to an ordered (position constrained) scatterplot.
# All variants of it display the population concerning some property.

# (A) Four types:
  # 1. Random position, random colors (typical scatterplot)
  # 2. Random position, clustered colors (clustered scatterplot?)
  # 3. Fixed positions (sample of positions constrained), random colors (random icon array)
  # 4. Fixed positions, clustered colors (typical icon array)

# (B) Two dimensions:
  # 1. Position
  # 2. Identity

# (C) Translating these dimensions into code:

  # 1. initialize vectors of positions:
  # TODO: Change data structure?
  posx_vec <- NULL
  posy_vec <- NULL

  # 2. Initialize vector of identities:
  ident_vec <- NULL

  ident_vec <- sdt

  # option 1 to obtain colors:
  icon_colors <- c(rep(sdt.colors["hi"], n.hi), rep(sdt.colors["mi"], n.mi),
                   rep(sdt.colors["fa"], n.fa), rep(sdt.colors["cr"], n.cr))

  # option 2 to obtain colors:
  color_types <- sdt.colors
  ident_types <- unique(ident_vec)  # get number of unique types.
  icon_colors2 <- ident_vec  # initialize colors as identities.
  for(i in ident_types){
    # replace the identities with their respective colors:
    icon_colors2[ident_vec == i] <- color_types[i]
  }

  # sum(icon_colors != icon_colors2)  # test for all equal.

  col_vec <- icon_colors2

# (D) Plotting dependent on this information:
  plotx_dim <- c(0, 1)
  ploty_dim <- c(0, 1)  # assuming square plot x = y.

# --------------------------------------------------------------------------
  # A1 Random position, random colors:
    # 1) Define positions:
      # 1a) draw random positions:
      posx_vec <- runif(n = pop, min = plotx_dim[1], max = plotx_dim[2])
      posy_vec <- runif(n = pop, min = ploty_dim[1], max = ploty_dim[2])

        # checking for duplicates:
        pos_duplicates <- sum(duplicated(cbind(posx_vec, posy_vec)))
        # no duplicated coordinates.

    # 2) Randomize vector:
      col_vec <- sample(icon_colors, replace = FALSE)

    # 3) Plot:
      plot(x = 1, xlim = plotx_dim, ylim = ploty_dim)

      # 3a) set plotting character:
      pch <- 15  # filled square as default.
      cex <- 2

      points(x = posx_vec, posy_vec, # positions.
             # visual details:
             pch = pch, col = col_vec, cex = cex, bty = "o")

  # A1 Random position, clustered colors:
    # 1) Define positions:
    # 1a) draw random positions:
    posx_vec <- runif(n = pop, min = plotx_dim[1], max = plotx_dim[2])
    posy_vec <- runif(n = pop, min = ploty_dim[1], max = ploty_dim[2])

    # checking for duplicates:
    pos_duplicates <- sum(duplicated(cbind(posx_vec, posy_vec)))
    # no duplicated coordinates.

    # 2) Randomize vector:
    col_vec <- sample(icon_colors, replace = FALSE)

    # 3) Plot:
    plot(x = 1, xlim = plotx_dim, ylim = ploty_dim)

    # 3a) set plotting character:
    pch <- 15  # filled square as default.
    cex <- 2

    points(x = posx_vec, posy_vec, # positions.
           # visual details:
           pch = pch, col = col_vec, cex = cex, bty = "o")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Plotting rectangles:----------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create raw plot to accomodate the icons:
plot(1, 1, type = "n",
     xlim = c(0.5, ncols + 0.5), ylim = c(0.5, nrows + 0.5))

# rect(0.55, 0.45, 1.45, 1.45, asp = 1)  # lower left.
#
# rect(1.55, 0.45, 2.45, 1.45, asp = 1)  # to the right of first.
#
# rect(2.55, 0.45, 3.45, 1.45, asp = 1)
#
# rect(3.55, 0.45, 4.45, 1.45, asp = 1)
#
# rect(0.45, 1.55, 1.45, 2.55, asp = 1)  # above first.


design.matrix <- expand.grid(1:ncols, nrows:(1 + ncols - nrows))  # create a matrix for positions.
design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.
design.matrix$icon_colors <- icon_colors

# TODO: Integrate color in design matrix?

for(i in 1:nrow(design.matrix)){

  xleft <- design.matrix[i,1] - 0.45
  xright <- design.matrix[i,1] + 0.45

  ybottom <- design.matrix[i,2] - 0.45
  ytop <- design.matrix[i,2] + 0.45

  rect(xleft, ybottom, xright, ytop, col = design.matrix$icon_colors[i])
}

# points(3, 3, pch = 15)  # does not really work with cex...; maybe try later.

lines(par("usr")[1], par("usr")[2], col = "red")

lines(x = c(1, 5), y = c(5, 5), col = "red")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Blocking A:
# TODo: Blocking should occur dependent on the population size:
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2),
    # mgp = c(0, 0, 0),  # margins for axes.
    mar = c(0, 0, 0, 0)
    )

# (1) create raw plot to accomodate the icons:
plot(1, 1, type = "n",
     xlim = c(0.5, ncols + 0.5), ylim = c(0.5, nrows + 0.5),
     xlab = "",
     ylab = "")

design.matrix <- expand.grid(1:ncols, nrows:(1 + ncols - nrows))  # create a matrix for positions.
design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.
design.matrix$icon_colors <- icon_colors

for(i in 1:nrow(design.matrix)){

  xleft <- design.matrix[i,1] - 0.45
  xright <- design.matrix[i,1] + 0.45

  ybottom <- design.matrix[i,2] - 0.45
  ytop <- design.matrix[i,2] + 0.45

  rect(xleft, ybottom, xright, ytop, col = design.matrix$icon_colors[i])
}

# par(mar = c(5, 0, 4, 2) + 0.1)

# (2) create raw plot to accomodate the icons:
plot(1, 1, type = "n",
     xlim = c(0.5, ncols + 0.5), ylim = c(0.5, nrows + 0.5),
     xlab = "",
     ylab = "")

design.matrix <- expand.grid(1:ncols, nrows:(1 + ncols - nrows))  # create a matrix for positions.
design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.
design.matrix$icon_colors <- icon_colors

for(i in 1:nrow(design.matrix)){

  xleft <- design.matrix[i,1] - 0.45
  xright <- design.matrix[i,1] + 0.45

  ybottom <- design.matrix[i,2] - 0.45
  ytop <- design.matrix[i,2] + 0.45

  rect(xleft, ybottom, xright, ytop, col = design.matrix$icon_colors[i])
}

par(op)  # restore original settings.



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Blocking B:
# TODo: Blocking should occur dependent on the population size:
# (1) create raw plot to accomodate the icons:

system.time({
  design.matrix <- expand.grid(1:ncols, nrows:(1 + ncols - nrows))  # create a matrix for positions.
  design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.
  design.matrix$icon_colors <- icon_colors
})

system.time({
  plot(1, 1, type = "n",
       xlim = c(0.5, ncols + 0.5), ylim = c(0.5, nrows + 0.5),
       xlab = "",
       ylab = "")

  for(i in 1:nrow(design.matrix)){

    xleft <- design.matrix[i,1] - 0.45
    xright <- design.matrix[i,1] + 0.45

    ybottom <- design.matrix[i,2] - 0.45
    ytop <- design.matrix[i,2] + 0.45

    rect(xleft, ybottom, xright, ytop, col = design.matrix$icon_colors[i])
  }
})



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# comparison to pch:
system.time({
  plot(design.matrix$Var1, design.matrix$Var2,
       col = design.matrix$icon_colors,
       pch = 5,
       cex = 22,
       xlim = c(0.5, ncols + 0.5), ylim = c(0.5, nrows + 0.5),
       xlab = "",
       ylab = "")
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test functionality of plotting icons:

plot(1, 1, type = "n", xlim = c(0.5, 10 + 0.5), ylim = c(0.5, 10 + 0.5))

points(1, 1, pch = 15)  # points are independent of the size of the plotting device!

design.matrix <- expand.grid(1:ncols, 10:(1 + ncols - nrows))  # create a matrix for positions.
design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.

## -----------------------------------------------
## (+) ToDo:

## - should icons be apt to alteration (e.g., circles)?
## - how to proceed for larger populations (i.e., > 100)?
  ## * create blocks of 10x10 via mfrow, with each block being an own plot?
  ## * create blocks of 10x10 and add spacing arguments (i.e., an additional quater column)
## - Add legend, title and other descriptive information.
## - Test which version (rect vs. pch) is quicker.




## -----------------------------------------------
## eof.
