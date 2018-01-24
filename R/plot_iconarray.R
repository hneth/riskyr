## plot_iconarray.R | riskyR
## 2018 01 18
## -----------------------------------------------

##  This function plots an iconarray flexibly, dependent on population size

## Notes:
  ## if the number of colums diverges largely from the number of rows, icons become distorted.
  ## maybe drawing rectangles is inefficient.

## -----------------------------------------------

# dev.new(width = 5, height = 15)  # create device with known aspect ratio.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Preparation:------------------------------------
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Note: The function finally only needs:
  # A vector of identities (colors.)
    # This can be obtained in different ways (e.g., calculation by respective function)
  # A vector of positions (to be generated according to version.)
  # A number of blocks.
  # A size for the icons (cex)
  # ...?

# old parameters:

# ncols <- 10  # define number of columns (to be filled by row)
# nrows_fix <- 10  # fix number of rows (for each block) to some value.
#
# nrows <- ceiling(pop / ncols)  # calculate the required number of rows (for one block).

# source("./R/init_txt.R")
# source("./R/init_pal.R")
# source("./R/init_num.R")
# source("./R/comp_freq.R")
# source("./R/comp_popu.R")

# popu
#
# sdt.colors
#
# N <- freq$N
#
# freq <- comp_freq(N = pop)
# freq


## (c) SDT (status decision/truth):
# calculate to be independent from source
pop <- 100 # define population size.
sens <- 0.80
spec <- 0.90
prev <- 0.70

n.hi <- pop * prev * sens
n.mi <- 14 # pop * prev * (1 - sens)
n.fa <- 3 # pop * (1 - prev) * (1 - spec)
n.cr <- pop * (1 - prev) * spec

length(rep("hi", n.hi))
length(rep("mi", 14))  # too short???  Apparently a failure in updating...
length(rep("fa", 3))  # too short???
length(rep("cr", n.cr))

sdt <- c(rep("hi", n.hi), rep("mi", n.mi),
  rep("fa", n.fa), rep("cr", n.cr))

length(sdt)

sum(n.hi, n.mi, n.fa, n.cr)

# icon_colors <- c(rep(sdt.colors["hi"], n.hi), rep(sdt.colors["mi"], n.mi),
#                 rep(sdt.colors["fa"], n.fa), rep(sdt.colors["cr"], n.cr))

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

  # 2. Initialize vector of identities / class membership:
  ident_vec <- NULL
  ident_vec <- sdt

  # option 1 to obtain colors:
  sdt.colors <- c("green", "red", "orange", "blue")
  # final function takes any four colors mapped onto four identities.
  icon_colors <- c(rep(sdt.colors[1], n.hi), rep(sdt.colors[2], n.mi),
                   rep(sdt.colors[3], n.fa), rep(sdt.colors[4], n.cr))

  icon_colors <- adjustcolor(icon_colors, alpha.f = .66) # make transparent

  # option 2 to obtain colors:
  color_types <- sdt.colors
  ident_types <- unique(ident_vec)  # get number of unique types.
  icon_colors2 <- ident_vec  # initialize colors as identities.

  for (i in ident_types) {
    # replace the identities with their respective colors:
    icon_colors2[ident_vec == i] <- color_types[ident_types == i]
  }

  # sum(icon_colors != icon_colors2)  # test for all equal.

  col_vec <- icon_colors2

# (D) Plotting dependent on this information:
  plotx_dim <- c(0, 1)
  ploty_dim <- c(0, 1)  # assuming square plot x = y.

  # TODO: Each of the types is a potential function.  Then it is more modular!

# A1 Random position, random colors:---------------------------------------
  #
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
      plot(x = 1, xlim = plotx_dim, ylim = ploty_dim, type = "n")

      # 3a) set plotting character:
      pch <- 21  # filled square as default.
      cex <- 2

      points(x = posx_vec, posy_vec, # positions.
             # visual details:
             pch = pch, col = grey(.10, .66), bg = col_vec, cex = cex, bty = "o")

# A2 Random position, clustered colors: ---------------------------------------
  #
    # 1) Define positions:
    # 1a) draw random positions:
      posx_vec <- runif(n = pop, min = plotx_dim[1], max = plotx_dim[2])
      posy_vec <- runif(n = pop, min = ploty_dim[1], max = ploty_dim[2])

      # 1b) sort dependent on parameter:
      type_sort <- "mosaic"  # options:
      # right: from left to right, top: from top to bottom,
      # equal: in equal spaces of the plot, mosaic: relative to area.

      if (!type_sort %in% c("right", "top", "equal", "mosaic")) {
        warning('type_sort has to be either "right", "top", "equal", or "mosaic"')
        # maybe add stop and error message?
      } else {

        # type: from left to right:
        if (type_sort == "right") {
          posx_vec <- sort(posx_vec)
        }

        # type: from top to bottom:
        if(type_sort == "top"){
          posy_vec <- sort(posy_vec)
        }

      # equal compartments:
      if (type_sort == "equal") {  # density varies, area is constant.

        # Distance parameteror distance between blocks:
        block_d <- 0.01

        # create n = "ident_type" compartments of the plot:
        comp_n <- length(ident_types)  # number of compartments for x and y.
        # TODO: not final; they should be distributed.

        # determine breakpoints:

        # !!!Currently for square numbers only:
        # TODO: include non-square points (e.g., by enlarging the plot area).
        comp_sq <- sqrt(comp_n)  # take square root.

        # create list of breakpoints including ident_types:
        seq_min <- (0:(comp_sq-1)) / comp_sq  # of minimal coordinates.
        seq_max <- (1:comp_sq) / comp_sq

        min_ranges <- expand.grid(x_min = seq_min, y_min = seq_min)  # all combinations of minima.
        max_ranges <- expand.grid(x_max = seq_max, y_max = seq_max)  # all combinations of maxima.

        # add distance between icon blocks:
        global_min <- min(min_ranges)  # get global minimum of minima.
        global_max <- max(max_ranges)  # get global maximum of maxima.
        min_ranges[min_ranges != global_min] <- min_ranges[min_ranges != global_min] + block_d
        # we don't want distance at the global minima nad maxima.
        max_ranges[max_ranges != global_max] <- max_ranges[max_ranges != global_max] - block_d

        # TODO: flipping by swapping x and y or by changing vector of frequencies?
        # ToDo: Bind ranges into one object?
        # TODO: notice the overlap!  Use cut?

        # reset position vectors:
        posx_vec <- NULL
        posy_vec <- NULL

        # calculate number of observations in each compartment retaining original order:
        type_n <- sapply(unique(ident_vec), function(x) sum(ident_vec == x))

       # sample the coordinates from the deterimined ranges:
       for(i in 1:nrow(min_ranges)){

          minx <- min_ranges$x_min[i]
          maxx <- max_ranges$x_max[i]
          miny <- min_ranges$y_min[i]
          maxy <- max_ranges$y_max[i]
          # TODO: This only holds for equal compartments.

          # sample vectors from compartments:
          posx_vec_i <- runif(n = type_n[i], min = minx, max = maxx)
          posy_vec_i <- runif(n = type_n[i], min = miny, max = maxy)

          posx_vec <- c(posx_vec, posx_vec_i)
          posy_vec <- c(posy_vec, posy_vec_i)
        }

      }

      # mosaic style:
      if (type_sort == "mosaic") {

        # here we need to define the compartments flexibly (holding density constant):
        # create "ident_type" compartments of the plot:
        comp_n <- length(ident_types)  # number of compartments for x and y.

        # calculate number of observations in each compartment retaining original order:
        type_n <- sapply(unique(ident_vec), function(x) sum(ident_vec == x))

        comp_p <- type_n / sum(type_n)  # proportion in each compartment.

        prev <- comp_p["hi"] + comp_p["mi"]  # prevalence (or number of true conditions).
        # TODO: Allow split in both directions!
        # TODO: Rename comp_p.

        # define boundaries:
        b1 <- comp_p ["hi"] / (comp_p ["hi"] + comp_p["mi"])
        b2 <- comp_p ["cr"] / (comp_p ["cr"] + comp_p["fa"])


        # Quadrant dimensions (with prevalence in y-direction):
        comp1 <- c(0, b1, 0, prev)
        comp2 <- c(b1, 1, 0, prev)
        comp3 <- c(b2, 1, prev, 1)
        comp4 <- c(0, b2, prev, 1)

        # TODO: Allow to shuffle components around (using a list?).
        # TODO: not general yet!  How to make it general?  Calculation of area proportions?

        # reset vectors:
        posx_vec <- NULL
        posy_vec <- NULL

        # set distance parameter:
        block_d <- 0.02  # this parameter may not be half the size of the distance between min and max.
        # for the example of prevalence == 0.15 it may not exceed 0.075.

        # diff(comps)

        # bind vectors together.
        comps <- rbind(comp1, comp2, comp3, comp4)
        comps[, c(1, 3)] <- comps[, c(1, 3)] + block_d
        comps[, c(2, 4)] <- comps[, c(2, 4)] - block_d
        comp_n <- sapply(unique(ident_vec),function(x)sum(ident_vec==x))
        # calculate number of observations in each compartment.
        comps <- cbind(comps, comp_n)  # bind to martix.


        for(i in 1:nrow(comps)){
          minx <- comps[i, 1]
          maxx <- comps[i, 2]
          miny <- comps[i, 3]
          maxy <- comps[i, 4]
          # TODO: This only holds for equal compartments.

          # sample vectors from compartments:
          posx_vec_i <- runif(n = comps[i, 5], min = minx, max = maxx)
          posy_vec_i <- runif(n = comps[i, 5], min = miny, max = maxy)

          posx_vec <- c(posx_vec, posx_vec_i)
          posy_vec <- c(posy_vec, posy_vec_i)

      }
      }
    }

# A4 Fixed position clustered colors:---------------------------------------
  #

  # 0. define nrows, ncols, and blocks (with option to calculate them later as well as blocks).
  nrows <- 10
  ncols <- 10
  blocks <- 4
  col_blocks <- 3
  row_blocks <- 2
  block_size <- 5  # maybe generalize in block cols and rows.
  block_d <- 0.4  # same name as above?

  # 1. Create matrix of positions:
    design.matrix <- expand.grid(0:(ncols  - 1), (nrows - 1):0)  # create a matrix for positions.
    design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.
    # TODO: Can likely be ommitted in favor of the below solution.

    # Get block breakpoints:
    block_breaks_col <- seq(block_size, by = block_size, length.out = col_blocks - 1)
    block_breaks_row <- seq(block_size, by = block_size, length.out = row_blocks - 1)

  # 2. Define block membership to determine order:
    # 2.1. Row-wise filling:

    # For two x two blocks:
      # y >= block_size: block 1 or 2 (thinking in rows)
      block_1_2 <- design.matrix$Var2 >= block_size
      # x >= block size: block 2 or 4
      block_2_4 <- design.matrix$Var1 >= block_size

      block_vec <- ifelse(block_1_2 & !block_2_4, 1,
             ifelse(block_1_2 & block_2_4, 2,
                    ifelse(!block_1_2 & !block_2_4, 3, 4))
             )

    # For x times y blocks:
      blocks <- expand.grid(1:col_blocks, 1:row_blocks)
      blocks <- cbind(blocks, block_num = 1:nrow(blocks))

      blocks <- expand.grid(block_breaks_col, block_breaks_row)

    # use workaround for sorting for the moment (as order and sort do not work as expected):
    order_vec <- 1:length(block_vec)
    block_order <- rep(NA, length(block_vec))
    for (i in unique(block_vec)) {
      i_log <- block_vec == i  # get logical vector of all 1st block entries.
      len <- sum(i_log)  # get the respective length.
      block_order[i_log] <- order_vec[1:len]  # take the first len elements from the order vec.
      order_vec <- order_vec[(len + 1):length(order_vec)]  # remove the used elements.
    }

    # sort colors accordingly:
    col_vec <- col_vec[block_order]

  # 3. Define positions:
    minx <- min(design.matrix$Var1)
    maxx <- max(design.matrix$Var1)
    miny <- min(design.matrix$Var2)
    maxy <- max(design.matrix$Var2)

    # find adjustment parameter for distances:
    max_posx <- max(design.matrix$Var1) - block_d
      # now find a monotonically increasing sequence, resulting in exactly this endpoint.
    adj_posx <- seq(0, max_posx, length.out = ncols)
    design.matrix$Var1 <- adj_posx

    max_posy <- max(design.matrix$Var2) - block_d
    # now find a monotonically increasing sequence, resulting in exactly this endpoint.
    adj_posy <- seq(max_posy, 0, length.out = nrows)
    design.matrix$Var2 <- rep(adj_posy , each = nrows)

    # TODO: To make it more general one will likely have to include a factor on block_d following n.blocks - 1.

    # # adjustment sequences:
    # adjx <- rep(
    #   seq(1, max(design.matrix$Var1)), length.out = length(design.matrix$Var1[!design.matrix$Var1 %in% c(minx)])) *
    #   block_d
    #
    # # adjy <- rep(
    # #   seq(1, max(design.matrix$Var2)), each = block_size,
    # #   length.out = length(design.matrix$Var2[!design.matrix$Var2 %in% c(miny)])) *
    # #   block_d
    #
    # design.matrix$Var1[!design.matrix$Var1 %in% c(minx)] <-
    #   design.matrix$Var1[!design.matrix$Var1 %in% c(minx)] - adjx
    # design.matrix$Var2[!design.matrix$Var2 %in% c(maxy)] <-
    #   design.matrix$Var2[!design.matrix$Var2 %in% c(maxy)] + adjy

    block_size_adj <- block_size - block_d  # block size needs to be adjusted for comparison.

    # calculate block borders:
    block_size_adjx <- adj_posx[block_size + 1]
    block_size_adjy <- adj_posy[block_size + 1]

    # design.matrix <- design.matrix / 1.2
    design.matrix$Var1[design.matrix$Var1 >= (block_size_adjx)] <-
      design.matrix$Var1[design.matrix$Var1 >= (block_size_adjx)] + block_d

    design.matrix$Var2[design.matrix$Var2 > (block_size_adjy)] <-
      design.matrix$Var2[design.matrix$Var2 > (block_size_adjy)] + block_d


    # save into respective vectors and norm on 0,1 space.
    posx_vec <- design.matrix$Var1 / (ncols - 1)
    posy_vec <- design.matrix$Var2 / (nrows - 1)

    # TODO: Not in region anymore --> change plot dimensions or decrease standard distance.

    # Plotting dimensions for testing:
    plotx_dim <- c(-0.5, 1.5)
    ploty_dim <- c(-0.5, 1.5)

# ---------------------------------------------------------------------------
  # B Actual plotting:
    # checking for duplicates:
    (pos_duplicates <- sum(duplicated(round(cbind(posx_vec, posy_vec), 4))))
    # no duplicated coordinates.

    # 2) Randomize vector:
    # col_vec <- icon_colors

    # 3) Plot:
    plot(x = 1, xlim = plotx_dim, ylim = ploty_dim, type = "n", xlab = "",
         ylab = "")

    # 3a) set plotting character:
    pch <- 22  # filled square as default.
    cex <- 3

    test <- paste(round(posx_vec, 1), round(posy_vec, 1))

    points(x = posx_vec, y = posy_vec, # positions.
           # visual details:
           pch = pch, col = grey(0, .66), bg = col_vec, cex = cex, bty = "o")

    # optional: add ablines.
    abline(v = c(0, 1), h = c(0, 1))



# ---------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test functionality of plotting icons:
    par("pin")  # this is likely the appropriate value to scale cex...

    plot(1, 1, type = "n", xlim = c(0.5, 2.5), ylim = c(0.5, 2.5))

    cex <- max(par("pin")) / length(c(1, 2, 1, 2))

    points(c(1, 2, 1, 2), c(1, 1, 2, 2), pch = 15, cex = cex)  # points are independent of the size of the plotting device!

    design.matrix <- expand.grid(1:ncols, 10:(1 + ncols - nrows))  # create a matrix for positions.
    design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.


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

# lines(par("usr")[1], par("usr")[2], col = "red")

# lines(x = c(1, 5), y = c(5, 5), col = "red")

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
       pch = 22,
       cex = 6,
       col = grey(.33, .66), # sample(design.matrix$icon_colors),
       bg = design.matrix$icon_colors, # sample(design.matrix$icon_colors),
       lwd = 5,
       xlim = c(0.5, ncols + 0.5), ylim = c(0.5, nrows + 0.5),
       xlab = "",
       ylab = "")
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -----------------------------------------------
## (+) ToDo:
## - Understand cex --> how does it work, when does it change size, when not?
## - should icons be apt to alteration (e.g., circles)?
## - how to proceed for larger populations (i.e., > 100)?
  ## * create blocks of 10x10 via mfrow, with each block being an own plot?
  ## * create blocks of 10x10 and add spacing arguments (i.e., an additional quater column)
## - Add legend, title and other descriptive information.
## - Test which version (rect vs. pch) is quicker.


# dev.new(width = 5, height = 5)
# plot(4, 4, type = "n", xlim = c(0, 4), ylim = c(0, 4))
# rect(1, 1, 2, 2, asp = 1)
# points(3, 3, pch = 15)

# dev.new(width = 5, height = 5)
# plot(4, 4, type = "n", xlim = c(0, 4), ylim = c(0, 4))
# rect(1, 1, 2, 2, asp = 1)
# points(3, 3, pch = 15)

## -----------------------------------------------
## eof.
