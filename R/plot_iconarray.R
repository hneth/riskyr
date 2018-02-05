## plot_iconarray.R | riskyR
## 2018 01 31
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


## Helper functions: ------------------------------------------------------
## (c) SDT (status decision/truth):
# calculate in small helper function to be independent from source:

get_pop_vec <- function (N = 10000, # define population size.
                        sens = 0.80,
                        spec = 0.90,
                        prev = 0.30) {

  n.hi <- round(N * prev * sens)
  n.mi <- round(N * prev * (1 - sens))
  n.fa <- round(N * (1 - prev) * (1 - spec))
  n.cr <- round(N * (1 - prev) * spec)

  length(rep("hi", n.hi))
  length(rep("mi", 14))  # too short???  Apparently a failure in updating...
  length(rep("fa", 3))  # too short???
  length(rep("cr", n.cr))

  sdt <- c(rep("hi", n.hi), rep("mi", n.mi),
           rep("fa", n.fa), rep("cr", n.cr))

  output <- list(sdt = sdt,
                 freq = list(
                   n.hi = n.hi, n.mi = n.mi, n.fa = n.fa, n.cr = n.cr
                 ))

  return(output)

}

# test default:
freqs <- get_pop_vec()


# 2. Initialize vector of identities / class membership:
ident_vec <- get_pop_vec()$sdt

# option 1 to obtain colors:
sdt.colors <- c("green", "red", "orange", "blue")

# Option 3:
icon_colors <- sdt.colors

# numerosities (e.g., n.hi, n.mi etc.):
#freqs <- get_pop_vec(N = 90)

numerosities <- get_pop_vec(N = 100)$freq

# create a matrix from it.
ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat vector.

col_vec <- rep(icon_colors, times = numerosities)  # why detour via numeric vector?
# this presupposes that the number of icon colors and the number of numerosities are equal.



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

  # for identity see color / identities above:
    icon_colors <- sdt.colors  # define a set of colors.

    # numerosities (e.g., n.hi, n.mi etc.):
    numerosities <- freqs$freq  # assign a set of n frequencies.

    # create a repeated vector from it.
    ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
    col_vec <- icon_colors[ind_col_num]

  # Do the same for plotting characters:
    plot_chars <- c(22, 22, 23, 23)  # squares and diamonds.
    char_vec <- plot_chars[ind_col_num]

# (D) Plotting dependent on this information:
  plotx_dim <- c(0, 1)
  ploty_dim <- c(0, 1)  # assuming square plot x = y.



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Begin of function!--------------------------
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# TODO:
  # - add legend and text (needs sufficient margins at some place.)
  # - have color vector defined outside the plotting function
    # (if nothing is specified use this as a default).
  # - remove parameters unnecessary for arrangement (which are needed?).
  # - How should the default array look like?
  # - Make plotting icons customizable

  # - Each of the plot types may be a potential function.  Then it is more modular!

# for default:
  num <- riskyr:::num
  freq <- riskyr:::freq
  pal <- riskyr:::pal


plot_iconarray <- function (
                            prev = num$prev,             # probabilities
                            sens = num$sens, mirt = NA,
                            spec = num$spec, fart = NA,  # was: num$fart,
                            N = freq$N,    # ONLY freq used (so far)
                            ident.order = c("hi", "mi", "fa", "cr"),
                            random.position = FALSE,  # are positions randomly drawn?
                            random.identities = FALSE,  # are identities randomly assigned to positions?
                            # defaults to classic icon array!
                            # TODO: rather name these?
                            col.vec = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"]),  # use one color for each usual type.
                            pch.vec = 22,  # plotting characters; default square with border.
                            pch.border = grey(.66, 0.70),  # border of characters.
                            transparency = 2/3,
                            # one can also enter a full vector of length N.
                            block.d = 0.01,  # distance between blocks (where applicable).

                            type.sort = NULL,  # needs to be given if random position but nonrandom ident.

                            # for classic icon arrays only:
                            # TODO: Allow to calculate defaults in the function!
                            ncols = NULL,
                            nrows = NULL,
                            blocks = 1,
                            col_blocks = 1,
                            row_blocks = 1,
                            block_size_col = NULL,
                            block_size_row = NULL,
                            # TODO: Do I need them all the information is pretty redundant?

                            fill_array = "left",
                            fill_blocks = "rowwise",

                            # (currently) fixed parameters:
                            xlim = c(0, 1),
                            ylim = c(0, 1),  # xlim and ylim should currently remain fixed!
                            cex = NULL,  # if NULL, cex will be calculated on demand!

                            ...  #additional parameters for plot()

                            ) {

  # TODO: Checking of parameters!
  ## A0.1: Check entered parameters for plausibility!--------------------------------------------

    # Check whether random.position and random.identities are logical:
    if ( !(is.logical(random.position) | is.logical(random.identities)) ) {
      stop("random.position and random.identities must be logical!")
    }

  ## A0.2: Check entered parameters for usabililty:------------------------------------------

    # TODO: Either check for missing N or use other comparison.

    # Check, whether the color vector is not of size N:
    if (length(col.vec) != N) {
      if (length(col.vec) > 1) {  # only if more than one color.

      # Check, whether the color vector contains one color per type...
      # But what are types?
      # TODO!!!!!

      # get the frequencies if the color vector does not depict the population:
      if (!is.null(names(col.vec))) {
        if (!any(!names(col.vec) %in% ident.order)) {  # only if all are in (not any not in).
          col.vec <- col.vec[ident.order]
          }
      }

      if (length(freq[ident.order]) == length(col.vec)) {  # check whether both are equally long.

        col.vec <- rep(1:length(col.vec), times = freq[ident.order])
        # repeat color vector to population size.
      }

      }
    }
  # repeat for character vector:
  if (length(pch.vec) != N) {
      if (length(pch.vec) > 1) {
        if (length(pch.vec) == length(table(col.vec))) {  # check whether both are equally long.

          pch.vec <- rep(pch.vec, times = table(col.vec))
          # repeat color vector to population size.
          # TODO: Dependency on freq is a problem!
        } else {

          if (length(pch.vec) == length(table(col.vec)) / 2) {

            warning("Only half of necessary elements specified in pch.vec.
                    The elements are repeatedly used.")
            pch.vec <- pch.vec[c(1,1,2,2)]
            pch.vec <- rep(pch.vec, times = table(col.vec))

          } else {
            warning("pch.vec has not one element for each color-identity.  Only the first element is used.")
            pch.vec <- pch.vec[1]
          }


        }
      }

    }

  # TODO: I need to ensure that:
  # col.vec (must be given as vector of length N at this point!)

  # N (for A1 and A2)

    if (random.position) {
      if (is.null(N)) {
        N <- length(col.vec)
      }
    }

    # xlim/ylim

  # are given

  # End checking.

  ## A1 Random position, random colors:---------------------------------------
  if (random.position & random.identities) {

    # 1) Define positions:
      # 1a) draw random positions within plot dimensions:
      posx_vec <- runif(n = N, min = xlim[1], max = xlim[2])
      posy_vec <- runif(n = N, min = ylim[1], max = ylim[2])

    # 2) Randomize vectors:
    rand_ix <- sample(1:length(col.vec), replace = FALSE)  # create random vector.
    col.vec <- col.vec[rand_ix]  # randomize colors and

    if (length(pch.vec) == length(posx_vec)) {
      pch.vec <- pch.vec[rand_ix]  # characters accordingy.
    } else {
      pch.vec <- pch.vec[1]
      warning("pch.vec was not of length N.  Only first element used. ")
    }


  }  # end A1: random position & random colors.


  ## A2 Random position, clustered colors: ---------------------------------------
  if (random.position & !random.identities) {

    # 1b) sort dependent on parameter:
    # options:
    # right: from left to right, top: from top to bottom,
    # equal: in equal spaces of the plot, mosaic: relative to area.

    if (!type.sort %in% c("right", "top", "equal", "mosaic")) {
      stop('type_sort must be either "right", "top", "equal", or "mosaic"')
      # maybe add stop and error message?
    } else {

      if (type.sort %in% c("right", "top")) {

        # 1a) draw random positions:
        posx_vec <- runif(n = N, min = xlim[1], max = xlim[2])
        posy_vec <- runif(n = N, min = ylim[1], max = ylim[2])

        # Then sort one of the vectors accordingly (presupposes ordered color vector).
        # type: from left to right:
        if (type.sort == "right") {
          posx_vec <- sort(posx_vec)
        }

        # type: from top to bottom:
        if(type.sort == "top"){
          posy_vec <- sort(posy_vec)
        }
      } else {  # if in equal or mosaic:

          # Initialize positions:
          posx_vec <- NULL
          posy_vec <- NULL

          # create n = "ident_type" compartments of the plot:
          block_n <- length(unique(col.vec))  # number of blocks for x and y.
          # TODO: not final; they should be distributed.

          # calculate number of observations in each block retaining original order:
          type_n <- sapply(unique(col.vec), function(x) sum(col.vec == x))

        # equal compartments:
        if (type.sort == "equal") {  # density varies, area is constant.

          # determine breakpoints:
          # !!!Currently for square numbers only:
          # TODO: include non-square points (e.g., by enlarging the plot area).
          block_sq <- sqrt(block_n)  # take square root.

          # create list of breakpoints including color types:
          seq_min <- (0:(block_sq - 1)) / block_sq  # of minimal coordinates.
          seq_max <- (1:block_sq) / block_sq

          min_ranges <- expand.grid(x_min = seq_min, y_min = seq_min)  # all combinations of minima.
          max_ranges <- expand.grid(x_max = seq_max, y_max = seq_max)  # all combinations of maxima.

          # add distance between icon blocks:
          global_min <- min(min_ranges)  # get global minimum of minima.
          global_max <- max(max_ranges)  # get global maximum of maxima.
          min_ranges[min_ranges != global_min] <- min_ranges[min_ranges != global_min] + block.d
          # we don't want distance at the global minima nad maxima.
          max_ranges[max_ranges != global_max] <- max_ranges[max_ranges != global_max] - block.d

          # TODO: flipping by swapping x and y or by changing vector of frequencies?
          # TODO: Bind ranges into one object?
          # TODO: notice the overlap!  Use cut?



          # sample the coordinates from the deterimined ranges:
          for(i in 1:nrow(min_ranges)){  # TODO: avoid for-loop!

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
        if (type.sort == "mosaic") {

          block_prop <- type_n / sum(type_n)  # proportion in each compartment.

          prev <- block_prop[1] + block_prop[2]
          # define boundaries:
          b1 <- block_prop[1] / (block_prop[1] + block_prop[2])
          b2 <- block_prop [4] / (block_prop[4] + block_prop[3])
          # TODO: This depends on our typical order!  Might be made more transparent and customizable.


          # Quadrant dimensions (with prevalence in y-direction):
          block1 <- c(0, b1, 0, prev)
          block2 <- c(b1, 1, 0, prev)
          block3 <- c(b2, 1, prev, 1)
          block4 <- c(0, b2, prev, 1)

          # TODO: Allow to shuffle components around (using a list?).
          # TODO: not general yet!  How to make it general?  Calculation of area proportions?

          # bind vectors together.
          blocks <- rbind(block1, block2, block3, block4)

          # set distance parameter:
          # block.d may not be half the size of the distance between min and max.
          # for the example of prevalence == 0.15 it may not exceed 0.075.
          diff_dx <- apply(X = blocks[, c(1, 2)], MAR = 1, FUN = diff)
          diff_dy <- apply(X = blocks[, c(3, 4)], MAR = 1, FUN = diff)

          boundary_d <- min(c(abs(diff_dx), abs(diff_dy))) / 2

          if (block.d >= boundary_d) {
            block.d <- boundary_d - 0.0001  # a little messy though...
          }

          blocks[, c(1, 3)] <- blocks[, c(1, 3)] + block.d
          blocks[, c(2, 4)] <- blocks[, c(2, 4)] - block.d
          block_n <- sapply(unique(col.vec), function(x) sum(col.vec == x))
          # calculate number of observations in each compartment.
          blocks <- cbind(blocks, block_n)  # bind to matrix.


          for(i in 1:nrow(blocks)){
            minx <- blocks[i, 1]
            maxx <- blocks[i, 2]
            miny <- blocks[i, 3]
            maxy <- blocks[i, 4]
            # TODO: This only holds for equal blocks.

            # sample vectors from blocks:
            posx_vec_i <- runif(n = blocks[i, 5], min = minx, max = maxx)
            posy_vec_i <- runif(n = blocks[i, 5], min = miny, max = maxy)

            posx_vec <- c(posx_vec, posx_vec_i)
            posy_vec <- c(posy_vec, posy_vec_i)

          }
        }
      }

    }  # end: valid type.sort
  }  # End A2!

  if (random.position) {

    if (is.null(cex)) {
      # TODO: How to covary cex with device size & point number?


      cex1 <- ((par("pin")[1] * 10) + 3) / sqrt(length(posx_vec))  # ad hoc formula.
      cex2 <- ((par("pin")[2] * 10) + 3) / sqrt(length(posx_vec))  # ad hoc formula.
      cex <- min(c(cex1, cex2))

    }
  }


    # A3 and A4: Fixed positions:  --------------------------------------
    if (!random.position) {

      # 1. Define positions:-----------------------------------------------

      # TODO: Allow for dynamic calculation of blocks and sizes!
      # some python code:
        # def factors_min_diff(n):
        #   lower = math.floor(math.sqrt(n))
        #   upper = math.ceil(math.sqrt(n))
        #
        #   while lower * upper != n:
        #     if lower * upper > n:
        #     lower -= 1
        #   if lower * upper < n:
        #     upper += 1

      # 1. Create matrix of positions:

      # find maximum for the positions given the units icons are moved:
      # find a monotonically increasing sequence, resulting in exactly the endpoint of xlim/ylim.
      # For x:
      max_posx <- ((ncols - 1) * xlim[2]) - (block.d * (col_blocks - 1))
      adj_posx <- seq(0, max_posx, length.out = ncols)

      # For y:
      max_posy <- ((nrows - 1) * ylim[2]) - (block.d * (row_blocks - 1))
      adj_posy <- seq(max_posy, 0, length.out = nrows)

      # create position matrices:
      pos_mx <- matrix(adj_posx, nrow = nrows, ncol = ncols, byrow = TRUE)
      pos_my <- matrix(adj_posy, nrow = nrows, ncol = ncols)

      # add  a sequence to the x matrix:
      # For x:
      seqx_off <- seq(0, (col_blocks - 1) * block.d, by = block.d)
      # get the sequence of offsets for icons in each block.
      seqx <- rep(seqx_off, each = block_size_col)
      # repeat this sequence by block size so every icon is affected.
      pos_mx <- pos_mx + rep(seqx, each = nrow(pos_mx))
      # do so for every row in the matrix.

      # For y:
      seqy_off <- seq((row_blocks - 1) * block.d, 0, by = -block.d)  # create sequence of number to add.
      seqy <- rep(seqy_off, each = block_size_row)  # repeat to number of rows.
      pos_my <- pos_my + seqy  # will be repeated for each column anyways.


      # Plotting preparations: ------------------------------------------------------

      # save into respective vectors and norm on 0,1 space.
      posx_vec <- pos_mx / (ncols - 1)
      posy_vec <- pos_my / (nrows - 1)

      # TODO: Not in region anymore --> change plot dimensions or decrease standard distance.

      # Plotting dimensions for testing:
      # plotx_dim <- c(-0.1, 1.1)
      # ploty_dim <- c(-0.1, 1.1)


      if (!random.identities) {  # sort colors according to input.
      # For A4 (fixed positions and clustered identities) only:
        # 2. Color sorting ------------------------------------------------------------
        # Create block information:
        seq_block1 <- 1:blocks  # create sequence of block positions.

        # Determine, whether blocks are used colwise or rowwise:
        #fill_array <- "left"  # alternatively: "rowwise"

        # If blocks are to be filled in x direction:
        if (fill_array == "left"){

          seq_blockx <- rep(seq_block1, each = block_size_col)
          # create sequence repeted to the number of cols (can be changed to number of rows).
          mat_block <- matrix(seq_blockx, ncol = ncols, byrow = TRUE)
          # create a matrix from it.
          ind_block <- rep(1:nrow(mat_block), each = block_size_row)  # create index to repeat matrix.
          mat_block <- mat_block[ind_block, ]
          # repeat each row of the matrix to the number of rows.
        }

        # If blocks are to be filled in y direction:
        if (fill_array == "top"){

          seq_blocky <- rep(seq_block1, each = block_size_row)
          # create sequence repeted to the number of cols (can be changed to number of rows).
          mat_block <- matrix(seq_blocky, nrow = nrows, byrow = FALSE)
          # create a matrix from it.
          ind_block <- rep(1:ncol(mat_block), each = block_size_col)  # create index to repeat matrix.
          mat_block <- mat_block[ , ind_block]
          # repeat each row of the matrix to the number of columns.
        }

        # Determine, whether blocks (within) are filled col- or rowwise:
        #fill_blocks <- "colwise"

        # sort colors accordingly:
          # TODO: Find out WHY ON EARTH order(order()) works!

        if (fill_blocks == "rowwise"){
          order_mat <- order(order(t(mat_block)))  # matrix has to be transposed to get the rows.
          m <- matrix(order_mat, nrow = nrows, ncol = ncols, byrow = TRUE)  # This is the "rowwise witin blocks" version.

          #if (fill_array == "top") { m <- t(m) }
        }

        if (fill_blocks == "colwise") {
          order_mat <- order(order(mat_block))
          m <- matrix(order_mat, nrow = nrows, ncol = ncols, byrow = FALSE)  # This is the "colwise within blocks" version.

          #if (fill_array == "top") { m <- t(m) }
        }


        # if the color vector already has the appropriate length:
        if (length(col.vec) == length(m)) {
          col.vec <- col.vec[m]  # order the color vector.
        }

        if (length(pch.vec) == length(m)) {
          pch.vec <- pch.vec[m]  # order the character vector.
        }


        # if the color vector is too short:
        # TODO: what to do if too long?
        if (length(col.vec) < length(m)) {

          len_diff <- length(pos_mx) - length(col.vec)  # calculate difference.
          col.vec <- c(col.vec, rep(NA, len_diff))  # enlarge the color vector.

          # mute the respective positions:
          col.vec <- col.vec[m]  # order color vector.
          posx_vec[is.na(col.vec)] <- NA  # set NA x positions...
          posy_vec[is.na(col.vec)] <- NA  # ... and y positions.

        }

        if (length(pch.vec) < length(m) & length(pch.vec) > 1) {

          len_diff <- length(pos_mx) - length(pch.vec)  # calculate difference.
          pch.vec <- c(pch.vec, rep(NA, len_diff))  # enlarge the color vector.

          # mute the respective positions:
          pch.vec <- pch.vec[m]  # order character vector.
          posx_vec[is.na(pch.vec)] <- NA  # set NA x positions...
          posy_vec[is.na(pch.vec)] <- NA  # ... and y positions.

        }

      }  # end fixed identities (A4).



      # For A3:-------------------------------------------------
      if (random.identities) {

        rand_ix <- sample(1:length(col.vec), replace = FALSE)  # random index.
        col.vec <- col.vec[rand_ix]  # sample from the vector of colors.

        if (length(pch.vec) > 1) {
          pch.vec <- pch.vec[rand_ix]  # analog sample from character vector.
        }


        # if the color vector is too short.
        if (length(col.vec) < (ncols * nrows) & length(col.vec) > 1) {

          # for colors:
          len_diff <- (ncols * nrows) - length(col.vec)
          if (length(pch.border) > 1) {
            pch.border <- c(pch.border, rep(NA, len_diff))
          }

          col.vec <- c(col.vec, rep(NA, len_diff))

        }

        # if the character vector is too short:
        if (length(pch.vec) < (ncols * nrows) & length(pch.vec) > 1) {

          # for colors:
          len_diff <- (ncols * nrows) - length(pch.vec)
          pch.vec <- c(pch.vec, rep(NA, len_diff))

        }


      }  # end random identities (A3).

      # Adjust cex dynamically:
      if (is.null(cex)) {
        # TODO: How to covary cex with device size & point number?

        cex1 <- ((par("pin")[1] * 10) + 3) / ncols
        cex2 <- ((par("pin")[2] * 10) + 3) / nrows
        cex <- min(c(cex1, cex2))

        # still not optimal...

        # cex <- 1.5 - (min(par("pin")) / N)  # this latter term likely plays some role...
      }


    }  # end A3 and A4 (fixed positions).


# B. Plotting --------------------------------------------------------

  # TODO: Add text!

    if (any(!pch.vec %in% c(NA, 21:25))) {
      # if any of the plotting characters is not in the ones with border,
      # omit border and color accordingly.
      pch.border <- col.vec
    }

    # 3) Plot:
    plot(x = 1, xlim = xlim, ylim = ylim, type = "n", xlab = "",
       ylab = "")

    # 3a) set plotting character:
    # pch <- 22  # filled square as default.
    # cex <- 0.5

    if (!is.null(transparency)) {
      col.vec <- adjustcolor(col.vec, alpha.f = transparency)
    }

    points(x = posx_vec, y = posy_vec, # positions.
           # visual details:
           pch = pch.vec, col = pch.border, bg = col.vec, cex = cex, bty = "o")

    # optional for testing: add ablines.
    # abline(v = c(0, 1), h = c(0, 1))

    # TODO: allow for a vector of plotting characters as well!

#---------------------------------------------
}  # end of function.


# Test default:
plot_iconarray(nrows = 10, ncols = 10, pch.vec = c(22,23,22,23),
               block_size_col = 10, block_size_row = 10)

plot_iconarray(nrows = 100, ncols = 100, pch.vec = c(22,23,22,23),
               block_size_col = 10, block_size_row = 10)

plot_iconarray(pch.vec = c(22,23,22,23), #cex = 3,
               random.position = TRUE, type.sort = "mosaic", block.d = 0.05)

plot_iconarray(pch.vec = c(22,23,21,23), #cex = 10,
               random.position = TRUE, type.sort = "equal", block.d = 0.05)

plot_iconarray(pch.vec = c(22,23,22,23), #cex = 10,
               random.position = TRUE, random.identities = TRUE)

# ncols and nrows must be calculated!

# Currently not used parts ---------------------------------------

  # checking for duplicates:
  pos_duplicates <- sum(duplicated(cbind(posx_vec, posy_vec)))

# Testing ground:-----------------------------------------------------

  # numerosities (e.g., n.hi, n.mi etc.):
  numerosities <- get_pop_vec(N = 10000)$freq  # assign a set of n frequencies.

  # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]
  pch_vec <- c(22,22,23,23)[ind_col_num]

  # random positions:
  # TODO: Why are they so slow now?
  plot_iconarray(N = 10000, col.vec = col_vec, pch.vec = pch_vec,
                 random.position = TRUE, random.identities = TRUE)
  # especially this guy is quite slow!

  plot_iconarray(N = 10000, col.vec = col_vec, cex = 1,
                 random.position = TRUE, random.identities = FALSE,
                 type.sort = "mosaic")

  # Example C:

  # non-random positions:
  plot_iconarray(N = 10000, col.vec = col_vec, pch.vec = c(22,23,21,23),
                 random.position = FALSE, random.identities = FALSE,
                 #cex = 0.5,
                 block.d = 0.5,
                 block_size_col = 10, block_size_row = 10,
                 blocks = 100,
                 col_blocks = 10, row_blocks = 10,
                 nrows = 100, ncols = 100,
                 fill_array = "left",
                 fill_blocks = "colwise")

  # even larger:
  numerosities <- get_pop_vec(N = 1000000)$freq  # assign a set of n frequencies.

  # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]
  # non-random positions:
  plot_iconarray(N = 1000000, col.vec = col_vec,
                 random.position = FALSE, random.identities = FALSE,
                 #cex = 0.5,
                 block.d = 0.5,
                 block_size_col = 100, block_size_row = 10,
                 blocks = 100,
                 col_blocks = 10, row_blocks = 10,
                 nrows = 1000, ncols = 1000,
                 fill_array = "left",
                 fill_blocks = "colwise")


  # Example A:
  # numerosities (e.g., n.hi, n.mi etc.):
  numerosities <- get_pop_vec(N = 90)$freq  # assign a set of n frequencies.

  # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]
  pch_vec <- c(22,22,23,23)[ind_col_num]

  plot_iconarray(N = 90, col.vec = col_vec, pch.vec = pch_vec,
                 random.position = FALSE, random.identities = FALSE,
                 #cex = 3,
                 block.d = 0.5,
                 block_size_col = 5, block_size_row = 2,
                 blocks = 9,
                 col_blocks = 3, row_blocks = 3,
                 nrows = 6, ncols = 15,
                 fill_array = "top",
                 fill_blocks = "rowwise")

  # New example:
  # numerosities (e.g., n.hi, n.mi etc.):
  numerosities <- get_pop_vec(N = 100)$freq  # assign a set of n frequencies.

  # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]

  plot_iconarray(N = 100, col.vec = col_vec,
                 random.position = FALSE, random.identities = FALSE,
                 # cex = 3,
                 block.d = 0.5,
                 block_size_col = 5, block_size_row = 5,
                 blocks = 4,
                 col_blocks = 2, row_blocks = 2,
                 nrows = 10, ncols = 10,
                 fill_array = "top",
                 fill_blocks = "rowwise")

  # TODO: Accomodate odd populations!
  numerosities <- get_pop_vec(N = 97)$freq  # assign a set of n frequencies.

  # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]
  pch_vec <- c(22,23,22,23)[ind_col_num]

  plot_iconarray(N = 100, col.vec = col_vec, pch.vec = pch_vec,
                 random.position = FALSE, random.identities = TRUE,
                 # cex = 3,
                 pch.border = "grey",
                 block.d = 0.5,
                 block_size_col = 5, block_size_row = 5,
                 blocks = 4,
                 col_blocks = 2, row_blocks = 2,
                 nrows = 10, ncols = 10,
                 fill_array = "left",
                 fill_blocks = "rowwise")


  numerosities <- get_pop_vec(N = 70)$freq  # assign a set of n frequencies.
  # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]
  pch_vec <- c(22,23,22,23)[ind_col_num]

  plot_iconarray(N = 70, col.vec = col_vec, pch.vec = pch_vec,
                 random.position = FALSE, random.identities = FALSE,
                 #cex = 3,
                 block.d = 0.5,
                 block_size_col = 5, block_size_row = 2,
                 blocks = 9,
                 pch.border = "black",
                 col_blocks = 3, row_blocks = 3,
                 nrows = 6, ncols = 15,
                 fill_array = "top",
                 fill_blocks = "rowwise")



  # testing examples...
  numerosities <- get_pop_vec(N = 70)$freq  # assign a set of n frequencies.

  # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]

  plot_iconarray(N = 90, col.vec = col_vec,
                 random.position = FALSE, random.identities = FALSE,
                 #cex = 3,
                 block.d = 0.5,
                 block_size_col = 7, block_size_row = 5,
                 blocks = 2,
                 col_blocks = 1, row_blocks = 2,
                 nrows = 10, ncols = 7,
                 fill_array = "top",
                 fill_blocks = "rowwise")


  numerosities <- get_pop_vec(N = 9)$freq  # assign a set of n frequencies.
    # create a repeated vector from it.
  ind_col_num <- rep(1:length(icon_colors), times = numerosities)  # create index to repeat matrix.
  col_vec <- icon_colors[ind_col_num]

  plot_iconarray(N = 9, col.vec = col_vec,
                 random.position = FALSE, random.identities = FALSE,
                 #cex = 3,
                 block.d = 0.5,
                 block_size_col = 1, block_size_row = 1,
                 blocks = 1,
                 col_blocks = 1, row_blocks = 1,
                 nrows = 3, ncols = 3,
                 # cex = 6,
                 fill_array = "top",
                 fill_blocks = "rowwise")

  # TODO: Make applicable for dumbest possible user...

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# END OF FUNCTION!!!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## -----------------------------------------------
## (+) ToDo:
## - Understand cex --> how does it work, when does it change size, when not?
## - should icons be apt to alteration (e.g., circles)?
## - Add legend, title and other descriptive information.

## -----------------------------------------------
## eof.
